use std::borrow::Borrow;
use std::ffi::{CStr, CString};
use std::iter::FromIterator;
use std::mem;
use std::slice;

use crate::ast::{Cond, Inst, MiscOp, Mode, Off, Op, Program, RVal, Size, Src};
use crate::errors::Result;
use crate::raw::*;

/// compile the BPF expression into a filter program.
pub fn compile<S: AsRef<str>>(link_type: u32, code: S) -> Result<Program> {
    let mut program: bpf_program = unsafe { mem::zeroed() };
    let code = CString::new(code.as_ref())?;

    unsafe {
        let p = pcap_open_dead(link_type as i32, i32::from(u16::max_value()));
        let ret = if pcap_compile(p, &mut program, code.as_ptr(), 1, PCAP_NETMASK_UNKNOWN) == 0 {
            (&program).into()
        } else {
            Err(failure::err_msg(CStr::from_ptr(pcap_geterr(p)).to_str()?))
        };
        pcap_close(p);
        pcap_freecode(&mut program);

        ret
    }
}

impl Program {
    pub fn validate(&self) -> bool {
        let insts: Vec<bpf_insn> = self.0.iter().cloned().map(|inst| inst.into()).collect();

        unsafe { bpf_validate(insts.as_ptr(), insts.len() as i32) != 0 }
    }
}

impl<'a> FromIterator<&'a bpf_insn> for Result<Program> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = &'a bpf_insn>,
    {
        iter.into_iter()
            .map(|inst| inst.into())
            .collect::<Result<Vec<_>>>()
            .map(Program)
    }
}

impl From<&bpf_program> for Result<Program> {
    fn from(program: &bpf_program) -> Self {
        let program = program.borrow();

        unsafe { slice::from_raw_parts(program.bf_insns, program.bf_len as usize) }
            .iter()
            .collect()
    }
}

impl Size {
    fn code(self) -> u32 {
        match self {
            Size::Reserved => 0,
            Size::Byte => BPF_B,
            Size::Half => BPF_H,
            Size::Word => BPF_W,
        }
    }
}

impl Src {
    fn code(self) -> u32 {
        match self {
            Src::K(_) => BPF_K,
            Src::X => BPF_X,
        }
    }

    fn value(self) -> u32 {
        match self {
            Src::K(v) => v,
            Src::X => 0,
        }
    }
}

impl RVal {
    fn code(&self) -> u32 {
        match self {
            RVal::A => BPF_A,
            RVal::K(_) => BPF_K,
        }
    }

    fn value(self) -> u32 {
        match self {
            RVal::K(v) => v,
            RVal::A => 0,
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
struct OpCode(u8);

impl OpCode {
    fn class(self) -> u32 {
        u32::from(self.0 & 0x07)
    }

    fn size(self) -> Size {
        match u32::from(self.0 & 0x18) {
            BPF_W => Size::Word,
            BPF_H => Size::Half,
            BPF_B => Size::Byte,
            _ => Size::Reserved,
        }
    }

    fn mode(self) -> u32 {
        u32::from(self.0 & 0xe0)
    }

    fn op(self) -> u32 {
        u32::from(self.0 & 0xf0)
    }

    fn src(self, k: Off) -> Src {
        match u32::from(self.0 & 0x08) {
            BPF_K => Src::K(k),
            BPF_X => Src::X,
            _ => unreachable!(),
        }
    }

    fn rval(self, k: Off) -> Option<RVal> {
        match u32::from(self.0 & 0x18) {
            BPF_K => Some(RVal::K(k)),
            BPF_A => Some(RVal::A),
            _ => None,
        }
    }

    fn misc_op(self) -> u32 {
        u32::from(self.0 & 0xf8)
    }
}

impl From<&bpf_insn> for Result<Inst> {
    fn from(inst: &bpf_insn) -> Self {
        let inst = inst.borrow();
        let code = OpCode(inst.code as u8);

        Ok(match code.class() {
            BPF_LD => Inst::Load(match code.mode() {
                BPF_IMM => Mode::Imm(inst.k),
                BPF_ABS => Mode::Abs(inst.k, code.size()),
                BPF_IND => Mode::Ind(inst.k, code.size()),
                BPF_MEM if inst.k < BPF_MEMWORDS => Mode::Mem(inst.k),
                BPF_LEN => Mode::Len,
                _ => bail!("invalid BPF_LD mode: 0x{:x}, k = {}", code.mode(), inst.k),
            }),
            BPF_LDX => Inst::LoadX(match code.mode() {
                BPF_IMM => Mode::Imm(inst.k),
                BPF_MEM if inst.k < BPF_MEMWORDS => Mode::Mem(inst.k),
                BPF_LEN => Mode::Len,
                BPF_MSH => Mode::Msh(inst.k),
                _ => bail!("invalid BPF_LDX mode: 0x{:x}, k = {}", code.mode(), inst.k),
            }),
            BPF_ST if inst.k < BPF_MEMWORDS => Inst::Store(inst.k),
            BPF_STX if inst.k < BPF_MEMWORDS => Inst::StoreX(inst.k),
            BPF_ALU => Inst::Alu(match code.op() {
                BPF_ADD => Op::Add(code.src(inst.k)),
                BPF_SUB => Op::Sub(code.src(inst.k)),
                BPF_MUL => Op::Mul(code.src(inst.k)),
                BPF_DIV if inst.k != 0 => Op::Div(code.src(inst.k)),
                BPF_OR => Op::Or(code.src(inst.k)),
                BPF_AND => Op::And(code.src(inst.k)),
                BPF_LSH => Op::LShift(code.src(inst.k)),
                BPF_RSH => Op::RShift(code.src(inst.k)),
                BPF_NEG => Op::Neg,
                BPF_MOD if inst.k != 0 => Op::Mod(code.src(inst.k)),
                BPF_XOR => Op::Xor(code.src(inst.k)),
                _ => bail!("invalid BPF_ALU op: 0x{:x}", code.op()),
            }),
            BPF_JMP => Inst::Jmp(match code.op() {
                BPF_JA => Cond::Abs(inst.k),
                BPF_JEQ => Cond::Eq(code.src(inst.k), inst.jt, inst.jf),
                BPF_JGT => Cond::Gt(code.src(inst.k), inst.jt, inst.jf),
                BPF_JGE => Cond::Ge(code.src(inst.k), inst.jt, inst.jf),
                BPF_JSET => Cond::Set(code.src(inst.k), inst.jt, inst.jf),
                _ => bail!("invalid BPF_JMP cond: 0x{:x}", code.op()),
            }),
            BPF_RET => Inst::Ret(code.rval(inst.k)),
            BPF_MISC if code.misc_op() == BPF_TAX => Inst::Misc(MiscOp::Tax),
            BPF_MISC if code.misc_op() == BPF_TXA => Inst::Misc(MiscOp::Txa),
            _ => bail!("invalid BPF inst: {:#?}", inst),
        })
    }
}

macro_rules! BPF_STMT {
    ($code:expr) => {
        bpf_insn {
            code: $code as u16,
            jt: 0,
            jf: 0,
            k: 0,
        }
    };
    ($code:expr, $k:expr) => {
        bpf_insn {
            code: $code as u16,
            jt: 0,
            jf: 0,
            k: $k,
        }
    };
}

macro_rules! BPF_JUMP {
    ($code:expr, $k:expr) => {
        bpf_insn {
            code: $code as u16,
            jt: 0,
            jf: 0,
            k: $k,
        }
    };
    ($code:expr, $k:expr, $jt:expr, $jf:expr) => {
        bpf_insn {
            code: $code as u16,
            jt: $jt,
            jf: $jf,
            k: $k,
        }
    };
}

impl From<Inst> for bpf_insn {
    fn from(inst: Inst) -> Self {
        match inst {
            Inst::Load(Mode::Abs(off, size)) => BPF_STMT!(BPF_LD | BPF_ABS | size.code(), off),
            Inst::Load(Mode::Ind(off, size)) => BPF_STMT!(BPF_LD | BPF_IND | size.code(), off),
            Inst::Load(Mode::Len) => BPF_STMT!(BPF_LD | BPF_LEN | BPF_W),
            Inst::Load(Mode::Imm(off)) => BPF_STMT!(BPF_LD | BPF_IMM, off),
            Inst::Load(Mode::Mem(off)) => BPF_STMT!(BPF_LD | BPF_MEM, off),
            Inst::Load(_) => unreachable!(),

            Inst::LoadX(Mode::Imm(off)) => BPF_STMT!(BPF_LD | BPF_IMM | BPF_W, off),
            Inst::LoadX(Mode::Mem(off)) => BPF_STMT!(BPF_LD | BPF_MEM | BPF_W, off),
            Inst::LoadX(Mode::Msh(off)) => BPF_STMT!(BPF_LD | BPF_MSH | BPF_B, off),
            Inst::LoadX(_) => unreachable!(),

            Inst::Store(off) => BPF_STMT!(BPF_ST, off),
            Inst::StoreX(off) => BPF_STMT!(BPF_STX, off),

            Inst::Jmp(Cond::Abs(off)) => BPF_JUMP!(BPF_JMP | BPF_JA, off),
            Inst::Jmp(Cond::Gt(src, jt, jf)) => BPF_JUMP!(BPF_JMP | BPF_JGT | src.code(), src.value(), jt, jf),
            Inst::Jmp(Cond::Ge(src, jt, jf)) => BPF_JUMP!(BPF_JMP | BPF_JGE | src.code(), src.value(), jt, jf),
            Inst::Jmp(Cond::Eq(src, jt, jf)) => BPF_JUMP!(BPF_JMP | BPF_JEQ | src.code(), src.value(), jt, jf),
            Inst::Jmp(Cond::Set(src, jt, jf)) => BPF_JUMP!(BPF_JMP | BPF_JSET | src.code(), src.value(), jt, jf),

            Inst::Alu(Op::Add(src)) => BPF_STMT!(BPF_ALU | BPF_ADD | src.code(), src.value()),
            Inst::Alu(Op::Sub(src)) => BPF_STMT!(BPF_ALU | BPF_SUB | src.code(), src.value()),
            Inst::Alu(Op::Mul(src)) => BPF_STMT!(BPF_ALU | BPF_MUL | src.code(), src.value()),
            Inst::Alu(Op::Div(src)) => BPF_STMT!(BPF_ALU | BPF_DIV | src.code(), src.value()),
            Inst::Alu(Op::Mod(src)) => BPF_STMT!(BPF_ALU | BPF_MOD | src.code(), src.value()),
            Inst::Alu(Op::And(src)) => BPF_STMT!(BPF_ALU | BPF_AND | src.code(), src.value()),
            Inst::Alu(Op::Or(src)) => BPF_STMT!(BPF_ALU | BPF_OR | src.code(), src.value()),
            Inst::Alu(Op::Xor(src)) => BPF_STMT!(BPF_ALU | BPF_XOR | src.code(), src.value()),
            Inst::Alu(Op::LShift(src)) => BPF_STMT!(BPF_ALU | BPF_LSH | src.code(), src.value()),
            Inst::Alu(Op::RShift(src)) => BPF_STMT!(BPF_ALU | BPF_RSH | src.code(), src.value()),
            Inst::Alu(Op::Neg) => BPF_STMT!(BPF_ALU | BPF_NEG),

            Inst::Ret(Some(rval)) => BPF_STMT!(BPF_RET | rval.code(), rval.value()),
            Inst::Ret(None) => BPF_STMT!(BPF_RET),

            Inst::Misc(MiscOp::Tax) => BPF_STMT!(BPF_MISC | BPF_TAX),
            Inst::Misc(MiscOp::Txa) => BPF_STMT!(BPF_MISC | BPF_TXA),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Inst::*, Mode::*, Size::*, *};

    #[test]
    fn program() {
        let p = compile(DLT_EN10MB, "ip and udp").unwrap();
        assert!(p.validate());

        let insts = vec![
            Load(Abs(12, Half)),
            Jmp(Cond::Eq(Src::K(0x800), 0, 3)),
            Load(Abs(23, Byte)),
            Jmp(Cond::Eq(Src::K(0x11), 0, 1)),
            Ret(Some(RVal::K(65535))),
            Ret(Some(RVal::K(0))),
        ];

        assert_eq!(&*p, insts.as_slice());
        assert_eq!(
            p.to_string(),
            r#"(000) ldh      [12]
(001) jeq      #0x800 jt 2 jf 5
(002) ldb      [23]
(003) jeq      #0x11 jt 4 jf 5
(004) ret      #65535
(005) ret      #0
"#
        );

        let bf_insns: Vec<bpf_insn> = insts.into_iter().map(|inst| inst.into()).collect();
        assert_eq!(unsafe { bpf_validate(bf_insns.as_ptr(), bf_insns.len() as i32) }, 1);
        assert_eq!(
            bf_insns
                .iter()
                .enumerate()
                .map(|(idx, inst)| unsafe {
                    CStr::from_ptr(bpf_image(inst, idx as i32))
                        .to_str()
                        .map(|s| s.to_owned())
                        .unwrap()
                })
                .collect::<Vec<_>>(),
            vec![
                "(000) ldh      [12]",
                "(001) jeq      #0x800           jt 2\tjf 5",
                "(002) ldb      [23]",
                "(003) jeq      #0x11            jt 4\tjf 5",
                "(004) ret      #65535",
                "(005) ret      #0"
            ]
        );

        let bf_prog = bpf_program {
            bf_len: bf_insns.len() as u32,
            bf_insns: bf_insns.as_ptr() as *mut _,
        };
        assert_eq!(Result::<Program>::from(&bf_prog).unwrap(), p);
    }
}
