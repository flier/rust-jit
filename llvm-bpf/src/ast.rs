use std::borrow::Borrow;
use std::ffi::{CStr, CString};
use std::fmt;
use std::mem;
use std::result::Result as StdResult;
use std::slice;

use crate::raw::*;

pub type Result<T> = StdResult<T, failure::Error>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Program(Vec<Inst>);

impl From<&bpf_program> for Result<Program> {
    fn from(program: &bpf_program) -> Self {
        let program = program.borrow();

        unsafe { slice::from_raw_parts(program.bf_insns, program.bf_len as usize) }
            .iter()
            .map(|inst| inst.into())
            .collect::<Result<Vec<_>>>()
            .map(Program)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, inst) in self.0.iter().enumerate() {
            writeln!(f, "({:03} {})", idx, inst)?;
        }

        Ok(())
    }
}

impl Program {
    pub fn compile<S: AsRef<str>>(&self, code: S) -> Result<Self> {
        let mut program: bpf_program = unsafe { mem::zeroed() };
        let code = CString::new(code.as_ref())?;

        unsafe {
            let p = pcap_open_dead(0, 0);
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

    pub fn validate(&self) -> bool {
        let insts: Vec<bpf_insn> = self.0.iter().cloned().map(|inst| inst.into()).collect();

        unsafe { bpf_validate(insts.as_ptr(), insts.len() as i32) != 0 }
    }
}

pub type Off = u32;
pub type K = u32;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Size {
    Reserved,
    Byte,
    Half,
    Word,
}

impl Size {
    pub fn suffix(self) -> &'static str {
        match self {
            Size::Byte => "b",
            Size::Half => "h",
            _ => "",
        }
    }

    pub fn bytes(self) -> usize {
        match self {
            Size::Reserved => 0,
            Size::Byte => 1,
            Size::Half => 2,
            Size::Word => 4,
        }
    }

    pub fn code(self) -> u32 {
        match self {
            Size::Reserved => 0,
            Size::Byte => BPF_B,
            Size::Half => BPF_H,
            Size::Word => BPF_W,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Mode {
    Abs(Off, Size),
    Ind(Off, Size),
    Len,
    Imm(Off),
    Mem(Off),
    Msh(Off),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Src {
    K(u32),
    X,
}

impl Src {
    pub fn code(self) -> u32 {
        match self {
            Src::K(_) => BPF_K,
            Src::X => BPF_X,
        }
    }

    pub fn value(self) -> u32 {
        match self {
            Src::K(v) => v,
            Src::X => 0,
        }
    }
}

impl fmt::Display for Src {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Src::K(v) => write!(f, "#{}", v),
            Src::X => write!(f, "x"),
        }
    }
}

impl fmt::LowerHex for Src {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Src::K(v) => write!(f, "#0x{:x}", v),
            Src::X => write!(f, "x"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Add(Src),
    Sub(Src),
    Mul(Src),
    Div(Src),
    Or(Src),
    And(Src),
    LShift(Src),
    RShift(Src),
    Neg,
    Mod(Src),
    Xor(Src),
}

/// Conditional jumps compare the accumulator against a constant (BPF_K) or the index register (BPF_X).
#[derive(Clone, Debug, PartialEq)]
pub enum Cond {
    Abs(Off),
    Gt(Src, u8, u8),
    Ge(Src, u8, u8),
    Eq(Src, u8, u8),
    Set(Src, u8, u8),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RVal {
    K(K),
    A,
}

impl RVal {
    pub fn code(&self) -> u32 {
        match self {
            RVal::A => BPF_A,
            RVal::K(_) => BPF_K,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MiscOp {
    Tax,
    Txa,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    /// BPF_LD These instructions copy a value into the accumulator.
    Load(Mode),
    /// BPF_LDX These instructions load a value into the index register.
    LoadX(Mode),
    /// BPF_ST This instruction	stores the accumulator into the scratch memory.
    Store(Off),
    /// BPF_STX This instruction stores the index register in the scratch memory store.
    StoreX(Off),
    /// BPF_ALU The alu instructions perform operations between the accumulator and index register or constant,
    /// and store the result back in the accumulator.
    Alu(Op),
    /// BPF_JMP The jump instructions alter flow of control.
    Jmp(Cond),
    /// BPF_RET The return instructions terminate the filter program
    /// and specify the amount of packet to accept (i.e., they return the truncation amount).
    Ret(Option<RVal>),
    /// BPF_MISC  The miscellaneous category was created for anything
    /// that does not fit into the	above classes,
    /// and for any new instructions that might need to be added.
    Misc(MiscOp),
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::Load(Mode::Abs(off, size)) => write!(f, "ld{}\t[{}]", size.suffix(), off),
            Inst::Load(Mode::Ind(off, size)) => write!(f, "ld{}\t[x + {}]", size.suffix(), off),
            Inst::Load(Mode::Len) => write!(f, "ld\t#pktlen"),
            Inst::Load(Mode::Imm(off)) => write!(f, "ld\t#0x{:x}", off),
            Inst::Load(Mode::Mem(off)) => write!(f, "ld\tM[{}]", off),
            Inst::Load(_) => unreachable!(),

            Inst::LoadX(Mode::Imm(off)) => write!(f, "ldx\t#0x{:x}", off),
            Inst::LoadX(Mode::Mem(off)) => write!(f, "ldx\tM[{}]", off),
            Inst::LoadX(Mode::Msh(off)) => write!(f, "ldxb\t4*([{}]&0xf)", off),
            Inst::LoadX(_) => unreachable!(),

            Inst::Store(off) => write!(f, "st\tM[{}]", off),
            Inst::StoreX(off) => write!(f, "stx\tM[{}]", off),

            Inst::Jmp(Cond::Abs(off)) => write!(f, "ja\t{}", off),
            Inst::Jmp(Cond::Gt(src, jt, jf)) => write!(f, "jgt\t{}\tjt {}\tjf {}", src, jt, jf),
            Inst::Jmp(Cond::Ge(src, jt, jf)) => write!(f, "jge\t{}\tjt {}\tjf {}", src, jt, jf),
            Inst::Jmp(Cond::Eq(src, jt, jf)) => write!(f, "jeq\t{}\tjt {}\tjf {}", src, jt, jf),
            Inst::Jmp(Cond::Set(src, jt, jf)) => write!(f, "jset\t{}\tjt {}\tjf {}", src, jt, jf),

            Inst::Alu(Op::Add(src)) => write!(f, "add\t{}", src),
            Inst::Alu(Op::Sub(src)) => write!(f, "sub\t{}", src),
            Inst::Alu(Op::Mul(src)) => write!(f, "mul\t{}", src),
            Inst::Alu(Op::Div(src)) => write!(f, "div\t{}", src),
            Inst::Alu(Op::Mod(src)) => write!(f, "mod\t{}", src),
            Inst::Alu(Op::And(src)) => write!(f, "and\t{:x}", src),
            Inst::Alu(Op::Or(src)) => write!(f, "or\t{:x}", src),
            Inst::Alu(Op::Xor(src)) => write!(f, "xor\t{:x}", src),
            Inst::Alu(Op::LShift(src)) => write!(f, "lsh\t{}", src),
            Inst::Alu(Op::RShift(src)) => write!(f, "rsh\t{}", src),
            Inst::Alu(Op::Neg) => write!(f, "neg"),

            Inst::Ret(Some(RVal::K(k))) => write!(f, "ret\t#0x{:x}", k),
            Inst::Ret(_) => write!(f, "ret"),

            Inst::Misc(MiscOp::Tax) => write!(f, "tax"),
            Inst::Misc(MiscOp::Txa) => write!(f, "txa"),
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OpCode(u8);

impl OpCode {
    pub fn class(self) -> u8 {
        self.0 & 0x07
    }

    pub fn size(self) -> Size {
        match (self.0 & 0x18) as u32 {
            BPF_W => Size::Word,
            BPF_H => Size::Half,
            BPF_B => Size::Byte,
            _ => Size::Reserved,
        }
    }

    pub fn mode(self) -> u8 {
        self.0 & 0xe0
    }

    pub fn op(self) -> u8 {
        self.0 & 0xf0
    }

    pub fn src(self, k: Off) -> Src {
        match (self.0 & 0x08) as u32 {
            BPF_K => Src::K(k),
            BPF_X => Src::X,
            _ => unreachable!(),
        }
    }

    pub fn rval(self, k: Off) -> Option<RVal> {
        match (self.0 & 0x18) as u32 {
            BPF_K => Some(RVal::K(k)),
            BPF_A => Some(RVal::A),
            _ => None,
        }
    }

    pub fn misc_op(self) -> u8 {
        self.0 & 0xf8
    }
}

impl From<&bpf_insn> for Result<Inst> {
    fn from(inst: &bpf_insn) -> Self {
        let inst = inst.borrow();
        let code = OpCode(inst.code as u8);

        Ok(match code.class() as u32 {
            BPF_LD => Inst::Load(match code.mode() as u32 {
                BPF_IMM => Mode::Imm(inst.k),
                BPF_ABS => Mode::Abs(inst.k, code.size()),
                BPF_IND => Mode::Ind(inst.k, code.size()),
                BPF_MEM if inst.k < BPF_MEMWORDS => Mode::Mem(inst.k),
                BPF_LEN => Mode::Len,
                _ => bail!("invalid BPF_LD mode: 0x{:x}, k = {}", code.mode(), inst.k),
            }),
            BPF_LDX => Inst::LoadX(match code.mode() as u32 {
                BPF_IMM => Mode::Imm(inst.k),
                BPF_MEM if inst.k < BPF_MEMWORDS => Mode::Mem(inst.k),
                BPF_LEN => Mode::Len,
                BPF_MSH => Mode::Msh(inst.k),
                _ => bail!("invalid BPF_LDX mode: 0x{:x}, k = {}", code.mode(), inst.k),
            }),
            BPF_ST if inst.k < BPF_MEMWORDS => Inst::Store(inst.k),
            BPF_STX if inst.k < BPF_MEMWORDS => Inst::StoreX(inst.k),
            BPF_ALU => Inst::Alu(match code.op() as u32 {
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
            BPF_JMP => Inst::Jmp(match code.op() as u32 {
                BPF_JA => Cond::Abs(inst.k),
                BPF_JEQ => Cond::Eq(code.src(inst.k), inst.jt, inst.jf),
                BPF_JGT => Cond::Gt(code.src(inst.k), inst.jt, inst.jf),
                BPF_JGE => Cond::Ge(code.src(inst.k), inst.jt, inst.jf),
                BPF_JSET => Cond::Set(code.src(inst.k), inst.jt, inst.jf),
                _ => bail!("invalid BPF_JMP cond: 0x{:x}", code.op()),
            }),
            BPF_RET => Inst::Ret(code.rval(inst.k)),
            BPF_MISC if code.misc_op() == BPF_TAX as u8 => Inst::Misc(MiscOp::Tax),
            BPF_MISC if code.misc_op() == BPF_TXA as u8 => Inst::Misc(MiscOp::Txa),
            _ => bail!("invalid BPF inst: {:#?}", inst),
        })
    }
}

#[macro_export]
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

#[macro_export]
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

            Inst::Ret(Some(rval)) => BPF_STMT!(BPF_RET | rval.code()),
            Inst::Ret(None) => BPF_STMT!(BPF_RET),

            Inst::Misc(MiscOp::Tax) => BPF_STMT!(BPF_MISC | BPF_TAX),
            Inst::Misc(MiscOp::Txa) => BPF_STMT!(BPF_MISC | BPF_TXA),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn inst() {}
}
