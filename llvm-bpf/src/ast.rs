use std::borrow::Borrow;
use std::fmt;
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

pub type Off = u32;
pub type A = u32;
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

#[derive(Clone, Debug, PartialEq)]
pub enum Src {
    K(u32),
    X,
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
                BPF_MEM => Mode::Mem(inst.k),
                BPF_LEN => Mode::Len,
                _ => bail!("invalid BPF_LD mode: 0x{:x}", code.mode()),
            }),
            BPF_LDX => Inst::LoadX(match code.mode() as u32 {
                BPF_IMM => Mode::Imm(inst.k),
                BPF_MEM => Mode::Mem(inst.k),
                BPF_LEN => Mode::Len,
                BPF_MSH => Mode::Msh(inst.k),
                _ => bail!("invalid BPF_LDX mode: 0x{:x}", code.mode()),
            }),
            BPF_ST => Inst::Store(inst.k),
            BPF_STX => Inst::StoreX(inst.k),
            BPF_ALU => Inst::Alu(match code.op() as u32 {
                BPF_ADD => Op::Add(code.src(inst.k)),
                BPF_SUB => Op::Sub(code.src(inst.k)),
                BPF_MUL => Op::Mul(code.src(inst.k)),
                BPF_DIV => Op::Div(code.src(inst.k)),
                BPF_OR => Op::Or(code.src(inst.k)),
                BPF_AND => Op::And(code.src(inst.k)),
                BPF_LSH => Op::LShift(code.src(inst.k)),
                BPF_RSH => Op::RShift(code.src(inst.k)),
                BPF_NEG => Op::Neg,
                BPF_MOD => Op::Mod(code.src(inst.k)),
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
