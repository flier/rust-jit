use std::fmt;

use crate::ast::{Cond, Inst, MiscOp, Mode, Op, Program, RVal, Size, Src};

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

impl Size {
    fn suffix(self) -> &'static str {
        match self {
            Size::Byte => "b",
            Size::Half => "h",
            _ => "",
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, inst) in self.0.iter().enumerate() {
            write!(f, "({:03}) ", idx)?;

            match inst {
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
                Inst::Jmp(Cond::Gt(src, jt, jf)) => write!(
                    f,
                    "jgt\t{:x}\tjt {}\tjf {}",
                    src,
                    idx + 1 + *jt as usize,
                    idx + 1 + *jf as usize
                ),
                Inst::Jmp(Cond::Ge(src, jt, jf)) => write!(
                    f,
                    "jge\t{:x}\tjt {}\tjf {}",
                    src,
                    idx + 1 + *jt as usize,
                    idx + 1 + *jf as usize
                ),
                Inst::Jmp(Cond::Eq(src, jt, jf)) => write!(
                    f,
                    "jeq\t{:x}\tjt {}\tjf {}",
                    src,
                    idx + 1 + *jt as usize,
                    idx + 1 + *jf as usize
                ),
                Inst::Jmp(Cond::Set(src, jt, jf)) => write!(
                    f,
                    "jset\t{:x}\tjt {}\tjf {}",
                    src,
                    idx + 1 + *jt as usize,
                    idx + 1 + *jf as usize
                ),

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

                Inst::Ret(Some(RVal::K(k))) => write!(f, "ret\t#{}", k),
                Inst::Ret(_) => write!(f, "ret"),

                Inst::Misc(MiscOp::Tax) => write!(f, "tax"),
                Inst::Misc(MiscOp::Txa) => write!(f, "txa"),
            }?;

            writeln!(f, "")?;
        }

        Ok(())
    }
}
