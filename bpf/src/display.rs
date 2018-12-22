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

pub struct InstFmt<'a>(pub usize, pub &'a Inst);

impl<'a> fmt::Display for InstFmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let idx = self.0;
        let inst = self.1;

        write!(f, "({:03}) ", idx)?;

        match inst {
            Inst::Load(Mode::Abs(off, size)) => write!(f, "{:<8} [{}]", format!("ld{}", size.suffix()), off),
            Inst::Load(Mode::Ind(off, size)) => write!(f, "{:<8} [x + {}]", format!("ld{}", size.suffix()), off),
            Inst::Load(Mode::Len) => write!(f, "{:<8} #pktlen", "ld"),
            Inst::Load(Mode::Imm(off)) => write!(f, "{:<8} #0x{:x}", "ld", off),
            Inst::Load(Mode::Mem(off)) => write!(f, "{:<8} M[{}]", "ld", off),
            Inst::Load(_) => unreachable!(),

            Inst::LoadX(Mode::Imm(off)) => write!(f, "{:<8} #0x{:x}", "ldx", off),
            Inst::LoadX(Mode::Mem(off)) => write!(f, "{:<8} M[{}]", "ldx", off),
            Inst::LoadX(Mode::Msh(off)) => write!(f, "{:<8} 4*([{}:1]&0xf)", "ldxb", off),
            Inst::LoadX(_) => unreachable!(),

            Inst::Store(off) => write!(f, "{:<8} M[{}]", "st", off),
            Inst::StoreX(off) => write!(f, "{:<8} M[{}]", "stx", off),

            Inst::Jmp(Cond::Abs(off)) => write!(f, "{:<8} {}", "ja", off),
            Inst::Jmp(Cond::Gt(src, jt, jf)) => write!(
                f,
                "{:<8} {:<16x} jt {} jf {}",
                "jgt",
                src,
                idx + 1 + *jt as usize,
                idx + 1 + *jf as usize
            ),
            Inst::Jmp(Cond::Ge(src, jt, jf)) => write!(
                f,
                "{:<8} {:<16x} jt {} jf {}",
                "jge",
                src,
                idx + 1 + *jt as usize,
                idx + 1 + *jf as usize
            ),
            Inst::Jmp(Cond::Eq(src, jt, jf)) => write!(
                f,
                "{:<8} {:<16x} jt {} jf {}",
                "jeq",
                src,
                idx + 1 + *jt as usize,
                idx + 1 + *jf as usize
            ),
            Inst::Jmp(Cond::Set(src, jt, jf)) => write!(
                f,
                "{:<8} {:<16x} jt {} jf {}",
                "jset",
                src,
                idx + 1 + *jt as usize,
                idx + 1 + *jf as usize
            ),

            Inst::Alu(Op::Add(src)) => write!(f, "{:<8} {}", "add", src),
            Inst::Alu(Op::Sub(src)) => write!(f, "{:<8} {}", "sub", src),
            Inst::Alu(Op::Mul(src)) => write!(f, "{:<8} {}", "mul", src),
            Inst::Alu(Op::Div(src)) => write!(f, "{:<8} {}", "div", src),
            Inst::Alu(Op::Mod(src)) => write!(f, "{:<8} {}", "mod", src),
            Inst::Alu(Op::And(src)) => write!(f, "{:<8} {:x}", "and", src),
            Inst::Alu(Op::Or(src)) => write!(f, "{:<8} {:x}", "or", src),
            Inst::Alu(Op::Xor(src)) => write!(f, "{:<8} {:x}", "xor", src),
            Inst::Alu(Op::LShift(src)) => write!(f, "{:<8} {}", "lsh", src),
            Inst::Alu(Op::RShift(src)) => write!(f, "{:<8} {}", "rsh", src),
            Inst::Alu(Op::Neg) => write!(f, "neg"),

            Inst::Ret(Some(RVal::K(k))) => write!(f, "{:<8} #{}", "ret", k),
            Inst::Ret(_) => write!(f, "ret"),

            Inst::Misc(MiscOp::Tax) => write!(f, "tax"),
            Inst::Misc(MiscOp::Txa) => write!(f, "txa"),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, inst) in self.0.iter().enumerate() {
            writeln!(f, "{}", InstFmt(idx, inst))?;
        }

        Ok(())
    }
}
