use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use jit::insts::AstNode;
use jit::prelude::*;

use crate::ast::{
    Cond, Inst, MiscOp, Mode, Op, Program, RVal,
    Size::{self, *},
    Src,
};
use crate::errors::Result;
use crate::raw::BPF_MEMWORDS;

impl Size {
    fn bytes(self) -> usize {
        match self {
            Reserved => 0,
            Byte => mem::size_of::<u8>(),
            Half => mem::size_of::<u16>(),
            Word => mem::size_of::<u32>(),
        }
    }

    fn ty(self, ctxt: &Context) -> PointerType {
        match self {
            Reserved => unreachable!(),
            Byte => ctxt.int8_t().ptr_t(),
            Half => ctxt.int16_t().ptr_t(),
            Word => ctxt.int32_t().ptr_t(),
        }
    }
}

impl Program {
    pub fn gen<S: AsRef<str>>(self, m: &Module, name: S) -> Result<Function> {
        let ctxt = m.context();
        let name = name.as_ref();
        let i8_ptr_t = ctxt.int8_t().ptr_t();
        let i32_t = ctxt.int32_t();
        let f = m.add_function(name, func!(|i8_ptr_t, i32_t| -> i32_t));
        let arg0 = f.get_param(0).unwrap();
        let arg1 = f.get_param(1).unwrap();
        let pkt = arg0.set_name("pkt");
        let len = arg1.set_name("len");

        info!("generate BPF function `{}` with signature: {}", name, *f);

        let builder = ctxt.create_builder();

        let uint32 = |k: u32| -> ConstantInt { i32_t.uint(k as u64) };

        let labels = Rc::new(RefCell::new(HashMap::new()));
        let mut get_or_insert_label = |off| -> BasicBlock {
            *labels
                .borrow_mut()
                .entry(off)
                .or_insert_with(|| f.append_basic_block_in_context(format!("L{}", off), &ctxt))
        };

        let entry = f.append_basic_block_in_context("entry", &ctxt);
        let (a, x, m) = builder.within(entry, || {
            (
                alloca!(i32_t; "A"),
                alloca!(i32_t; "X"),
                alloca!(i32_t.array_t(BPF_MEMWORDS as usize); "M"),
            )
        });

        builder.within(entry, || (store!(uint32(0), a), store!(uint32(0), x)));

        let get_src_value = |src| -> AstNode {
            match src {
                Src::K(k) => uint32(k).into(),
                Src::X => load!(x; "x").into(),
            }
        };

        for (idx, inst) in self.into_iter().enumerate() {
            if let Some(label) = labels.borrow().get(&idx) {
                builder.position_at_end(*label);
            }

            match inst {
                Inst::Load(mode) => {
                    let value: AstNode = match mode {
                        Mode::Abs(off, size) => {
                            let name = format!("pkt[k:{}]", size.bytes());

                            zext!(
                                load!(bit_cast!(gep!(*pkt, uint32(off); "p"), size.ty(&ctxt)); name),
                                i32_t
                            )
                            .into()
                        }
                        Mode::Ind(off, size) => {
                            let name = format!("pkt[X+k:{}]", size.bytes());

                            zext!(
                                load!(bit_cast!(gep!(*pkt, add!(load!(x; "x"), uint32(off); "idx"); "p"), size.ty(&ctxt)); name),
                                i32_t
                            )
                            .into()
                        }
                        Mode::Len => (*len).into(),
                        Mode::Imm(off) => uint32(off).into(),
                        Mode::Mem(off) => extract_value!(m, off; "M[k]").into(),
                        _ => unreachable!(),
                    };

                    store!(value, a).emit_to(&builder);
                }
                Inst::Store(off) => {
                    store!(load!(a; "a"), gep!(m, uint32(off); "M[k]")).emit_to(&builder);
                }
                Inst::StoreX(off) => {
                    store!(load!(x; "x"), gep!(m, uint32(off); "M[k]")).emit_to(&builder);
                }

                Inst::Jmp(Cond::Abs(off)) => {
                    br!(get_or_insert_label(idx + off as usize + 1)).emit_to(&builder);
                }
                Inst::Jmp(Cond::Gt(src, jt, jf)) => {
                    let cond = ugt!(load!(a; "a"), get_src_value(src));

                    br!(
                        cond => get_or_insert_label(idx + jt as usize + 1),
                        _ => get_or_insert_label(idx + jf as usize + 1)
                    )
                    .emit_to(&builder);
                }
                Inst::Jmp(Cond::Ge(src, jt, jf)) => {
                    let cond = uge!(load!(a; "a"), get_src_value(src));

                    br!(
                        cond => get_or_insert_label(idx + jt as usize + 1),
                        _ => get_or_insert_label(idx + jf as usize + 1)
                    )
                    .emit_to(&builder);
                }
                Inst::Jmp(Cond::Eq(src, jt, jf)) => {
                    let cond = eq!(load!(a; "a"), get_src_value(src));

                    br!(
                        cond => get_or_insert_label(idx + jt as usize + 1),
                        _ => get_or_insert_label(idx + jf as usize + 1)
                    )
                    .emit_to(&builder);
                }
                Inst::Jmp(Cond::Set(src, jt, jf)) => {
                    let cond = eq!(and!(load!(a; "a"), get_src_value(src)), get_src_value(src));

                    br!(
                        cond => get_or_insert_label(idx + jt as usize + 1),
                        _ => get_or_insert_label(idx + jf as usize + 1)
                    )
                    .emit_to(&builder);
                }
                Inst::Alu(op) => {
                    let value: AstNode = match op {
                        Op::Add(src) => add!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Sub(src) => sub!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Mul(src) => mul!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Div(src) => udiv!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Mod(src) => urem!(load!(a; "a"), get_src_value(src)).into(),
                        Op::And(src) => and!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Or(src) => or!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Xor(src) => xor!(load!(a; "a"), get_src_value(src)).into(),
                        Op::LShift(src) => shl!(load!(a; "a"), get_src_value(src)).into(),
                        Op::RShift(src) => lshr!(load!(a; "a"), get_src_value(src)).into(),
                        Op::Neg => sub!(uint32(0), load!(a; "a")).into(),
                    };

                    store!(value, a).emit_to(&builder);
                }
                Inst::Ret(rval) => {
                    let result: AstNode = match rval {
                        Some(RVal::K(k)) => uint32(k).into(),
                        Some(RVal::A) => a.into(),
                        None => uint32(0).into(),
                    };

                    ret!(result).emit_to(&builder);
                }
                Inst::Misc(MiscOp::Tax) => {
                    store!(load!(a; "a"), x).emit_to(&builder);
                }
                Inst::Misc(MiscOp::Txa) => {
                    store!(load!(x; "x"), a).emit_to(&builder);
                }
                _ => {
                    debug!("skip inst: {:?}", inst);
                }
            }
        }

        f.verify()?;

        Ok(f)
    }
}

#[cfg(test)]
mod tests {
    use crate::compile;
    use crate::raw::*;

    use super::*;

    #[test]
    fn program() {
        let _ = pretty_env_logger::try_init();

        jit::target::AllTargetMCs::init();

        let ctxt = Context::new();
        let m = ctxt.create_module("test");

        let p = compile(DLT_EN10MB, "ip and udp and port 53").unwrap();
        assert!(p.validate());

        let f = p.gen(&m, "filter");

        debug!("generated module:\n{}", m);

        let f = f.unwrap();
    }
}
