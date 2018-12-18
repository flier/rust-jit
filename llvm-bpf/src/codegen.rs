use std::mem;

use jit::insts::AstNode;
use jit::prelude::*;

use crate::ast::{
    Inst, Mode, Program,
    Size::{self, *},
};
use crate::errors::Result;

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
    pub fn gen<S: AsRef<str>>(&self, ctxt: &Context, m: &Module, name: S) -> Result<Function> {
        let name = name.as_ref();
        let i8_t = ctxt.int8_t();
        let i16_t = ctxt.int16_t();
        let i32_t = ctxt.int32_t();
        let i8_ptr_t = i8_t.ptr_t();
        let f = m.add_function(name, func!(|i8_ptr_t, i32_t| -> i32_t));
        let arg0 = f.get_param(0).unwrap();
        let arg1 = f.get_param(1).unwrap();
        let pkt = arg0.set_name("pkt");
        let len = arg1.set_name("len");

        info!("generate BPF function `{}` with signature: {}", name, *f);

        let entry = f.append_basic_block_in_context("entry", ctxt);

        let builder = ctxt.create_builder();

        let (a, x, m) = builder.within(entry, || {
            (
                alloca!(i32_t; "A"),
                alloca!(i32_t; "X"),
                alloca!(i32_t.array_t(BPF_MEMWORDS as usize); "M"),
            )
        });

        builder.within(entry, || (store!(i32_t.uint(0), a), store!(i32_t.uint(0), x)));

        for inst in self {
            match inst {
                Inst::Load(mode) => {
                    let value: AstNode = match mode {
                        Mode::Abs(off, size) => {
                            let k = i32_t.uint(*off as u64);
                            let name = format!("pkt[k:{}]", size.bytes());

                            load!(bit_cast!(gep!(*pkt, k), size.ty(ctxt)); name).into()
                        }
                        Mode::Ind(off, size) => {
                            let k = i32_t.uint(*off as u64);
                            let name = format!("pkt[X+k:{}]", size.bytes());

                            load!(bit_cast!(gep!(*pkt, add!(x, k)), size.ty(ctxt)); name).into()
                        }
                        Mode::Len => (*len).into(),
                        Mode::Imm(off) => i32_t.uint(*off as u64).into(),
                        Mode::Mem(off) => extract_value!(m, *off; "M[k]").into(),
                        _ => unreachable!(),
                    };

                    store!(value, a).emit_to(&builder);
                }
                _ => {
                    debug!("skip inst: {:?}", inst);
                }
            };
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

        let p = compile(DLT_EN10MB, "ip and udp").unwrap();
        assert!(p.validate());

        let f = p.gen(&ctxt, &m, "filter");

        debug!("generated module:\n{}", m);

        let f = f.unwrap();
    }
}
