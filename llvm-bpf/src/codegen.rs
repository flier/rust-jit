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
use crate::display::InstFmt;
use crate::errors::Result;
use crate::raw::BPF_MEMWORDS;

pub type Filter = extern "C" fn(*const u8, u32) -> u32;

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

        let i8_t = ctxt.int8_t();
        let i8_ptr_t = i8_t.ptr_t();
        let i32_t = ctxt.int32_t();

        let donothing = m.intrinsic_declaration(IntrinsicId::donothing, &[][..]);

        let f = m.add_function(name, func!(|i8_ptr_t, i32_t| -> i32_t));

        let arg0 = f.get_param(0).unwrap();
        let arg1 = f.get_param(1).unwrap();
        let pkt = arg0.set_name("pkt");
        let len = arg1.set_name("len");

        info!("generate BPF function `{}` with signature: {}", name, *f);

        let builder = ctxt.create_builder();

        let uint8 = |k: u8| -> ConstantInt { i8_t.uint(k as u64) };
        let uint32 = |k: u32| -> ConstantInt { i32_t.uint(k as u64) };

        let labels = Rc::new(RefCell::new(HashMap::new()));
        let get_or_insert_label = |off| -> BasicBlock {
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

        let md_opcode = ctxt.md_kind_id("opcode");

        for (idx, inst) in self.into_iter().enumerate() {
            if let Some(label) = labels.borrow().get(&idx) {
                builder.position_at_end(*label);
            }

            if cfg!(feature = "debug") {
                call!(donothing)
                    .emit_to(&builder)
                    .set_metadata(md_opcode, ctxt.md_string(InstFmt(idx, &inst).to_string()));
            }

            match inst {
                Inst::Load(mode) => {
                    let value: AstNode = match mode {
                        Mode::Abs(off, size) => {
                            let p = bit_cast!(gep!(*pkt, uint32(off); "p"), size.ty(&ctxt); "p");

                            zext!(load!(p; format!("P[k:{}]", size.bytes())), i32_t; "v").into()
                        }
                        Mode::Ind(off, size) => {
                            let p = gep!(*pkt, add!(load!(x; "x"), uint32(off); "idx"); "p");
                            let p = bit_cast!(p, size.ty(&ctxt); "p");

                            zext!(load!(p; format!("P[X+k:{}]", size.bytes())), i32_t; "v").into()
                        }
                        Mode::Len => (*len).into(),
                        Mode::Imm(off) => uint32(off).into(),
                        Mode::Mem(off) => extract_value!(m, off; "M[k]").into(),
                        _ => unreachable!(),
                    };

                    store!(value, a).emit_to(&builder);
                }
                Inst::LoadX(mode) => {
                    let value: AstNode = match mode {
                        Mode::Len => (*len).into(),
                        Mode::Imm(off) => uint32(off).into(),
                        Mode::Mem(off) => extract_value!(m, off; "M[k]").into(),
                        Mode::Msh(off) => mul!(
                            uint32(4),
                            zext!(
                                and!(
                                    load!(gep!(*pkt, uint32(off); "p"); "P[k:1]"),
                                    uint8(0x0f);
                                    "P[k:1]&0xf"
                                ),
                                i32_t
                            );
                            "4*(P[k:1]&0xf)"
                        )
                        .into(),
                        _ => unreachable!(),
                    };

                    store!(value, x).emit_to(&builder);
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
            }
        }

        f.verify()?;

        Ok(f)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::compile;
    use crate::raw::*;

    use super::*;

    #[test]
    fn program() {
        let _ = pretty_env_logger::try_init();

        jit::target::NativeTarget::init().unwrap();
        jit::target::NativeDisassembler::init().unwrap();
        jit::target::NativeAsmPrinter::init().unwrap();

        let ctxt = Context::new();
        let m = ctxt.create_module("test");

        let p = compile(DLT_EN10MB, "ip and udp and port 53").unwrap();
        assert!(p.validate());

        debug!("compiled program:\n{}", p);
        // (000) ldh	[12]
        // (001) jeq	#0x800	jt 2	jf 12
        // (002) ldb	[23]
        // (003) jeq	#0x11	jt 4	jf 12
        // (004) ldh	[20]
        // (005) jset	#0x1fff	jt 12	jf 6
        // (006) ldxb	4*([14]&0xf)
        // (007) ldh	[x + 14]
        // (008) jeq	#0x35	jt 11	jf 9
        // (009) ldh	[x + 16]
        // (010) jeq	#0x35	jt 11	jf 12
        // (011) ret	#65535
        // (012) ret	#0

        let fname = "filter";
        let f = p.gen(&m, fname);

        debug!("generated module:\n{}", m);

        let f = f.unwrap();

        debug!("generated filter: {:?}", f);

        let passmgr = jit::PassManager::new();
        jit::PassManagerBuilder::new()
            .set_opt_level(3)
            .populate_module_pass_manager(&passmgr);
        passmgr.run(&m);

        // build an execution engine
        let mut opts = jit::engine::MCJITCompilerOptions::default();
        let mut mm = jit::engine::mmap::MemoryManager::default();
        opts.MCJMM = jit::engine::MCJITMemoryManager::from(&mut mm).into_raw();
        let engine = jit::engine::MCJITCompiler::for_module(m, opts).unwrap();

        let addr = engine.get_function_address(fname).unwrap() as *const u8;
        let section = mm.code_sections.iter().find(|section| section.contains(addr)).unwrap();
        let size = section.len();

        let triple = Target::default_triple();
        let disasm = jit::Disasm::new::<()>(&triple, 0, None, None, None);

        let off = addr as usize - section.begin() as usize;
        let code = &section[off..size];
        let mut cur = Cursor::new(code);
        for (off, _len, inst) in disasm.disasm_insts(&mut cur, 0) {
            debug!("0x{:04x} {}", off, inst);
        }

        let f: Filter = unsafe { mem::transmute(addr) };

        let pkt = &[];
        let res = f(pkt.as_ptr(), pkt.len() as u32);

        assert_eq!(res, 65535);
    }
}
