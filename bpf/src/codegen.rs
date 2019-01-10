use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::ops::Deref;
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
        Generator::new(m, name).gen(self.into_iter())
    }
}

struct Generator<'a> {
    ctxt: jit::Context,
    m: &'a jit::Module,
    f: jit::Function,
    state: State,
}

impl<'a> Deref for Generator<'a> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

struct State {
    builder: jit::IRBuilder,
    pkt: jit::ValueRef,
    len: jit::ValueRef,
    a: jit::insts::AllocaInst,
    x: jit::insts::AllocaInst,
    mem: jit::insts::AllocaInst,
    labels: Rc<RefCell<HashMap<usize, BasicBlock>>>,
}

impl State {
    pub fn new(ctxt: &jit::Context, f: &jit::Function) -> Self {
        let i32_t = ctxt.int32_t();
        let uint32 = |v: u64| -> ConstantInt { i32_t.uint(v) };

        let pkt = f.param(0).unwrap();
        let len = f.param(1).unwrap();

        pkt.set_name("pkt");
        len.set_name("len");

        let builder = ctxt.create_builder();

        let entry = f.append_basic_block_in_context("entry", &ctxt);
        let (a, x, mem) = builder.within(entry, || {
            (
                alloca!(i32_t; "A"),
                alloca!(i32_t; "X"),
                alloca!(i32_t.array_t(BPF_MEMWORDS as usize); "M"),
            )
        });

        builder.within(entry, || (store!(uint32(0), a), store!(uint32(0), x)));

        let labels = Rc::new(RefCell::new(HashMap::new()));

        State {
            builder,
            pkt,
            len,
            a,
            x,
            mem,
            labels,
        }
    }
}

impl<'a> Generator<'a> {
    pub fn new<S: AsRef<str>>(m: &'a jit::Module, name: S) -> Self {
        let ctxt = m.context();
        let name = name.as_ref();

        let i8_t = ctxt.int8_t();
        let i8_ptr_t = i8_t.ptr_t();
        let i32_t = ctxt.int32_t();

        let f = m.add_function(name, func!(|i8_ptr_t, i32_t| -> i32_t));

        info!("generate BPF function `{}` with signature: {}", name, *f);

        let state = State::new(&ctxt, &f);

        Generator { ctxt, m, f, state }
    }

    pub fn gen<I>(self, insts: I) -> Result<jit::Function>
    where
        I: Iterator<Item = Inst>,
    {
        let donothing = self.m.intrinsic_declaration(IntrinsicId::donothing, &[][..]);
        let md_opcode = self.ctxt.md_kind_id("opcode");

        for (idx, inst) in insts.enumerate() {
            if let Some(label) = self.labels.borrow().get(&idx) {
                self.builder.position_at_end(*label);
            }

            if cfg!(feature = "debug") {
                call!(donothing)
                    .emit_to(&self.builder)
                    .set_metadata(md_opcode, self.ctxt.md_string(InstFmt(idx, &inst).to_string()));
            }

            self.gen_inst(idx, inst).emit_to(&self.builder);
        }

        self.f.verify()?;

        Ok(self.f)
    }

    pub fn gen_inst(&self, idx: usize, inst: Inst) -> AstNode {
        let i8_t = self.ctxt.int8_t();
        let i32_t = self.ctxt.int32_t();

        let uint8 = |k: u8| -> ConstantInt { i8_t.uint(k as u64) };
        let uint32 = |k: u32| -> ConstantInt { i32_t.uint(k as u64) };

        let load_a = || load!(self.a; "a");
        let load_x = || load!(self.x; "x");

        let get_src_value = |src| -> AstNode {
            match src {
                Src::K(k) => self.ctxt.uint32(k).into(),
                Src::X => load!(self.x; "x").into(),
            }
        };

        let get_or_insert_label_at_offset = |off: usize| {
            let off = idx + off + 1;

            *self
                .labels
                .borrow_mut()
                .entry(off)
                .or_insert_with(|| self.f.append_basic_block_in_context(format!("L{}", off), &self.ctxt))
        };

        match inst {
            Inst::Load(mode) => {
                let value: AstNode = match mode {
                    Mode::Abs(off, size) => {
                        let p = bit_cast!(gep!(self.pkt, uint32(off); "p"), size.ty(&self.ctxt); "p");

                        zext!(load!(p; format!("P[k:{}]", size.bytes())), i32_t; "v").into()
                    }
                    Mode::Ind(off, size) => {
                        let p = gep!(self.pkt, add!(load_x(), uint32(off); "idx"); "p");
                        let p = bit_cast!(p, size.ty(&self.ctxt); "p");

                        zext!(load!(p; format!("P[X+k:{}]", size.bytes())), i32_t; "v").into()
                    }
                    Mode::Len => self.len.into(),
                    Mode::Imm(off) => uint32(off).into(),
                    Mode::Mem(off) => extract_value!(self.mem, off; "M[k]").into(),
                    _ => unreachable!(),
                };

                store!(value, self.a).into()
            }
            Inst::LoadX(mode) => {
                let value: AstNode = match mode {
                    Mode::Len => self.len.into(),
                    Mode::Imm(off) => uint32(off).into(),
                    Mode::Mem(off) => extract_value!(self.mem, off; "M[k]").into(),
                    Mode::Msh(off) => mul!(
                        uint32(4),
                        zext!(
                            and!(
                                load!(gep!(self.pkt, uint32(off); "p"); "P[k:1]"),
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

                store!(value, self.x).into()
            }

            Inst::Store(off) => store!(load_a(), gep!(self.mem, uint32(off); "M[k]")).into(),
            Inst::StoreX(off) => store!(load_x(), gep!(self.mem, uint32(off); "M[k]")).into(),

            Inst::Jmp(Cond::Abs(off)) => br!(get_or_insert_label_at_offset(off as usize)).into(),
            Inst::Jmp(Cond::Gt(src, jt, jf)) => {
                let cond = ugt!(load_a(), get_src_value(src));

                br!(
                    cond => get_or_insert_label_at_offset(jt as usize),
                    _ =>get_or_insert_label_at_offset(jf as usize)
                )
                .into()
            }
            Inst::Jmp(Cond::Ge(src, jt, jf)) => {
                let cond = uge!(load_a(), get_src_value(src));

                br!(
                    cond => get_or_insert_label_at_offset(jt as usize),
                    _ => get_or_insert_label_at_offset(jf as usize)
                )
                .into()
            }
            Inst::Jmp(Cond::Eq(src, jt, jf)) => {
                let cond = eq!(load_a(), get_src_value(src));

                br!(
                    cond => get_or_insert_label_at_offset(jt as usize),
                    _ => get_or_insert_label_at_offset(jf as usize)
                )
                .into()
            }
            Inst::Jmp(Cond::Set(src, jt, jf)) => {
                let cond = eq!(and!(load_a(), get_src_value(src)), get_src_value(src));

                br!(
                    cond => get_or_insert_label_at_offset(jt as usize),
                    _ => get_or_insert_label_at_offset(jf as usize)
                )
                .into()
            }
            Inst::Alu(op) => {
                let value: AstNode = match op {
                    Op::Add(src) => add!(load_a(), get_src_value(src)).into(),
                    Op::Sub(src) => sub!(load_a(), get_src_value(src)).into(),
                    Op::Mul(src) => mul!(load_a(), get_src_value(src)).into(),
                    Op::Div(src) => udiv!(load_a(), get_src_value(src)).into(),
                    Op::Mod(src) => urem!(load_a(), get_src_value(src)).into(),
                    Op::And(src) => and!(load_a(), get_src_value(src)).into(),
                    Op::Or(src) => or!(load_a(), get_src_value(src)).into(),
                    Op::Xor(src) => xor!(load_a(), get_src_value(src)).into(),
                    Op::LShift(src) => shl!(load_a(), get_src_value(src)).into(),
                    Op::RShift(src) => lshr!(load_a(), get_src_value(src)).into(),
                    Op::Neg => sub!(uint32(0), load_a()).into(),
                };

                store!(value, self.a).into()
            }
            Inst::Ret(rval) => {
                let result: AstNode = match rval {
                    Some(RVal::K(k)) => uint32(k).into(),
                    Some(RVal::A) => self.a.into(),
                    None => uint32(0).into(),
                };

                ret!(result).into()
            }
            Inst::Misc(MiscOp::Tax) => store!(load_a(), self.x).into(),
            Inst::Misc(MiscOp::Txa) => store!(load_x(), self.a).into(),
        }
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

        let addr = engine.function_address(fname).unwrap() as *const u8;
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
