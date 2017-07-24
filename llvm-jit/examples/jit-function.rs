extern crate pretty_env_logger;
#[macro_use]
extern crate llvm_jit as jit;

use std::mem;

use jit::prelude::*;

fn main() {
    pretty_env_logger::init().unwrap();

    // Set up a context, module and builder in that context.
    let context = jit::Context::new();
    let module = jit::Module::with_context("sum", &context);
    let builder = jit::Builder::with_context(&context);

    // get a type for sum function
    let i64t = context.int64();
    let argts = [i64t, i64t, i64t];
    let function_type = jit::FunctionType::new(i64t, &argts, false);

    // add it to our module
    let function = module.add_function("sum", function_type);

    // Create a basic block in the function and set our builder to generate code in it.
    let bb = function.append_basic_block(&context, "entry");

    builder.position(jit::Position::AtEnd(bb));

    // get the function's arguments
    let x = function.param(0);
    let y = function.param(1);
    let z = function.param(2);

    let sum = builder.emit(add!(x, y, "sum.1"));
    let sum = builder.emit(add!(sum, z, "sum.2"));

    // Emit a `ret` into the function
    builder.emit(ret!(sum));

    // done building
    drop(builder);

    // Dump the module as IR to stdout.
    module.dump();

    // may need to set `LLVM_SYS_40_FFI_WORKAROUND` when build `llvm-sys`
    //
    // see also: https://bitbucket.org/tari/llvm-sys.rs/issues/12/

    jit::MCJIT::init();
    jit::NativeTarget::init().unwrap();
    jit::NativeAsmPrinter::init().unwrap();

    // build an execution engine
    let engine = jit::ExecutionEngine::for_module(&module).unwrap();

    let addr = engine.get_function_address("sum").unwrap();

    let f: extern "C" fn(u64, u64, u64) -> u64 = unsafe { mem::transmute(addr) };

    let x = 1;
    let y = 1;
    let z = 1;
    let res = f(x, y, z);

    println!("{} + {} + {} = {}", x, y, z, res);
}
