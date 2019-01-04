#[macro_use]
extern crate llvm_jit as jit;

use std::mem;

use self::jit::prelude::*;

fn main() {
    pretty_env_logger::init();

    // Set up a context, module and builder in that context.
    let context = Context::new();
    let module = context.create_module("sum");

    // get a type for sum function
    let i64_t = context.int64_t();
    let function_type = func!(|i64_t, i64_t, i64_t| -> i64_t);

    // add it to our module
    let function = module.add_function("sum", function_type);

    // Create a basic block in the function and set our builder to generate code in it.
    let bb = function.append_basic_block_in_context("entry", &context);

    context.create_builder().within(bb, || {
        // get the function's arguments
        let x = function.get_param(0).unwrap();
        let y = function.get_param(1).unwrap();
        let z = function.get_param(2).unwrap();

        // Emit a `ret` into the function
        ret!(add!(add!(x, y; "sum.1"), z; "sum.2"))
    });

    // Dump the module as IR to stdout.
    module.dump();

    // may need to set `LLVM_SYS_50_FFI_WORKAROUND` when build `llvm-sys`
    //
    // see also: https://bitbucket.org/tari/llvm-sys.rs/issues/12/

    jit::target::NativeTarget::init().unwrap();
    jit::target::NativeAsmPrinter::init().unwrap();

    // build an execution engine
    let engine = ExecutionEngine::for_module(module).unwrap();

    let addr = engine.get_function_address("sum").unwrap();

    let f: extern "C" fn(u64, u64, u64) -> u64 = unsafe { mem::transmute(addr) };

    let x = 1;
    let y = 1;
    let z = 1;
    let res = f(x, y, z);

    println!("{} + {} + {} = {}", x, y, z, res);
}
