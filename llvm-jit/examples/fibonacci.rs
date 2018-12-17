//===----------------------------------------------------------------------===//
//
// This small program provides an example of how to build quickly a small module
// with function Fibonacci and execute it with the JIT.
//
// The goal of this snippet is to create in the memory the LLVM module
// consisting of one function as follow:
//
//   int fib(int x) {
//     if(x<=2) return 1;
//     return fib(x-1)+fib(x-2);
//   }
//
// Once we have this, we compile the module via JIT, then execute the `fib'
// function and return result to a driver, i.e. to a "host program".
//
//===----------------------------------------------------------------------===//
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_sys as llvm;

use crate::jit::prelude::*;

fn create_fib_function(context: &Context, module: &Module) -> Function {
    // Create the fib function and insert it into module M.  This function is said
    // to return an int and take an int parameter.
    let i32_t = context.int32_t();
    let fib_f = module.get_or_insert_function("fib", i32_t, &[i32_t]);

    // Add a basic block to the function.
    let entry_bb = fib_f.append_basic_block_in_context("entry", &context);

    // Create a basic block builder with default parameters.
    let mut builder = context.create_builder();
    builder.position_at_end(entry_bb);

    // Get the constants.
    let one = i32_t.int(1);
    let two = i32_t.int(2);

    // Get the integer argument of the add1 function...
    let argx = fib_f.get_param(0).unwrap(); // Get the arg
    argx.set_name("AnArg"); // Give it a nice symbolic name for fun.

    // Create the true_block.
    let ret_bb = fib_f.append_basic_block_in_context("return", &context);
    // Create an exit block.
    let recurse_bb = fib_f.append_basic_block_in_context("recurse", &context);

    // Create the "if (arg < 2) goto exitbb"
    builder <<= br!(
            icmp!(SLE argx, two; "cond") => ret_bb,
            _ => recurse_bb
        );

    // Create: ret int 1
    builder.position_at_end(ret_bb);

    builder <<= ret!(one);

    builder.position_at_end(recurse_bb);

    // create fib(x-1)
    let arg = sub!(argx, one; "arg");
    let call_fib_x1 = call!(fib_f, arg; "fibx1");

    // create fib(x-2)
    let arg = sub!(argx, two; "arg");
    let call_fib_x2 = call!(fib_f, arg; "fibx2");

    // fib(x-1)+fib(x-2)
    let sum = add!(call_fib_x1, call_fib_x2);

    // Create the return instruction and add it to the basic block
    builder <<= ret!(sum);

    return fib_f;
}

fn main() {
    pretty_env_logger::init();

    jit::target::NativeTarget::init().unwrap();
    jit::target::NativeAsmPrinter::init().unwrap();

    let context = Context::new();

    // Create some module to put our function into it.
    let module = context.create_module("test");

    // We are about to create the "fib" function:
    let fib_f = create_fib_function(&context, &module);

    //module.verify().unwrap();

    println!("We just constructed this LLVM module:\n\n{}\n\nRunning: ", module);

    // Now we create the JIT.
    let ee = ExecutionEngine::for_module(module).unwrap();

    let v = ee.run_function(&fib_f, vec![GenericValue::from_int(context.int32_t(), 32)])
        .to_int();

    // import result of execution
    println!("Result: {}", v);
}
