//===----------------------------------------------------------------------===//
//
// Parallel JIT
//
// This test program creates two LLVM functions then calls them from three
// separate threads.  It requires the pthreads library.
// The three threads are created and then block waiting on a condition variable.
// Once all threads are blocked on the conditional variable, the main thread
// wakes them up. This complicated work is performed so that all three threads
// call into the JIT at the same time (or the best possible approximation of the
// same time). This test had assertion errors until I got the locking right.
//
//===----------------------------------------------------------------------===//
extern crate crossbeam;
#[macro_use]
extern crate llvm_jit as jit;
extern crate pretty_env_logger;

use std::sync::{Arc, Barrier};

use jit::prelude::*;

fn create_add1(context: &Context, module: &Module) -> Function {
    // Create the add1 function entry and insert this entry into module M.  The
    // function will have a return type of "int" and take an argument of "int".
    // The '0' terminates the list of argument types.
    let i32_t = context.int32_t();
    let add1_f = module.get_or_insert_function("add1", i32_t, &[i32_t]);

    // Add a basic block to the function.
    let bb = add1_f.append_basic_block_in_context("entry", &context);

    // Create a basic block builder with default parameters.
    let mut builder = context.create_builder();
    builder.position_at_end(bb);

    // Get the constant `1'.
    let one = i32_t.int(1);

    // Get the integer argument of the add1 function...
    assert!(add1_f.params().count() > 0); // Make sure there's an arg
    let argx = add1_f.params().next().unwrap(); // Get the arg
    argx.set_name("AnArg"); // Give it a nice symbolic name for fun.

    // Create the add instruction, inserting it into the end of BB.
    let add = add!(one, argx; "addresult");

    // Create the return instruction and add it to the basic block
    builder <<= ret!(add);

    add1_f
}

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

    let add1_f = create_add1(&context, &module);
    let fib_f = create_fib_function(&context, &module);

    println!("We just constructed this LLVM module:\n\n{}\n\nRunning: ", module);

    // Now we create the JIT.
    let ee = ExecutionEngine::for_module(module).unwrap();

    let i32_t = context.int32_t();

    let barrier = Arc::new(Barrier::new(3));

    crossbeam::scope(|scope| {
        let add1_thread = scope.spawn(|| {
            barrier.clone().wait();

            let v = ee
                .run_function(&add1_f, vec![GenericValue::from_int(i32_t, 1000)])
                .to_int();

            v
        });
        let fib_thread1 = scope.spawn(|| {
            barrier.clone().wait();

            let v = ee
                .run_function(&fib_f, vec![GenericValue::from_int(i32_t, 39)])
                .to_int();

            v
        });
        let fib_thread2 = scope.spawn(|| {
            barrier.clone().wait();

            let v = ee
                .run_function(&fib_f, vec![GenericValue::from_int(i32_t, 42)])
                .to_int();

            v
        });

        println!("Add1(1000) returned {}", add1_thread.join().unwrap());
        println!("Fib1(39) returned {}", fib_thread1.join().unwrap());
        println!("Fib2(42) returned {}", fib_thread2.join().unwrap());
    });
}
