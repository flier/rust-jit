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
extern crate pretty_env_logger;
extern crate crossbeam;
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_sys as llvm;

use std::mem;
use std::sync::{Arc, Barrier};

use jit::{Context, ExecutionEngine, Function, GenericValue, IRBuilder, Module, Position};
use jit::insts::*;
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
    let builder = IRBuilder::within_context(&context);
    builder.position(Position::AtEnd(bb));

    // Get the constant `1'.
    let one = i32_t.int(1);

    // Get the integer argument of the add1 function...
    assert!(add1_f.params().count() > 0); // Make sure there's an arg
    let argx = add1_f.params().next().unwrap(); // Get the arg
    argx.set_name("AnArg"); // Give it a nice symbolic name for fun.

    // Create the add instruction, inserting it into the end of BB.
    let add = add!(one, argx; "addresult").emit_to(&builder);

    // Create the return instruction and add it to the basic block
    ret!(add).emit_to(&builder);

    add1_f
}

fn create_fib_function(context: &Context, module: &Module) -> Function {
    // Create the fib function and insert it into module M.  This function is said
    // to return an int and take an int parameter.
    let i32_t = context.int32_t();
    let fib_f = module.get_or_insert_function("fib", i32_t, &[i32_t]);

    // Add a basic block to the function.
    let bb = fib_f.append_basic_block_in_context("entry", &context);

    // Create a basic block builder with default parameters.
    let builder = IRBuilder::within_context(&context);
    builder.position(Position::AtEnd(bb));

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
    let cond_inst = icmp!(sle argx, two; "cond").emit_to(&builder);
    br!(
        cond_inst => ret_bb,
        _ => recurse_bb
    ).emit_to(&builder);

    // Create: ret int 1
    builder.position(jit::Position::AtEnd(ret_bb));

    ret!(one).emit_to(&builder);

    // create fib(x-1)
    builder.position(jit::Position::AtEnd(recurse_bb));

    let sub = sub!(argx, one; "arg").emit_to(&builder);
    let call_fib_x1 = call!(fib_f, sub; "fibx1").emit_to(&builder);

    // create fib(x-2)
    let sub = sub!(argx, two; "arg").emit_to(&builder);
    let call_fib_x2 = call!(fib_f, sub; "fibx2").emit_to(&builder);

    // fib(x-1)+fib(x-2)
    let sum = add!(call_fib_x1, call_fib_x2).emit_to(&builder);

    // Create the return instruction and add it to the basic block
    ret!(sum).emit_to(&builder);

    return fib_f;
}

fn main() {
    pretty_env_logger::init().unwrap();

    jit::MCJIT::link_in();
    jit::target::AllTargetMCs::init();

    let context = Context::new();

    // Create some module to put our function into it.
    let module = Module::with_name_in_context("test", &context);

    let add1_f = create_add1(&context, &module);
    let fib_f = create_fib_function(&context, &module);

    println!(
        "We just constructed this LLVM module:\n\n{}\n\nRunning: ",
        module
    );

    // Now we create the JIT.
    let ee = ExecutionEngine::for_module(module).unwrap();

    let i32_t = context.int32_t();

    let addr = ee.get_function_address("add1");

    println!("add1 @ {:?}", addr);

    let add1: extern "C" fn(i32) -> i32 = unsafe { mem::transmute(addr.unwrap()) };

    println!("add1(1000) = {}", add1(1000));

    let addr = ee.get_function_address("fib").unwrap();

    let fib: extern "C" fn(i32) -> i32 = unsafe { mem::transmute(addr) };

    println!("fib(1000) = {}", fib(1000));

    let barrier = Arc::new(Barrier::new(3));

    crossbeam::scope(|scope| {
        let add1_thread = scope.spawn(|| {
            barrier.clone().wait();

            println!("add1(1000)");

            let v = ee.run_function(add1_f, &[GenericValue::from_int(i32_t, 1000)])
                .to_int();

            v
        });
        let fib_thread1 = scope.spawn(|| {
            barrier.clone().wait();

            println!("fib(39)");

            let v = ee.run_function(fib_f, &[GenericValue::from_int(i32_t, 39)])
                .to_int();

            v
        });
        let fib_thread2 = scope.spawn(|| {
            barrier.clone().wait();

            println!("fib(42)");

            let v = ee.run_function(fib_f, &[GenericValue::from_int(i32_t, 42)])
                .to_int();

            v
        });

        println!("Add1 returned {}", add1_thread.join());
        println!("Fib1 returned {}", fib_thread1.join());
        println!("Fib2 returned {}", fib_thread2.join());
    });
}
