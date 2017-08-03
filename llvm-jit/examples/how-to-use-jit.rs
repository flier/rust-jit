//===----------------------------------------------------------------------===//
//
//  This small program provides an example of how to quickly build a small
//  module with two functions and execute it with the JIT.
//
// Goal:
//  The goal of this snippet is to create in the memory
//  the LLVM module consisting of two functions as follow:
//
// int add1(int x) {
//   return x+1;
// }
//
// int foo() {
//   return add1(10);
// }
//
// then compile the module via JIT, then execute the `foo'
// function and return result to a driver, i.e. to a "host program".
//
// Some remarks and questions:
//
// - could we invoke some code using noname functions too?
//   e.g. evaluate "foo()+foo()" without fears to introduce
//   conflict of temporary function name with some real
//   existing function name?
//
//===----------------------------------------------------------------------===//
#[macro_use]
extern crate llvm_jit as jit;

use jit::insts::*;
use jit::prelude::*;

fn main() {
    jit::target::AllTargetMCs::init();

    let context = jit::Context::new();

    // Create some module to put our function into it.
    let m = jit::Module::with_name_in_context("test", &context);

    // Create the add1 function entry and insert this entry into module M.  The
    // function will have a return type of "int" and take an argument of "int".
    let i32_t = context.int32_t();
    let add1_f = m.get_or_insert_function("add1", i32_t, &[i32_t]);

    // Add a basic block to the function.
    let bb = add1_f.append_basic_block_in_context("EntryBlock", &context);

    // Create a basic block builder with default parameters.
    let builder = jit::IRBuilder::within_context(&context);
    builder.position(jit::Position::AtEnd(bb));

    // Get the constant `1'.
    let one = i32_t.int(1);

    // Get the integer argument of the add1 function...
    assert!(add1_f.params().count() > 0); // Make sure there's an arg
    let argx = add1_f.get_param(0).unwrap(); // Get the arg
    argx.set_name("AnArg"); // Give it a nice symbolic name for fun.

    // Create the add instruction, inserting it into the end of BB.
    let add = add!(one, argx).emit_to(&builder);

    // Create the return instruction and add it to the basic block
    ret!(add).emit_to(&builder);

    // Now, function add1 is ready.

    // Now we're going to create function `foo', which returns an int and takes no arguments.
    let foo_f = m.get_or_insert_function("foo", i32_t, &[]);

    // Add a basic block to the `foo` function.
    let bb = foo_f.append_basic_block_in_context("EntryBlock", &context);

    // Tell the basic block builder to attach itself to the new basic block
    builder.position(jit::Position::AtEnd(bb));

    // Get the constant `10'.
    let ten = i32_t.int(10);

    // Pass Ten to the call to `add1_f`
    let add1_call_res = call!(add1_f, ten).emit_to(&builder);
    add1_call_res.set_tail_call(true);

    // Create the return instruction and add it to the basic block.
    ret!(add1_call_res).emit_to(&builder);

    println!(
        "We just constructed this LLVM module:\n\n{}\n\nRunning foo: ",
        m
    );

    // Now we create the JIT.
    let ee = jit::ExecutionEngine::for_module(m).unwrap();

    // Call the `foo' function with no arguments:
    let noargs = &[];
    let gv = ee.run_function(foo_f, noargs);

    println!("Result: {}\n", gv.to_int());

    jit::shutdown();
}
