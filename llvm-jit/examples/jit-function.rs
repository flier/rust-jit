extern crate pretty_env_logger;
extern crate llvm_jit as jit;

use jit::IntegerTypes;

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
}
