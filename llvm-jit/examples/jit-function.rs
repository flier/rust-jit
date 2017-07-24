extern crate pretty_env_logger;
extern crate llvm_jit as jit;

use jit::IntegerTypes;

fn main() {
    pretty_env_logger::init().unwrap();

    // Set up a context, module and builder in that context.
    let context = jit::Context::new();
    let module = jit::Module::with_context("sum", &context).unwrap();
    let builder = jit::Builder::with_context(&context);

    // get a type for sum function
    let i64t = context.int64();
    let mut argts = [i64t, i64t, i64t];
    let function_type = jit::Function::new(i64t, &argts, false);

    // add it to our module
    let function = module.add_function("sum", function_type).unwrap();

    // Create a basic block in the function and set our builder to generate code in it.
}
