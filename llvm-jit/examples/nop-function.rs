extern crate pretty_env_logger;
#[macro_use]
extern crate llvm_jit as jit;

use jit::prelude::*;

fn main() {
    pretty_env_logger::init().unwrap();

    // Set up a context, module and builder in that context.
    let context = jit::Context::new();
    let module = jit::Module::with_name_in_context("nop", &context);
    let builder = jit::IRBuilder::within_context(&context);

    // Get the type signature for void nop(void);
    // Then create it in our module.
    let void = context.void();
    let function_type = jit::FunctionType::new(void, &[], false);
    let function = module.add_function("nop", function_type);

    // Create a basic block in the function and set our builder to generate code in it.
    let bb = function.append_basic_block(&context, "entry");
    builder.position(jit::Position::AtEnd(bb));

    // Emit a `ret void` into the function
    builder.emit(ret!());

    // Dump the module as IR to stdout.
    module.dump();
}
