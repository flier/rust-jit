#[macro_use]
extern crate llvm_jit as jit;

use self::jit::prelude::*;

fn main() {
    pretty_env_logger::init();

    // Set up a context, module and builder in that context.
    let context = Context::new();
    let module = context.create_module("nop");

    // Get the type signature for void nop(void);
    // Then create it in our module.
    let void = context.void_t();
    let function_type = func!(|| -> void);
    let function = module.add_function("nop", function_type);

    // Create a basic block in the function and set our builder to generate code in it.
    let bb = function.append_basic_block_in_context("entry", &context);

    context.create_builder().within(bb, || {
        // Emit a `ret void` into the function
        ret!()
    });

    // Dump the module as IR to stdout.
    module.dump();
}
