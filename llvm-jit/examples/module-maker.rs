extern crate pretty_env_logger;
#[macro_use]
extern crate llvm_jit as jit;

use jit::prelude::*;

fn main() {
    pretty_env_logger::init().unwrap();

    let context = Context::new();

    // Create the "module" or "program" or "translation unit" to hold the function
    let m = context.create_module("test");

    // Create the main function: first create the type 'int ()'
    let i32_t = context.int32_t();
    let ft = FunctionType::new(i32_t, &[], false);

    // By passing a module as the last parameter to the Function constructor,
    // it automatically gets appended to the Module.
    let f = m.add_function("main", ft);

    // Add a basic block to the function... again, it automatically inserts
    // because of the last argument.
    let bb = f.append_basic_block_in_context("EntryBlock", &context);
    let mut builder = context.create_builder();
    builder.position_at_end(bb);

    // Get the constant integers...
    let two = i32_t.int(2);
    let three = i32_t.int(3);

    // Create the add instruction... does not insert...
    let add = add!(two, three; "addresult");

    builder <<= ret!(add);

    m.dump();
}
