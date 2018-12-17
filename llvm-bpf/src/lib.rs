#[macro_use]
extern crate failure;
#[macro_use]
extern crate llvm_jit as jit;

mod ast;
mod raw {
    #![allow(dead_code, non_camel_case_types, non_snake_case)]

    include!(concat!(env!("OUT_DIR"), "/raw.rs"));
}
