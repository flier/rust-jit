#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate llvm_jit as jit;

mod raw {
    #![allow(dead_code, non_camel_case_types, non_snake_case)]

    include!(concat!(env!("OUT_DIR"), "/raw.rs"));
}
mod ast;
mod codegen;
mod compile;
mod display;
mod errors;

pub use ast::{Cond, Inst, MiscOp, Mode, Op, Program, RVal, Size, Src};
pub use codegen::Filter;
pub use compile::compile;
