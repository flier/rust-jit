#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
extern crate llvm_jit as jit;

pub mod raw {
    #![allow(dead_code, non_camel_case_types, non_snake_case)]

    include!(concat!(env!("OUT_DIR"), "/raw.rs"));
}
mod ast;
mod codegen;
#[macro_use]
mod compile;
mod display;
mod errors;
mod parse;

pub use self::ast::{Cond, Inst, MiscOp, Mode, Op, Program, RVal, Size, Src};
pub use self::codegen::Filter;
pub use self::compile::compile;
pub use self::display::InstFmt;
