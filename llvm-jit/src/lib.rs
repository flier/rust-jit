#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys as llvm;

pub mod errors;
mod context;
mod module;
mod builder;
mod types;
mod block;

pub use context::Context;
pub use module::Module;
pub use builder::Builder;
pub use types::{TypeRef, IntegerTypes, IntegerType, Function, int1, int8, int16, int32, int64,
                int128, int_type};
