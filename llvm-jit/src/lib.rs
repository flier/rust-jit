#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys as llvm;

pub mod errors;
mod context;
mod module;
mod builder;
mod block;
mod types;
mod utils;

pub use context::Context;
pub use module::Module;
pub use builder::{Position, Builder};
pub use types::{TypeRef, IntegerTypes, IntegerType, FunctionType, int1, int8, int16, int32, int64,
                int128, int_type};
