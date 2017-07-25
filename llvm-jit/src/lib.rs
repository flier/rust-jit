#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys as llvm;

#[cfg(test)]
extern crate tempfile;
#[cfg(test)]
#[macro_use]
extern crate matches;

pub mod errors;
mod context;
mod module;
#[macro_use]
mod builder;
mod block;
mod types;
mod value;
#[macro_use]
mod utils;
mod engine;
mod target;

pub use builder::{Builder, Instruction, Position};
pub use context::Context;
pub use engine::{ExecutionEngine, Interpreter, MCJIT};
pub use module::Module;
pub use target::{AllAsmParsers, AllAsmPrinters, AllDisassemblers, AllTargetInfos, AllTargetMCs,
                 AllTargets, NativeAsmParser, NativeAsmPrinter, NativeDisassembler, NativeTarget};
pub use types::{FloatingPointType, FunctionType, IntegerType, OtherType, TypeKind, TypeRef};
pub use value::{Function, ValueKind, ValueRef};

pub mod prelude {
    pub use types::{FloatingPointTypes, IntegerTypes, OtherTypes};
}
