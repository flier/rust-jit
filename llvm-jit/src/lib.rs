#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys as llvm;

pub mod errors;
mod context;
mod module;
#[macro_use]
mod builder;
mod block;
mod types;
mod utils;
mod engine;
mod target;

pub use builder::{Builder, Instruction, Position};
pub use context::Context;
pub use engine::{ExecutionEngine, Interpreter, MCJIT};
pub use module::Module;
pub use target::{AllAsmParsers, AllAsmPrinters, AllDisassemblers, AllTargetInfos, AllTargetMCs,
                 AllTargets, NativeAsmParser, NativeAsmPrinter, NativeDisassembler, NativeTarget};
pub use types::{FloatingPointType, FunctionType, IntegerType, OtherType, TypeRef, double, float,
                fp128, half, int1, int128, int16, int32, int64, int8, int_type, label, ppc_fp128,
                void, x86_fp80, x86mmx};

pub mod prelude {
    pub use types::FloatingPointTypes;
    pub use types::IntegerTypes;
    pub use types::OtherTypes;
}
