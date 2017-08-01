#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy(conf_file=".clippy.toml")))]
#![cfg_attr(feature="clippy", allow(module_inception, block_in_if_condition_stmt))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate bitflags;
extern crate hexplay;
extern crate libc;
extern crate llvm_sys as llvm;

#[cfg(test)]
extern crate pretty_env_logger;
#[cfg(test)]
extern crate tempfile;
#[cfg(test)]
extern crate mmap;
#[cfg(test)]
#[macro_use]
extern crate matches;
#[cfg(test)]
#[macro_use]
extern crate hamcrest;

#[macro_use]
pub mod errors;
#[macro_use]
mod utils;
mod context;
mod module;
#[macro_use]
mod types;
#[macro_use]
mod value;
#[macro_use]
mod constant;
mod global;
mod function;
#[macro_use]
pub mod insts;
mod block;
mod passmgr;
mod engine;
pub mod target;
mod analysis;
mod disasm;

pub use constant::{Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString,
                   ConstantStruct, ConstantVector};
pub use context::{Context, GlobalContext};
pub use disasm::Disasm;
pub use engine::{ExecutionEngine, GenericValue, Interpreter, JITCompiler, MCJIT, MCJITCompiler,
                 MCJITCompilerOptions, MCJITMemoryManager};
pub use function::{Function, FunctionType};
pub use global::GlobalVar;
pub use insts::{IRBuilder, Position};
pub use module::{AddressSpace, Module};
pub use passmgr::{Pass, PassManager, PassManagerBuilder, PassRegistry};
pub use types::{ArrayType, FloatingPointType, IntegerType, OtherType, PointerType, StructType,
                TypeKind, TypeRef, VectorType};
pub use utils::{AsBool, AsLLVMBool, AsResult, Boolinator};
pub use value::{BlockAddress, Instruction, ValueKind, ValueRef};

pub mod prelude {
    pub use constant::{ConstantFPs, ConstantInts, ConstantStrings, Constants, ToConstantArray,
                       ToConstantStruct};
    pub use types::{AsTypeRef, FloatingPointTypes, IntegerTypes, OtherTypes, ToArrayType,
                    ToPointerType, ToStructType, ToVectorType};
}
