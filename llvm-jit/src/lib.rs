#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy(conf_file=".clippy.toml")))]
#![cfg_attr(feature="clippy", allow(module_inception, block_in_if_condition_stmt))]

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
#[cfg(test)]
#[macro_use]
extern crate hamcrest;

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
mod engine;
pub mod target;

pub use constant::{Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString,
                   ConstantStruct, ConstantVector};
pub use context::{Context, GlobalContext};
pub use engine::{ExecutionEngine, Interpreter, MCJIT};
pub use function::{Function, FunctionType};
pub use global::GlobalVar;
pub use insts::{IRBuilder, Position};
pub use module::{AddressSpace, Module};
pub use types::{ArrayType, FloatingPointType, IntegerType, OtherType, PointerType, StructType,
                TypeKind, TypeRef, VectorType};
pub use utils::{AsBool, AsLLVMBool, AsResult, Boolinator};
pub use value::{BlockAddress, Instruction, ValueKind, ValueRef};

pub use insts::*;

pub mod prelude {
    pub use constant::{ConstantFPs, ConstantInts, ConstantStrings, Constants, ToConstantArray,
                       ToConstantStruct};
    pub use types::{AsTypeRef, FloatingPointTypes, IntegerTypes, OtherTypes, ToArrayType,
                    ToPointerType, ToStructType, ToVectorType};
}
