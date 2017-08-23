#![recursion_limit="128"]

#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy(conf_file=".clippy.toml")))]
#![cfg_attr(feature="clippy", allow(module_inception, block_in_if_condition_stmt))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate bitflags;
extern crate hexplay;
extern crate libc;
extern crate llvm_sys;

#[doc(hidden)]
pub mod llvm {
    pub use llvm_sys::*;
}

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

#[doc(hidden)]
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
mod constexpr;
mod attribute;
mod global;
mod function;
mod intrinsics;
#[macro_use]
pub mod insts;
mod block;
mod passmgr;
mod engine;
mod orc;
pub mod target;
mod analysis;
mod disasm;
mod membuf;
mod object;
mod bitcode;

pub use attribute::{Attribute, AttributeIndex, EnumAttribute, StringAttribute, attrs};
pub use constant::{Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString,
                   ConstantStruct, ConstantVector};
pub use context::{Context, GlobalContext};
pub use disasm::Disasm;
pub use engine::{ExecutionEngine, GenericValue, Interpreter, JITCompiler, MCJIT, MCJITCompiler,
                 MCJITCompilerOptions, MCJITMemoryManager, shutdown};
pub use function::{Function, FunctionType};
pub use global::{DLLStorageClass, GlobalValue, GlobalVar, Linkage, ThreadLocalMode, Visibility};
pub use insts::{IRBuilder, Position};
pub use intrinsics::{Intrinsic, IntrinsicId};
pub use membuf::MemoryBuffer;
pub use module::{AddressSpace, Module};
pub use object::{ObjectFile, Section, SectionIter, Symbol, SymbolIter};
pub use orc::{JITStack, LazyCompileCallback, SymbolResolver, ModuleHandle, TargetAddress};
pub use passmgr::{FunctionPassManager, Pass, PassManager, PassManagerBuilder, PassRegistry};
pub use types::{ArrayType, FloatingPointType, IntegerType, OtherType, PointerType, StructType,
                TypeKind, TypeRef, VectorType};
pub use value::{BlockAddress, Instruction, Opcode, ValueKind, ValueRef};

pub mod prelude {
    //! A prelude for writing LLVM JIT code.

    pub use block::BasicBlock;
    pub use context::Context;
    pub use engine::{ExecutionEngine, GenericValue};
    pub use function::{Function, FunctionType};
    pub use insts::{IRBuilder, Position};
    pub use module::Module;
    pub use types::TypeRef;
    pub use value::ValueRef;

    pub use attribute::AttributeGroups;
    pub use constant::{ConstantFPs, ConstantInts, ConstantStrings, Constants, ToConstantArray,
                       ToConstantStruct, ToConstantVector};
    pub use constexpr::ConstantExpr;
    pub use global::GlobalValue;
    pub use insts::InstructionBuilder;
    pub use intrinsics::Intrinsics;
    pub use types::{AsTypeRef, FloatingPointTypes, IntegerTypes, OtherTypes, ToArrayType,
                    ToPointerType, ToStructType, ToVectorType};
    pub use utils::{AsRaw, IntoRaw};
    pub use value::AsValueRef;
}
