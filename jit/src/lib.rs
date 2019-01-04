#![recursion_limit = "128"]
#![allow(clippy::zero_prefixed_literal, clippy::trivially_copy_pass_by_ref)]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;
#[macro_use(
    call,
    do_parse,
    error_position,
    error_node_position,
    named,
    named_args,
    many_m_n,
    many_till,
    map,
    switch,
    value
)]
extern crate nom;

#[doc(hidden)]
pub mod llvm {
    pub use llvm_sys::*;
}

#[cfg(test)]
#[macro_use]
extern crate hamcrest;
#[cfg(test)]
#[macro_use]
extern crate matches;

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
mod attribute;
mod constexpr;
mod function;
mod global;
mod intrinsics;
#[macro_use]
pub mod insts;
mod analysis;
mod bitcode;
mod block;
#[macro_use]
pub mod debuginfo;
mod disasm;
pub mod engine;
mod inlineasm;
mod membuf;
mod metadata;
mod object;
mod orc;
mod passmgr;
mod symbols;
pub mod target;

pub use crate::attribute::{attrs, Attribute, AttributeIndex, EnumAttribute, StringAttribute};
pub use crate::constant::{
    Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString, ConstantStruct, ConstantVector, InlineAsm,
};
pub use crate::context::{Context, GlobalContext};
pub use crate::disasm::Disasm;
pub use crate::engine::{
    shutdown, CodeGenOptLevel, ExecutionEngine, GenericValue, Interpreter, JITCompiler, MCJITCompiler,
    MCJITCompilerOptions, MCJITMemoryManager, MCJIT,
};
pub use crate::function::{Function, FunctionType};
pub use crate::global::{DLLStorageClass, GlobalValue, GlobalVar, Linkage, ThreadLocalMode, Visibility};
pub use crate::insts::{IRBuilder, Position};
pub use crate::membuf::MemoryBuffer;
pub use crate::metadata::{MDKindId, MDNode, MDString, Metadata, ModuleFlagEntry, ModuleFlagsMetadata};
pub use crate::module::{AddressSpace, Module};
pub use crate::object::{ObjectFile, Section, SectionIter, Symbol, SymbolIter};
pub use crate::orc::{JITStack, LazyCompileCallback, ModuleHandle, SymbolResolver, TargetAddress};
pub use crate::passmgr::{FunctionPassManager, Pass, PassManager, PassManagerBuilder, PassRegistry};
pub use crate::symbols::Symbols;
pub use crate::types::{
    ArrayType, FloatingPointType, IntegerType, OtherType, PointerType, StructType, TypeKind, TypeRef, VectorType,
};
pub use crate::value::{BlockAddress, Instruction, Opcode, ValueKind, ValueRef};

pub mod prelude {
    //! A prelude for writing LLVM JIT code.

    pub use crate::block::BasicBlock;
    pub use crate::context::Context;
    pub use crate::engine::{ExecutionEngine, GenericValue};
    pub use crate::function::{Function, FunctionType};
    pub use crate::insts::{IRBuilder, Position};
    pub use crate::module::Module;
    pub use crate::types::TypeRef;
    pub use crate::value::ValueRef;

    pub use crate::attribute::AttributeGroups;
    pub use crate::constant::{
        ConstantFP, ConstantFPs, ConstantInt, ConstantInts, ConstantString, ConstantStrings, Constants,
        ToConstantArray, ToConstantStruct, ToConstantVector, ToNamedConstantStruct,
    };
    pub use crate::constexpr::ConstantExpr;
    pub use crate::global::GlobalValue;
    pub use crate::insts::InstructionBuilder;
    pub use crate::intrinsics::IntrinsicId;
    pub use crate::target::{targets, Target, TargetMachine};
    pub use crate::types::{
        ArrayType, AsTypeRef, FloatingPointTypes, IntegerType, IntegerTypes, OtherType, OtherTypes, PointerType,
        StructType, ToArrayType, ToPointerType, ToStructType, ToVectorType, VectorType,
    };
    pub use crate::utils::{AsRaw, IntoRaw};
    pub use crate::value::{AsValueRef, Instruction};
}
