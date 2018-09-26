#![recursion_limit = "128"]
#![cfg_attr(feature = "cargo-clippy", feature(tool_lints))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]

#[macro_use]
extern crate bitflags;
extern crate boolinator;
#[macro_use]
extern crate failure;
extern crate hexplay;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate llvm_sys;
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
#[cfg(test)]
extern crate mmap;
#[cfg(test)]
extern crate pretty_env_logger;
#[cfg(test)]
extern crate tempfile;

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
mod debuginfo;
mod disasm;
mod engine;
mod membuf;
mod metadata;
mod object;
mod orc;
mod passmgr;
mod symbols;
pub mod target;

pub use attribute::{attrs, Attribute, AttributeIndex, EnumAttribute, StringAttribute};
pub use constant::{
    Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString, ConstantStruct, ConstantVector, InlineAsm,
};
pub use context::{Context, GlobalContext};
pub use debuginfo::{debug_metadata_version, DIBuilder};
pub use disasm::Disasm;
pub use engine::{
    shutdown, CodeGenOptLevel, ExecutionEngine, GenericValue, Interpreter, JITCompiler, MCJITCompiler,
    MCJITCompilerOptions, MCJITMemoryManager, MCJIT,
};
pub use function::{Function, FunctionType};
pub use global::{DLLStorageClass, GlobalValue, GlobalVar, Linkage, ThreadLocalMode, Visibility};
pub use insts::{IRBuilder, Position};
pub use membuf::MemoryBuffer;
pub use metadata::{MDKindId, MDNode, MDString, Metadata};
pub use module::{AddressSpace, Module};
pub use object::{ObjectFile, Section, SectionIter, Symbol, SymbolIter};
pub use orc::{JITStack, LazyCompileCallback, ModuleHandle, SymbolResolver, TargetAddress};
pub use passmgr::{FunctionPassManager, Pass, PassManager, PassManagerBuilder, PassRegistry};
pub use symbols::Symbols;
pub use types::{
    ArrayType, FloatingPointType, IntegerType, OtherType, PointerType, StructType, TypeKind, TypeRef, VectorType,
};
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
    pub use constant::{
        ConstantFPs, ConstantInts, ConstantStrings, Constants, ToConstantArray, ToConstantStruct, ToConstantVector,
        ToNamedConstantStruct,
    };
    pub use constexpr::ConstantExpr;
    pub use global::GlobalValue;
    pub use insts::InstructionBuilder;
    pub use intrinsics::IntrinsicId;
    pub use types::{
        AsTypeRef, FloatingPointTypes, IntegerTypes, OtherTypes, ToArrayType, ToPointerType, ToStructType, ToVectorType,
    };
    pub use utils::{AsRaw, IntoRaw};
    pub use value::{AsValueRef, Instruction};
}

use std::sync::{Once, ONCE_INIT};

static INIT: Once = ONCE_INIT;

pub fn init() {
    use llvm::initialization::*;

    use utils::AsRaw;

    INIT.call_once(|| {
        let _ = Context::global();

        PassRegistry::global().with(|p| unsafe {
            LLVMInitializeCore(p);
            LLVMInitializeTransformUtils(p);
            LLVMInitializeScalarOpts(p);
            LLVMInitializeObjCARCOpts(p);
            LLVMInitializeVectorization(p);
            LLVMInitializeInstCombine(p);;
            LLVMInitializeIPO(p);
            LLVMInitializeInstrumentation(p);
            LLVMInitializeAnalysis(p);
            LLVMInitializeCodeGen(p);
            LLVMInitializeTarget(p);
        })
    })
}
