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
mod builder;
mod block;
mod engine;
mod target;

pub use builder::{IRBuilder, InstructionBuilder, Position};
pub use context::Context;
pub use engine::{ExecutionEngine, Interpreter, MCJIT};
pub use module::{GlobalVar, Module, ThreadLocalMode};
pub use target::{AllAsmParsers, AllAsmPrinters, AllDisassemblers, AllTargetInfos, AllTargetMCs,
                 AllTargets, NativeAsmParser, NativeAsmPrinter, NativeDisassembler, NativeTarget};
pub use types::{ArrayType, FloatingPointType, FunctionType, IntegerType, OtherType, PointerType,
                StructType, TypeKind, TypeRef, VectorType};
pub use value::{BlockAddress, Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString,
                ConstantStruct, ConstantVector, Function, Instruction, ValueKind, ValueRef};

pub mod prelude {
    pub use types::{AsTypeRef, FloatingPointTypes, IntegerTypes, OtherTypes, ToArrayType,
                    ToPointerType, ToStructType, ToVectorType};
    pub use value::{ConstantFPs, ConstantInts, ConstantStrings, Constants, ToConstantArray,
                    ToConstantStruct};
}

pub mod ops {
    /// Terminators
    pub use builder::{AggregateRet, Br, CondBr, IndirectBr, Invoke, LandingPad, Resume, Ret,
                      RetVoid, Switch};

    /// Arithmetic
    pub use builder::{AShr, Add, And, ExactSDiv, ExactUDiv, FAdd, FDiv, FMul, FNeg, FRem, FSub,
                      LShr, Mul, NSWAdd, NSWMul, NSWNeg, NSWSub, NUWAdd, NUWMul, NUWNeg, NUWSub,
                      Neg, Not, Or, SDiv, SRem, Shl, Sub, UDiv, URem, Unreachable, Xor};

    /// Memory
    pub use builder::{Alloca, ArrayAlloca, ArrayMalloc, Free, Load, Malloc, Store};

    /// Casts
    pub use builder::{AddrSpaceCast, BitCast, FPCast, FPExt, FPToSI, FPToUI, FPTrunc, IntCast,
                      IntToPtr, PointerCast, PtrToInt, SExt, SExtOrBitCast, SIToFP, Trunc,
                      TruncOrBitCast, UIToFP, ZExt, ZExtOrBitCast};

    /// Miscellaneous
    pub use builder::{ExtractElement, ExtractValue, InsertElement, InsertValue, ShuffleVector};
}
