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
mod builder;
mod block;
mod types;
mod value;
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
pub use value::{Constant, ConstantArray, ConstantFP, ConstantInt, ConstantString, ConstantStruct,
                ConstantVector, Function, Instruction, ValueKind, ValueRef};

pub mod prelude {
    pub use types::{AsTypeRef, FloatingPointTypes, IntegerTypes, OtherTypes, StructTypes};
    pub use value::{ConstantArrays, ConstantFPs, ConstantInts, ConstantStrings, ConstantStructs,
                    Constants};
}

pub mod ops {
    pub use builder::{AShr, Add, AggregateRet, And, Br, CondBr, ExactSDiv, ExactUDiv, FAdd, FDiv,
                      FMul, FNeg, FRem, FSub, LShr, Mul, NSWAdd, NSWMul, NSWNeg, NSWSub, NUWAdd,
                      NUWMul, NUWNeg, NUWSub, Neg, Not, Or, Ret, RetVoid, SDiv, SRem, Shl, Sub,
                      Switch, UDiv, URem, Unreachable, Xor, br, cond_br, ret, ret_void};
}
