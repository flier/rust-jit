use std::borrow::Cow;
use std::fmt;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use types::ValueRef;
use utils::unchecked_cstring;

/// An instruction builder represents a point within a basic block and is the exclusive means of building instructions.
#[derive(Debug)]
pub struct Builder(LLVMBuilderRef);

#[derive(Debug)]
pub enum Position {
    AtEnd(BasicBlock),
}

pub enum Instruction<'a> {
    Add(ValueRef, ValueRef, Cow<'a, str>),
    Sub(ValueRef, ValueRef, Cow<'a, str>),
    Mul(ValueRef, ValueRef, Cow<'a, str>),
    UDiv(ValueRef, ValueRef, Cow<'a, str>),
    SDiv(ValueRef, ValueRef, Cow<'a, str>),
    ReturnVoid,
    Return(ValueRef),
}

#[macro_export]
macro_rules! add {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Instruction::Add($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! sub {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Instruction::Sub($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! mul {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Instruction::Mul($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! udiv {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Instruction::UDiv($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! sdiv {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Instruction::SDiv($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! ret {
    () => { $crate::Instruction::ReturnVoid };
    ($result:expr) => { $crate::Instruction::Return($result) };
}

impl<'a> fmt::Display for Instruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Add(lhs, rhs, ref name) => write!(f, "{} = add {}, {}", name, lhs, rhs),
            Instruction::Sub(lhs, rhs, ref name) => write!(f, "{} = sub {}, {}", name, lhs, rhs),
            Instruction::Mul(lhs, rhs, ref name) => write!(f, "{} = mul {}, {}", name, lhs, rhs),
            Instruction::UDiv(lhs, rhs, ref name) => write!(f, "{} = udiv {}, {}", name, lhs, rhs),
            Instruction::SDiv(lhs, rhs, ref name) => write!(f, "{} = sdiv {}, {}", name, lhs, rhs),
            Instruction::ReturnVoid => write!(f, "return void"),
            Instruction::Return(ret) => write!(f, "return {}", ret),
        }
    }
}

impl Builder {
    pub fn new() -> Self {
        let builder = unsafe { LLVMCreateBuilder() };

        trace!("create builder in global context: Builder({:?})", builder);

        Builder(builder)
    }

    pub fn with_context(context: &Context) -> Self {
        let builder = unsafe { LLVMCreateBuilderInContext(context.as_raw()) };

        trace!("create builder in {:?}: Builder({:?})", context, builder);

        Builder(builder)
    }

    pub fn position(&self, position: Position) -> &Self {
        trace!("{:?} move position {:?}", self, position);

        unsafe {
            match position {
                Position::AtEnd(block) => LLVMPositionBuilderAtEnd(self.0, block.as_raw()),
            }
        }

        &self
    }

    pub fn emit(&self, instruction: Instruction) -> ValueRef {
        trace!("{:?} emit: {}", self, instruction);

        let result = unsafe {
            match instruction {
                Instruction::Add(lhs, rhs, name) => {
                    LLVMBuildAdd(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Instruction::Sub(lhs, rhs, name) => {
                    LLVMBuildSub(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Instruction::Mul(lhs, rhs, name) => {
                    LLVMBuildMul(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Instruction::UDiv(lhs, rhs, name) => {
                    LLVMBuildUDiv(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Instruction::SDiv(lhs, rhs, name) => {
                    LLVMBuildSDiv(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Instruction::ReturnVoid => LLVMBuildRetVoid(self.0),
                Instruction::Return(ret) => LLVMBuildRet(self.0, ret.as_raw()),
            }
        };

        ValueRef::from_raw(result)
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}
