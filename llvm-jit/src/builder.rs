use std::borrow::Cow;
use std::fmt;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

/// An instruction builder represents a point within a basic block and is the exclusive means of building instructions.
#[derive(Debug)]
pub struct IRBuilder(LLVMBuilderRef);

#[derive(Debug)]
pub enum Position {
    To(BasicBlock, Instruction),
    Before(Instruction),
    AtEnd(BasicBlock),
}

pub enum Inst<'a> {
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
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Inst::Add($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! sub {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Inst::Sub($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! mul {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Inst::Mul($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! udiv {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Inst::UDiv($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! sdiv {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::Inst::SDiv($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! ret {
    () => { $crate::Inst::ReturnVoid };
    ($result:expr) => { $crate::Inst::Return($result) };
}

impl<'a> fmt::Display for Inst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Inst::Add(lhs, rhs, ref name) => write!(f, "{} = add {}, {}", name, lhs, rhs),
            Inst::Sub(lhs, rhs, ref name) => write!(f, "{} = sub {}, {}", name, lhs, rhs),
            Inst::Mul(lhs, rhs, ref name) => write!(f, "{} = mul {}, {}", name, lhs, rhs),
            Inst::UDiv(lhs, rhs, ref name) => write!(f, "{} = udiv {}, {}", name, lhs, rhs),
            Inst::SDiv(lhs, rhs, ref name) => write!(f, "{} = sdiv {}, {}", name, lhs, rhs),
            Inst::ReturnVoid => write!(f, "return void"),
            Inst::Return(ret) => write!(f, "return {}", ret),
        }
    }
}

impl IRBuilder {
    /// Create a new IR builder in the global context.
    pub fn new() -> Self {
        let builder = unsafe { LLVMCreateBuilder() };

        trace!("create builder in global context: IRBuilder({:?})", builder);

        IRBuilder(builder)
    }

    /// Create a new IR builder in a specific context.
    pub fn within_context(context: &Context) -> Self {
        let builder = unsafe { LLVMCreateBuilderInContext(context.as_raw()) };

        trace!("create builder in {:?}: IRBuilder({:?})", context, builder);

        IRBuilder(builder)
    }

    pub fn insert_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetInsertBlock(self.0).as_mut() }.map(|block| BasicBlock::from_raw(block))
    }

    /// This specifies that created instructions should be inserted at the specified point.
    pub fn position(&self, position: Position) -> &Self {
        trace!("{:?} move position {:?}", self, position);

        unsafe {
            match position {
                Position::To(block, instr) => {
                    LLVMPositionBuilder(self.0, block.as_raw(), instr.as_raw())
                }
                Position::Before(instr) => LLVMPositionBuilderBefore(self.0, instr.as_raw()),
                Position::AtEnd(block) => LLVMPositionBuilderAtEnd(self.0, block.as_raw()),
            }
        }

        &self
    }

    pub fn emit(&self, inst: Inst) -> ValueRef {
        trace!("{:?} emit: {}", self, inst);

        let result = unsafe {
            match inst {
                Inst::Add(lhs, rhs, name) => {
                    LLVMBuildAdd(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Inst::Sub(lhs, rhs, name) => {
                    LLVMBuildSub(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Inst::Mul(lhs, rhs, name) => {
                    LLVMBuildMul(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Inst::UDiv(lhs, rhs, name) => {
                    LLVMBuildUDiv(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Inst::SDiv(lhs, rhs, name) => {
                    LLVMBuildSDiv(
                        self.0,
                        lhs.as_raw(),
                        rhs.as_raw(),
                        unchecked_cstring(name).as_ptr(),
                    )
                }
                Inst::ReturnVoid => LLVMBuildRetVoid(self.0),
                Inst::Return(ret) => LLVMBuildRet(self.0, ret.as_raw()),
            }
        };

        ValueRef::from_raw(result)
    }
}

impl Drop for IRBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}
