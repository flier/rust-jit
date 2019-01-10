use std::fmt;
use std::ops;
use std::ptr;

use arrayvec::ArrayVec;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::block::BasicBlock;
use crate::context::{Context, GlobalContext};
use crate::errors::Result;
use crate::utils::{AsRaw, AsResult};
use crate::value::{Instruction, ValueRef};

pub trait InstructionBuilder {
    type Target;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target;
}

impl<T: Into<ValueRef>> InstructionBuilder for T {
    type Target = ValueRef;

    fn emit_to(self, _builder: &IRBuilder) -> Self::Target {
        self.into()
    }
}

impl<T: InstructionBuilder> InstructionBuilder for Option<T> {
    type Target = Option<T::Target>;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        self.map(|ib| ib.emit_to(builder))
    }
}

impl<T: InstructionBuilder> InstructionBuilder for Result<T> {
    type Target = Result<T::Target>;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        self.map(|ib| ib.emit_to(builder))
    }
}

macro_rules! array_impls {
    ($($len:tt)+) => {
        $(
            impl<T> InstructionBuilder for [T; $len]
            where
                T: Clone + InstructionBuilder,
                T::Target: fmt::Debug
            {
                type Target = [T::Target; $len];

                #[inline]
                fn emit_to(self, builder: &IRBuilder) -> Self::Target
                {
                    self.into_iter()
                        .map(|v| v.clone().emit_to(builder))
                        .collect::<ArrayVec<Self::Target>>()
                        .into_inner()
                        .unwrap()
                }
            }
        )+
    }
}

array_impls!(01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16
             17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32);

macro_rules! tuple_impls {
    ($($len:expr => ($($n:tt $name:ident)+))+) => {
        $(
            impl<$($name),+> InstructionBuilder for ($($name,)+)
            where
                $($name: InstructionBuilder,)+
            {
                type Target = ($($name::Target ,)+);

                #[inline]
                fn emit_to(self, builder: &IRBuilder) -> Self::Target
                {
                    (
                        $( self.$n.emit_to(builder) ,)+
                    )
                }
            }
        )+
    }
}

tuple_impls! {
    1 => (0 T0)
    2 => (0 T0 1 T1)
    3 => (0 T0 1 T1 2 T2)
    4 => (0 T0 1 T1 2 T2 3 T3)
    5 => (0 T0 1 T1 2 T2 3 T3 4 T4)
    6 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5)
    7 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6)
    8 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7)
    9 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8)
    10 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9)
    11 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10)
    12 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11)
    13 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12)
    14 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13)
    15 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14)
    16 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14 15 T15)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Position {
    To(BasicBlock, Option<Instruction>),
    Before(Instruction),
    AtEnd(BasicBlock),
}

/// An instruction builder represents a point within a basic block
/// and is the exclusive means of building instructions.
#[repr(transparent)]
#[derive(Debug)]
pub struct IRBuilder(LLVMBuilderRef);

inherit_from!(IRBuilder; LLVMBuilderRef);

impl Drop for IRBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}

impl IRBuilder {
    pub fn insert_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetInsertBlock(self.0) }.ok()
    }

    pub fn clear_insertion_position(&self) {
        unsafe { LLVMClearInsertionPosition(self.0) }
    }

    /// This specifies that created instructions should be inserted at the specified point.
    pub fn position(&self, position: Position) -> &Self {
        trace!("{:?} move position {:?}", self, position);

        match position {
            Position::To(block, instr) => self.position_to(block, instr),
            Position::Before(instr) => self.position_before(instr),
            Position::AtEnd(block) => self.position_at_end(block),
        }

        self
    }

    /// This specifies that created instructions should be inserted at the specified point.
    pub fn position_to(&self, block: BasicBlock, instr: Option<Instruction>) {
        unsafe {
            LLVMPositionBuilder(
                self.0,
                block.as_raw(),
                instr.map(|instr| instr.as_raw()).unwrap_or(ptr::null_mut()),
            )
        }
    }

    /// This specifies that created instructions should be inserted before the specified instruction.
    pub fn position_before(&self, instr: Instruction) {
        unsafe { LLVMPositionBuilderBefore(self.0, instr.as_raw()) }
    }

    /// This specifies that created instructions should be appended to the end of the specified block.
    pub fn position_at_end(&self, block: BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.0, block.as_raw()) }
    }

    pub fn within<F, I>(&self, block: BasicBlock, callback: F) -> I::Target
    where
        F: Fn() -> I,
        I: InstructionBuilder,
    {
        self.position_at_end(block);

        callback().emit_to(self)
    }

    pub fn emit<I: InstructionBuilder>(&self, inst: I) -> I::Target {
        inst.emit_to(self)
    }
}

impl<I: InstructionBuilder> ops::ShlAssign<I> for IRBuilder {
    fn shl_assign(&mut self, inst: I) {
        inst.emit_to(self);
    }
}

impl Context {
    /// Create a new IR builder in a specific context.
    pub fn create_builder(&self) -> IRBuilder {
        let builder = unsafe { LLVMCreateBuilderInContext(self.as_raw()) }.into();

        trace!("create builder in {:?}: {:?}", self, builder);

        builder
    }
}

impl GlobalContext {
    /// Create a new IR builder in the global context.
    pub fn create_builder() -> IRBuilder {
        let builder = unsafe { LLVMCreateBuilder() }.into();

        trace!("create builder in global context: {:?}", builder);

        builder
    }
}
