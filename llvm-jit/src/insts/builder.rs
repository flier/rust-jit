use std::ops;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::{Context, GlobalContext};
use utils::{AsRaw, FromRaw};
use value::{Instruction, ValueRef};

pub trait InstructionBuilder {
    type Target: Into<ValueRef>;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target;
}

impl<T: Into<ValueRef>> InstructionBuilder for T {
    type Target = ValueRef;

    fn emit_to(self, _builder: &IRBuilder) -> Self::Target {
        self.into()
    }
}

#[derive(Debug)]
pub enum Position {
    To(BasicBlock, Option<Instruction>),
    Before(Instruction),
    AtEnd(BasicBlock),
}

/// An instruction builder represents a point within a basic block
/// and is the exclusive means of building instructions.
#[derive(Debug)]
pub struct IRBuilder(LLVMBuilderRef);

inherit_from!(IRBuilder, LLVMBuilderRef);

impl Drop for IRBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}

impl IRBuilder {
    pub fn insert_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetInsertBlock(self.0) }.wrap()
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
