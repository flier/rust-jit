use std::fmt;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use value::{AsValueRef, Instruction};

pub trait InstructionBuilder {
    type Target: AsValueRef;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target;
}

#[derive(Debug)]
pub enum Position {
    To(BasicBlock, Instruction),
    Before(Instruction),
    AtEnd(BasicBlock),
}

/// An instruction builder represents a point within a basic block
/// and is the exclusive means of building instructions.
#[derive(Debug)]
pub struct IRBuilder(LLVMBuilderRef);

impl Default for IRBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for IRBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
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

    pub fn as_raw(&self) -> LLVMBuilderRef {
        self.0
    }

    pub fn insert_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetInsertBlock(self.0).as_mut() }.map(|block| BasicBlock::from_raw(block))
    }

    pub fn clear_insertion_position(&self) {
        unsafe { LLVMClearInsertionPosition(self.0) }
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

        self
    }

    pub fn emit<I: InstructionBuilder + fmt::Debug>(&self, inst: I) -> I::Target {
        trace!("{:?} emit: {:?}", self, inst);

        inst.emit_to(self)
    }
}
