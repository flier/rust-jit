use std::fmt;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::{Context, GlobalContext};
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

inherit_from!(IRBuilder, LLVMBuilderRef);

impl Drop for IRBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}

impl IRBuilder {
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
        inst.emit_to(self)
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
