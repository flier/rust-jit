use std::fmt;

use llvm::prelude::*;

pub struct BasicBlock(LLVMBasicBlockRef);

impl BasicBlock {
    /// Wrap a raw basic block reference.
    pub fn from_raw(block: LLVMBasicBlockRef) -> Self {
        BasicBlock(block)
    }

    /// Extracts the raw basic block reference.
    pub fn as_raw(&self) -> LLVMBasicBlockRef {
        self.0
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "block@{:?}", self.0)
    }
}
