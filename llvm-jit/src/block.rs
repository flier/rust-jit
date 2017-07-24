use llvm::prelude::*;

#[derive(Debug)]
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
