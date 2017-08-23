use llvm::orc::*;
use llvm::prelude::*;

#[derive(Debug)]
pub struct JITStack(LLVMOrcJITStackRef);
