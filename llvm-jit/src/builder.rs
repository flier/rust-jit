use llvm::prelude::*;
use llvm::core::*;

use errors::Result;
use context::Context;

/// An instruction builder represents a point within a basic block and is the exclusive means of building instructions.
pub struct Builder(LLVMBuilderRef);

impl Builder {
    pub fn new() -> Self {
        let builder = unsafe { LLVMCreateBuilder() };

        trace!("create builder #{:?} in global context", builder);

        Builder(builder)
    }

    pub fn with_context(context: &Context) -> Result<Self> {
        let context = context.as_raw();
        let builder = unsafe { LLVMCreateBuilderInContext(context) };

        trace!("create builder #{:?} in context #{:?}", builder, context);

        Ok(Builder(builder))
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        trace!("drop builder #{:?}", self.0);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}
