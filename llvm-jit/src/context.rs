use llvm::prelude::*;
use llvm::core::*;

/// Contexts are execution states for the core LLVM IR system.
pub type Context(LLVMContextRef);

impl Context {
    pub fn wrap(context: LLVMContextRef) -> Self {
        Context(context)
    }

    /// Create a new context.
    pub fn new() -> Self {
        let context = unsafe { LLVMContextCreate() };

        trace!("create context #{:?}", context);

        Context(context)
    }

    /// Obtain the global context instance.
    pub fn global() -> Self {
        let context = unsafe { LLVMGetGlobalContext() };

        trace!("obtain global context #{:?}", context);

        Context(context)
    }

    /// Extracts the raw context reference.
    pub fn as_raw(&self) -> LLVMContextRef {
        self.0
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        trace!("drop context #{:?}", self.0);

        unsafe { LLVMContextDispose(self.0) }
    }
}
