use llvm::core::*;
use llvm::prelude::*;

/// Contexts are execution states for the core LLVM IR system.
#[derive(Debug)]
pub struct Context(LLVMContextRef);

impl Context {
    /// Create a new context.
    pub fn new() -> Self {
        let context = unsafe { LLVMContextCreate() };

        trace!("create Context({:?})", context);

        Context(context)
    }

    /// Obtain the global context instance.
    pub fn global() -> Self {
        let context = unsafe { LLVMGetGlobalContext() };

        trace!("obtain global Context({:?})", context);

        Context(context)
    }

    /// Wrap a raw context reference.
    pub fn from_raw(context: LLVMContextRef) -> Self {
        Context(context)
    }

    /// Extracts the raw context reference.
    pub fn as_raw(&self) -> LLVMContextRef {
        self.0
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMContextDispose(self.0) }
    }
}
