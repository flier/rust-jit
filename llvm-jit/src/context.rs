use std::fmt;

use llvm::prelude::*;
use llvm::core::*;

/// Contexts are execution states for the core LLVM IR system.
pub struct Context(LLVMContextRef);

impl Context {
    /// Create a new context.
    pub fn new() -> Self {
        let context = unsafe { LLVMContextCreate() };

        trace!("create context@{:?}", context);

        Context(context)
    }

    /// Obtain the global context instance.
    pub fn global() -> Self {
        let context = unsafe { LLVMGetGlobalContext() };

        trace!("obtain global context@{:?}", context);

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
        trace!("drop context@{:?}", self.0);

        unsafe { LLVMContextDispose(self.0) }
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "context@{:?}", self.0)
    }
}
