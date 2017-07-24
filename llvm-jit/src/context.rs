use llvm::core::*;
use llvm::prelude::*;

/// Contexts are execution states for the core LLVM IR system.
#[derive(Debug, PartialEq)]
pub struct Context(LLVMContextRef, bool);

impl Context {
    /// Create a new context.
    pub fn new() -> Self {
        let context = unsafe { LLVMContextCreate() };

        trace!("create Context({:?})", context);

        Context(context, true)
    }

    /// Obtain the global context instance.
    pub fn global() -> Self {
        let context = unsafe { LLVMGetGlobalContext() };

        trace!("obtain global Context({:?})", context);

        Context(context, false)
    }

    /// Wrap a raw context reference.
    pub fn from_raw(context: LLVMContextRef) -> Self {
        Context(context, false)
    }

    /// Extracts the raw context reference.
    pub fn as_raw(&self) -> LLVMContextRef {
        self.0
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        if self.1 {
            trace!("drop {:?}", self);

            unsafe { LLVMContextDispose(self.0) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create() {
        let ctxt = Context::new();

        assert!(!ctxt.as_raw().is_null());
        assert!(ctxt != Context::new());
    }

    #[test]
    fn global() {
        let ctxt = Context::global();

        assert!(!ctxt.as_raw().is_null());
        assert_eq!(ctxt, Context::global());
    }
}
