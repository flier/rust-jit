use llvm::core::*;
use llvm::prelude::*;

/// Contexts are execution states for the core LLVM IR system.
#[derive(Debug, PartialEq)]
pub struct Context(State);

#[derive(Debug, PartialEq)]
enum State {
    Owned(LLVMContextRef),
    Global(LLVMContextRef),
    Borrowed(LLVMContextRef),
}

impl Context {
    /// Create a new context.
    pub fn new() -> Self {
        let context = unsafe { LLVMContextCreate() };

        trace!("create Context({:?})", context);

        Context(State::Owned(context))
    }

    /// Obtain the global context instance.
    pub fn global() -> Self {
        let context = unsafe { LLVMGetGlobalContext() };

        trace!("obtain global Context({:?})", context);

        Context(State::Global(context))
    }

    /// Wrap a raw context reference.
    pub fn from_raw(context: LLVMContextRef) -> Self {
        Context(State::Borrowed(context))
    }

    /// Extracts the raw context reference.
    pub fn as_raw(&self) -> LLVMContextRef {
        match self.0 {
            State::Owned(context) |
            State::Global(context) |
            State::Borrowed(context) => context,
        }
    }
}

impl From<LLVMContextRef> for Context {
    fn from(context: LLVMContextRef) -> Self {
        Self::from_raw(context)
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        if let State::Owned(context) = self.0 {
            trace!("drop {:?}", self);

            unsafe { LLVMContextDispose(context) }
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
