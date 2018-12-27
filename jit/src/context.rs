use std::ops::Deref;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::utils::AsRaw;

/// Contexts are execution states for the core LLVM IR system.
#[derive(Debug)]
pub enum Context {
    Owned(LLVMContextRef),
    Borrowed(LLVMContextRef),
}

impl AsRaw for Context {
    type RawType = LLVMContextRef;

    fn as_raw(&self) -> Self::RawType {
        match self {
            Context::Owned(context) | Context::Borrowed(context) => *context,
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq<Context> for Context {
    fn eq(&self, other: &Context) -> bool {
        self.as_raw() == other.as_raw()
    }
}

impl From<LLVMContextRef> for Context {
    fn from(context: LLVMContextRef) -> Self {
        Context::Borrowed(context)
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        if let Context::Owned(context) = self {
            unsafe { LLVMContextDispose(*context) }

            trace!("drop {:?}", self);
        }
    }
}

impl Context {
    /// Create a new context.
    pub fn new() -> Self {
        let context = unsafe { LLVMContextCreate() };

        trace!("create Context({:?})", context);

        Context::Owned(context)
    }

    /// Obtain the global context instance.
    pub fn global() -> GlobalContext {
        let context = unsafe { LLVMGetGlobalContext() };

        trace!("global Context({:?})", context);

        GlobalContext(Context::Borrowed(context))
    }
}

#[repr(transparent)]
#[derive(Debug, PartialEq)]
pub struct GlobalContext(Context);

impl Deref for GlobalContext {
    type Target = Context;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create() {
        let ctxt = Context::new();

        assert!(!ctxt.as_raw().is_null());
        assert_ne!(ctxt, Context::new());
    }

    #[test]
    fn global() {
        let ctxt = Context::global();

        assert!(!ctxt.as_raw().is_null());
        assert_eq!(ctxt, Context::global());
    }
}
