use std::ffi::CString;

use llvm::prelude::*;
use llvm::core::*;

use errors::Result;
use context::Context;
use types::{ValueRef, Function};

/// Modules represent the top-level structure in an LLVM program.
pub struct Module(LLVMModuleRef);

impl Module {
    /// Create a new, empty module in the global context.
    pub fn with_name<S: AsRef<str>>(name: S) -> Result<Self> {
        let name = name.as_ref();
        let cname = CString::new(name)?;
        let module = unsafe { LLVMModuleCreateWithName(cname.as_ptr()) };

        trace!("create `{}` module #{:?} in global context", name, module);

        Ok(Module(module))
    }

    /// Create a new, empty module in a specific context.
    pub fn with_context<S: AsRef<str>>(name: S, context: &Context) -> Result<Self> {
        let name = name.as_ref();
        let cname = CString::new(name)?;
        let context = context.as_raw();
        let module = unsafe { LLVMModuleCreateWithNameInContext(cname.as_ptr(), context) };

        trace!(
            "create `{}` module #{:?} in context #{:?}",
            name,
            module,
            context
        );

        Ok(Module(module))
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func: Function) -> Result<ValueRef> {
        let name = name.as_ref();
        let cname = CString::new(name)?;

        Ok(ValueRef::wrap(unsafe {
            LLVMAddFunction(self.0, cname.as_ptr(), func.as_raw())
        }))
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        trace!("drop module #{:?}", self.0);

        unsafe { LLVMDisposeModule(self.0) }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        trace!("clone context #{:?}", self.0);

        Module(unsafe { LLVMCloneModule(self.0) })
    }
}
