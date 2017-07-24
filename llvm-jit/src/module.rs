use std::fmt;

use llvm::prelude::*;
use llvm::core::*;

use context::Context;
use types::{ValueRef, FunctionType, Function};
use utils::unchecked_cstring;

/// Modules represent the top-level structure in an LLVM program.
pub struct Module(LLVMModuleRef);

impl Module {
    /// Create a new, empty module in the global context.
    pub fn with_name<S: AsRef<str>>(name: S) -> Self {
        let cname = unchecked_cstring(name);
        let module = unsafe { LLVMModuleCreateWithName(cname.as_ptr()) };

        trace!(
            "create `{}` module@{:?} in global context",
            cname.to_string_lossy(),
            module
        );

        Module(module)
    }

    /// Create a new, empty module in a specific context.
    pub fn with_context<S: AsRef<str>>(name: S, context: &Context) -> Self {
        let cname = unchecked_cstring(name);
        let module = unsafe { LLVMModuleCreateWithNameInContext(cname.as_ptr(), context.as_raw()) };

        trace!(
            "create `{}` module@{:?} in {}",
            cname.to_string_lossy(),
            module,
            context
        );

        Module(module)
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func: FunctionType) -> Function {
        let cname = unchecked_cstring(name);

        ValueRef::from_raw(unsafe {
            LLVMAddFunction(self.0, cname.as_ptr(), func.as_raw())
        })
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        trace!("drop module@{:?}", self.0);

        unsafe { LLVMDisposeModule(self.0) }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        trace!("clone module@{:?}", self.0);

        Module(unsafe { LLVMCloneModule(self.0) })
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "module@{:?}", self.0)
    }
}
