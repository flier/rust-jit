use llvm::core::*;
use llvm::prelude::*;

use context::Context;
use types::FunctionType;
use utils::unchecked_cstring;
use value::{Function, ValueRef};

/// Modules represent the top-level structure in an LLVM program.
#[derive(Debug)]
pub struct Module(LLVMModuleRef);

impl Module {
    /// Create a new, empty module in the global context.
    pub fn with_name<S: AsRef<str>>(name: S) -> Self {
        let cname = unchecked_cstring(name);
        let module = unsafe { LLVMModuleCreateWithName(cname.as_ptr()) };

        trace!(
            "create `{}` module in global context: Module({:?})",
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
            "create `{}` module in {:?}: Module({:?})",
            cname.to_string_lossy(),
            context,
            module,
        );

        Module(module)
    }

    /// Wrap a raw module reference.
    pub fn from_raw(module: LLVMModuleRef) -> Self {
        Module(module)
    }

    /// Extracts the raw module reference.
    pub fn as_raw(&self) -> LLVMModuleRef {
        self.0
    }

    /// Dump a representation of a module to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.0) }
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func_type: FunctionType) -> Function {
        let cname = unchecked_cstring(name);
        let func = unsafe { LLVMAddFunction(self.0, cname.as_ptr(), func_type.as_raw()) };

        trace!(
            "add `{}` function: {:?} to {:?} as Function({:?})",
            cname.to_string_lossy(),
            func_type,
            self,
            func,
        );

        ValueRef::from_raw(func)
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeModule(self.0) }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        trace!("clone {:?}", self);

        Module(unsafe { LLVMCloneModule(self.0) })
    }
}
