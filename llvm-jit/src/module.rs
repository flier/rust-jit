use std::borrow::Cow;
use std::ffi::CStr;
use std::path::Path;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use context::Context;
use errors::Result;
use types::FunctionType;
use utils::{from_unchecked_cstr, unchecked_cstring};
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
    pub fn with_name_in_context<S: AsRef<str>>(name: S, context: &Context) -> Self {
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

    /// Obtain the identifier of a module.
    pub fn name(&self) -> Cow<str> {
        unsafe {
            let mut len = 0;
            let p = LLVMGetModuleIdentifier(self.0, &mut len);

            from_unchecked_cstr(p as *const u8, len as usize)
        }
    }

    /// Set the identifier of a module to a string Ident with length Len.
    pub fn set_name<S: AsRef<str>>(&self, name: S) {
        let len = name.as_ref().len();
        let cname = unchecked_cstring(name);
        unsafe { LLVMSetModuleIdentifier(self.0, cname.as_ptr(), len) }
    }

    /// Obtain the data layout for a module.
    pub fn data_layout(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetDataLayoutStr(self.0)).to_string_lossy() }
    }

    /// Set the data layout for a module.
    pub fn set_data_layout<S: AsRef<str>>(&self, name: S) {
        let cname = unchecked_cstring(name);
        unsafe { LLVMSetDataLayout(self.0, cname.as_ptr()) }
    }

    /// Obtain the target triple for a module.
    pub fn target_triple(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetTarget(self.0)).to_string_lossy() }
    }

    /// Set the target triple for a module.
    pub fn set_target_triple<S: AsRef<str>>(&self, triple: S) {
        let ctriple = unchecked_cstring(triple);
        unsafe { LLVMSetTarget(self.0, ctriple.as_ptr()) }
    }

    /// Dump a representation of a module to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.0) }
    }

    /// Print a representation of a module to a file.
    pub fn print_to_file<P: AsRef<Path>>(&self, filename: P) -> Result<()> {
        let cfilename = unchecked_cstring(filename.as_ref().to_string_lossy());
        let mut err = ptr::null_mut();

        unsafe {
            if LLVMPrintModuleToFile(self.0, cfilename.as_ptr(), &mut err) == 0 {
                Ok(())
            } else {
                bail!(format!(
                    "fail to print {:?} to file `{}`, {}",
                    self,
                    cfilename.to_string_lossy(),
                    CStr::from_ptr(err).to_string_lossy()
                ))
            }
        }
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

#[cfg(test)]
mod tests {
    use std::io::prelude::*;

    use tempfile::NamedTempFile;

    use super::*;

    #[test]
    fn create() {
        let context = Context::new();
        let m = Module::with_name_in_context("test", &context);

        assert!(!m.as_raw().is_null());
        assert_eq!(m.name(), "test");

        m.set_name("hello");
        assert_eq!(m.name(), "hello");

        assert_eq!(m.data_layout(), "");
        m.set_data_layout("e");
        assert_eq!(m.data_layout(), "e");

        assert_eq!(m.target_triple(), "");
        m.set_target_triple("x86_64-apple-darwin");
        assert_eq!(m.target_triple(), "x86_64-apple-darwin");

        let mut f = NamedTempFile::new().unwrap();

        m.print_to_file(f.path()).unwrap();

        let mut buf = String::new();

        f.read_to_string(&mut buf).unwrap();

        assert_eq!(
            buf,
            r#"; ModuleID = 'hello'
source_filename = "test"
target datalayout = "e"
target triple = "x86_64-apple-darwin"
"#
        );
    }
}
