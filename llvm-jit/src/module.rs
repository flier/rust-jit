use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::ops::Deref;
use std::path::Path;
use std::ptr;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use context::Context;
use errors::Result;
use types::{AsTypeRef, FunctionType, TypeRef};
use utils::{from_unchecked_cstr, unchecked_cstring};
use value::{Constant, Function, ValueRef};

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

    /// Obtain the context to which this module is associated.
    pub fn context(&self) -> Context {
        Context::from_raw(unsafe { LLVMGetModuleContext(self.0) })
    }

    /// Obtain the identifier of a module.
    pub fn name(&self) -> Cow<str> {
        unsafe {
            let mut len = 0;
            let p = LLVMGetModuleIdentifier(self.0, &mut len);

            from_unchecked_cstr(p as *const u8, len as usize + 1)
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

    /// Obtain a Type from a module by its registered name.
    pub fn get_type<S: AsRef<str>>(&self, name: S) -> Option<TypeRef> {
        let cname = unchecked_cstring(name);
        let t = unsafe { LLVMGetTypeByName(self.0, cname.as_ptr()).as_mut() }
            .map(|t| TypeRef::from_raw(t));

        if let Some(t) = t {
            trace!(
                "found `{}` type in {:?}: {:?}",
                cname.to_string_lossy(),
                self,
                t
            );
        } else {
            trace!("not found `{}` type in {:?}", cname.to_string_lossy(), self);
        }

        t
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func_type: FunctionType) -> Function {
        let cname = unchecked_cstring(name);
        let func = unsafe { LLVMAddFunction(self.0, cname.as_ptr(), func_type.as_raw()) };

        trace!(
            "add function `{}`: {:?} to {:?}: Function({:?})",
            cname.to_string_lossy(),
            func_type,
            self,
            func,
        );

        Function::from_raw(func)
    }

    /// Obtain a Function value from a Module by its name.
    pub fn get_function<S: AsRef<str>>(&self, name: S) -> Option<Function> {
        let cname = unchecked_cstring(name);
        let func = unsafe { LLVMGetNamedFunction(self.0, cname.as_ptr()).as_mut() }
            .map(|f| Function::from_raw(f));

        if let Some(f) = func {
            trace!(
                "found `{}` function in {:?}: {:?}",
                cname.to_string_lossy(),
                self,
                f
            );
        } else {
            trace!(
                "not found `{}` function in {:?}",
                cname.to_string_lossy(),
                self
            );
        }

        func
    }

    /// Obtain an iterator to the Function in a module.
    pub fn functions(&self) -> FuncIter {
        FuncIter::new(self.as_raw())
    }

    /// Add a global variable to a module under a specified name.
    pub fn add_global_var<S: AsRef<str>>(&self, name: S, ty: TypeRef) -> GlobalVar {
        let cname = unchecked_cstring(name);

        let var = unsafe { LLVMAddGlobal(self.as_raw(), ty.as_raw(), cname.as_ptr()) };

        trace!(
            "add global var `{}`: {:?} to {:?}: ValueRef({:?})",
            cname.to_string_lossy(),
            ty,
            self,
            var,
        );

        GlobalVar::from_raw(var)
    }

    /// Add a global variable to a module under a specified name.
    pub fn add_global_var_in_address_space<S: AsRef<str>>(
        &self,
        name: S,
        ty: TypeRef,
        address_space: u32,
    ) -> GlobalVar {
        let cname = unchecked_cstring(name);

        let var = unsafe {
            LLVMAddGlobalInAddressSpace(self.as_raw(), ty.as_raw(), cname.as_ptr(), address_space)
        };

        trace!(
            "add global var `{}`: {:?} to {:?}: ValueRef({:?})",
            cname.to_string_lossy(),
            ty,
            self,
            var,
        );

        GlobalVar::from_raw(var)
    }

    /// Obtain a global variable value from a Module by its name.
    pub fn get_global_var<S: AsRef<str>>(&self, name: S) -> Option<GlobalVar> {
        let cname = unchecked_cstring(name);

        unsafe { LLVMGetNamedGlobal(self.as_raw(), cname.as_ptr()).as_mut() }
            .map(|var| GlobalVar::from_raw(var))
    }

    /// Obtain an iterator to the global variables in a module.
    pub fn global_vars(&self) -> GlobalVarIter {
        GlobalVarIter::new(self.as_raw())
    }
}

impl_iter!(
    FuncIter,
    LLVMGetFirstFunction[LLVMModuleRef],
    LLVMGetNextFunction[LLVMValueRef],
    Function::from_raw
);

impl_iter!(
    GlobalVarIter,
    LLVMGetFirstGlobal[LLVMModuleRef],
    LLVMGetNextGlobal[LLVMValueRef],
    GlobalVar::from_raw
);

/// Global Variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalVar(ValueRef);

impl Deref for GlobalVar {
    type Target = ValueRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GlobalVar {
    fn from_raw(v: LLVMValueRef) -> Self {
        GlobalVar(ValueRef::from_raw(v))
    }

    pub fn delete(&self) {
        unsafe { LLVMDeleteGlobal(self.as_raw()) }
    }

    pub fn initializer(&self) -> Option<ValueRef> {
        unsafe { LLVMGetInitializer(self.as_raw()).as_mut() }.map(|var| ValueRef::from_raw(var))
    }

    pub fn set_initializer(&self, initializer: Constant) {
        unsafe { LLVMSetInitializer(self.as_raw(), initializer.as_raw()) }
    }

    pub fn is_thread_local(&self) -> bool {
        unsafe { LLVMIsThreadLocal(self.as_raw()) != 0 }
    }

    pub fn set_thread_local(&self, is_thread_local: bool) {
        unsafe { LLVMSetThreadLocal(self.as_raw(), if is_thread_local { 1 } else { 0 }) }
    }

    pub fn is_global_constant(&self) -> bool {
        unsafe { LLVMIsGlobalConstant(self.as_raw()) != 0 }
    }

    pub fn set_global_constant(&self, is_global_constant: bool) {
        unsafe { LLVMSetGlobalConstant(self.as_raw(), if is_global_constant { 1 } else { 0 }) }
    }

    pub fn thread_local_mode(&self) -> ThreadLocalMode {
        unsafe { LLVMGetThreadLocalMode(self.as_raw()) }
    }

    pub fn set_thread_local_mode(&self, mode: ThreadLocalMode) {
        unsafe { LLVMSetThreadLocalMode(self.as_raw(), mode) }
    }

    pub fn is_externally_initialized(&self) -> bool {
        unsafe { LLVMIsExternallyInitialized(self.as_raw()) != 0 }
    }

    pub fn set_externally_initialized(&self, is_externally_initialized: bool) {
        unsafe {
            LLVMSetExternallyInitialized(
                self.as_raw(),
                if is_externally_initialized { 1 } else { 0 },
            )
        }
    }
}

pub type ThreadLocalMode = LLVMThreadLocalMode;

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

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let s = LLVMPrintModuleToString(self.0);

            let r = write!(f, "{}", CStr::from_ptr(s).to_string_lossy());

            LLVMDisposeMessage(s);

            r
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::prelude::*;

    use llvm;
    use tempfile::NamedTempFile;

    use super::*;
    use types::*;
    use value::*;

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

        assert_eq!(m.to_string(), buf);
    }

    #[test]
    fn function() {
        let context = Context::new();
        let m = Module::with_name_in_context("test", &context);

        assert_eq!(m.get_function("nop"), None);

        let f = m.add_function("nop", FunctionType::new(context.void(), &[], false));

        assert!(!f.as_raw().is_null());
        assert_eq!(f.to_string(), "\ndeclare void @nop()\n");

        assert_eq!(m.get_function("nop"), Some(f));
        assert_eq!(m.get_function("sum"), None);

        let i64t = context.int64();
        let argts = [i64t, i64t, i64t];
        let sum = m.add_function("sum", FunctionType::new(i64t, &argts, false));

        assert_eq!(sum.name(), Some("sum".into()));
        assert_eq!(m.get_function("sum"), Some(sum));

        assert_eq!(m.functions().collect::<Vec<Function>>(), vec![f, sum]);
    }

    #[test]
    fn global_var() {
        let c = Context::new();
        let m = Module::with_name_in_context("test", &c);

        assert_eq!(m.get_global_var("x"), None);

        let i64t = c.int64();
        let f64t = c.double();

        m.add_global_var("x", i64t);
        m.add_global_var("y", f64t);

        let x = m.get_global_var("x").unwrap();
        let y = m.get_global_var("y").unwrap();

        assert_eq!(m.global_vars().collect::<Vec<GlobalVar>>(), vec![x, y]);

        assert_eq!(x.name().unwrap(), "x");
        assert_eq!(y.name().unwrap(), "y");

        assert_eq!(x.to_string(), "@x = external global i64");

        // set initializer
        assert_eq!(x.initializer(), None);

        let v = i64t.uint(123);

        x.set_initializer(v);

        assert_eq!(x.initializer(), Some(v));

        assert_eq!(x.to_string(), "@x = global i64 123");

        // set as global constant
        assert!(!x.is_global_constant());

        x.set_global_constant(true);

        assert_eq!(x.to_string(), "@x = constant i64 123");

        x.set_global_constant(false);

        // set as externally initialized
        assert!(!x.is_externally_initialized());

        x.set_externally_initialized(true);

        assert_eq!(x.to_string(), "@x = externally_initialized global i64 123");

        x.set_externally_initialized(false);

        // set as thread local
        assert!(!x.is_thread_local());

        x.set_thread_local(true);

        assert!(x.is_thread_local());

        assert_eq!(x.to_string(), "@x = thread_local global i64 123");

        x.set_thread_local(false);

        // set thread local mod
        assert!(matches!(
            x.thread_local_mode(),
            llvm::LLVMThreadLocalMode::LLVMNotThreadLocal
        ));
    }
}
