use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::mem;
use std::path::Path;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use context::{Context, GlobalContext};
use errors::Result;
use function::Function;
use function::FunctionType;
use global::GlobalVar;
use types::{AsTypeRef, TypeRef};
use utils::{AsResult, DisposableMessage, from_unchecked_cstr, unchecked_cstring};
use value::ValueRef;

pub type AddressSpace = u32;

/// Modules represent the top-level structure in an LLVM program.
#[derive(Debug)]
pub struct Module(State);

#[derive(Debug)]
pub enum State {
    Owned(LLVMModuleRef),
    Borrowed(LLVMModuleRef),
}

impl Module {
    pub fn from_faw(m: LLVMModuleRef) -> Self {
        Module(State::Borrowed(m))
    }

    pub fn as_raw(&self) -> LLVMModuleRef {
        match self.0 {
            State::Owned(m) |
            State::Borrowed(m) => m,
        }
    }

    pub fn into_raw(self) -> LLVMModuleRef {
        let raw = self.as_raw();
        mem::forget(self);
        raw
    }

    /// Obtain the context to which this module is associated.
    pub fn context(&self) -> Context {
        unsafe { LLVMGetModuleContext(self.as_raw()) }.into()
    }

    /// Obtain the identifier of a module.
    pub fn name(&self) -> Cow<str> {
        unsafe {
            let mut len = 0;
            let p = LLVMGetModuleIdentifier(self.as_raw(), &mut len);

            from_unchecked_cstr(p as *const u8, len as usize + 1)
        }
    }

    /// Set the identifier of a module to a string Ident with length Len.
    pub fn set_name<S: AsRef<str>>(&self, name: S) {
        let len = name.as_ref().len();
        let cname = unchecked_cstring(name);
        unsafe { LLVMSetModuleIdentifier(self.as_raw(), cname.as_ptr(), len) }
    }

    /// Obtain the data layout for a module.
    pub fn data_layout_str(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetDataLayoutStr(self.as_raw())).to_string_lossy() }
    }

    /// Set the data layout for a module.
    pub fn set_data_layout_str<S: AsRef<str>>(&self, name: S) {
        let cname = unchecked_cstring(name);
        unsafe { LLVMSetDataLayout(self.as_raw(), cname.as_ptr()) }
    }

    /// Obtain the target triple for a module.
    pub fn target_triple(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetTarget(self.as_raw())).to_string_lossy() }
    }

    /// Set the target triple for a module.
    pub fn set_target_triple<S: AsRef<str>>(&self, triple: S) {
        let ctriple = unchecked_cstring(triple);
        unsafe { LLVMSetTarget(self.as_raw(), ctriple.as_ptr()) }
    }

    /// Dump a representation of a module to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.as_raw()) }
    }

    /// Print a representation of a module to a file.
    pub fn print_to_file<P: AsRef<Path>>(&self, filename: P) -> Result<()> {
        let cfilename = unchecked_cstring(filename.as_ref().to_string_lossy());
        let mut err = ptr::null_mut();

        unsafe {
            if LLVMPrintModuleToFile(self.as_raw(), cfilename.as_ptr(), &mut err).is_ok() {
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
        let t = unsafe { LLVMGetTypeByName(self.as_raw(), cname.as_ptr()).as_mut() }
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

    pub fn get_named_operands<S: AsRef<str>>(&self, name: S) -> Vec<ValueRef> {
        let cname = unchecked_cstring(name);
        let count = unsafe { LLVMGetNamedMetadataNumOperands(self.as_raw(), cname.as_ptr()) };

        let mut operands = vec![ptr::null_mut(); count as usize];

        unsafe {
            LLVMGetNamedMetadataOperands(self.as_raw(), cname.as_ptr(), operands.as_mut_ptr())
        };

        operands.into_iter().map(ValueRef::from_raw).collect()
    }

    pub fn add_named_operand<S: AsRef<str>, V: AsRef<ValueRef>>(&self, name: S, v: V) {
        unsafe {
            LLVMAddNamedMetadataOperand(
                self.as_raw(),
                unchecked_cstring(name).as_ptr(),
                v.as_ref().as_raw(),
            )
        }
    }

    /// Look up the specified function in the module symbol table.
    ///
    /// If it does not exist, add a prototype for the function and return it.
    /// This is nice because it allows most passes to get away with not handling
    /// the symbol table directly for this common task.
    pub fn get_or_insert_function<S: AsRef<str>>(
        &self,
        name: S,
        return_type: TypeRef,
        params_type: &[TypeRef],
    ) -> Function {
        self.get_function(name.as_ref()).unwrap_or_else(|| {
            self.add_function(name, FunctionType::new(return_type, params_type, false))
        })
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func_type: FunctionType) -> Function {
        let cname = unchecked_cstring(name);
        let func = unsafe { LLVMAddFunction(self.as_raw(), cname.as_ptr(), func_type.as_raw()) };

        trace!(
            "add function `{}`: {:?} to {:?}: Function({:?})",
            cname.to_string_lossy(),
            func_type,
            self,
            func,
        );

        func.into()
    }

    /// Obtain a Function value from a Module by its name.
    pub fn get_function<S: AsRef<str>>(&self, name: S) -> Option<Function> {
        let cname = unchecked_cstring(name);
        let func = unsafe { LLVMGetNamedFunction(self.as_raw(), cname.as_ptr()).as_mut() }
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

        var.into()
    }

    /// Add a global variable to a module under a specified name.
    pub fn add_global_var_in_address_space<S: AsRef<str>>(
        &self,
        name: S,
        ty: TypeRef,
        address_space: AddressSpace,
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

        var.into()
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
    LLVMGetFirstFunction | LLVMGetLastFunction[LLVMModuleRef],
    LLVMGetNextFunction | LLVMGetPreviousFunction[LLVMValueRef],
    Function::from_raw
);

impl_iter!(
    GlobalVarIter,
    LLVMGetFirstGlobal | LLVMGetLastGlobal[LLVMModuleRef],
    LLVMGetNextGlobal | LLVMGetPreviousGlobal[LLVMValueRef],
    GlobalVar::from_raw
);

impl Drop for Module {
    fn drop(&mut self) {
        if let State::Owned(m) = self.0 {
            trace!("drop {:?}", self);

            unsafe { LLVMDisposeModule(m) }
        }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        trace!("clone {:?}", self);

        unsafe { LLVMCloneModule(self.as_raw()) }.into()
    }
}

impl From<LLVMModuleRef> for Module {
    fn from(m: LLVMModuleRef) -> Self {
        Module::from_faw(m)
    }
}

impl From<Module> for LLVMModuleRef {
    fn from(m: Module) -> Self {
        m.as_raw()
    }
}

impl PartialEq<Module> for Module {
    fn eq(&self, other: &Module) -> bool {
        self.as_raw() == other.as_raw()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            unsafe { LLVMPrintModuleToString(self.as_raw()) }.to_string()
        )
    }
}

impl Context {
    /// Create a new, empty module in a specific context.
    pub fn create_module<S: AsRef<str>>(&self, name: S) -> Module {
        let cname = unchecked_cstring(name);
        let module = unsafe { LLVMModuleCreateWithNameInContext(cname.as_ptr(), self.as_raw()) };

        trace!(
            "create `{}` module in {:?}: Module({:?})",
            cname.to_string_lossy(),
            self,
            module,
        );

        Module(State::Owned(module))
    }
}

impl GlobalContext {
    /// Create a new, empty module in the global context.
    pub fn create_module<S: AsRef<str>>(name: S) -> Module {
        let cname = unchecked_cstring(name);
        let module = unsafe { LLVMModuleCreateWithName(cname.as_ptr()) };

        trace!(
            "create `{}` module in global context: Module({:?})",
            cname.to_string_lossy(),
            module
        );

        Module(State::Owned(module))
    }
}

#[cfg(test)]
mod tests {
    use std::io::prelude::*;

    use llvm::analysis::LLVMVerifierFailureAction;
    use tempfile::NamedTempFile;

    use super::*;
    use types::*;

    #[test]
    fn create() {
        let context = Context::new();
        let m = context.create_module("test");

        assert!(!m.as_raw().is_null());
        assert_eq!(m.name(), "test");

        m.set_name("hello");
        assert_eq!(m.name(), "hello");

        assert_eq!(m.data_layout_str(), "");
        m.set_data_layout_str("e");
        assert_eq!(m.data_layout_str(), "e");

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
        let m = context.create_module("function");

        assert_eq!(m.get_function("nop"), None);

        let f = m.add_function("nop", FunctionType::new(context.void_t(), &[], false));

        assert!(!f.as_raw().is_null());
        assert_eq!(f.to_string(), "\ndeclare void @nop()\n");

        assert_eq!(m.get_function("nop"), Some(f));
        assert_eq!(m.get_function("sum"), None);

        let i64_t = context.int64_t();
        let argts = [i64_t, i64_t, i64_t];
        let sum = m.add_function("sum", FunctionType::new(i64_t, &argts, false));

        assert_eq!(sum.name(), Some("sum".into()));
        assert_eq!(m.get_function("sum"), Some(sum));

        assert_eq!(m.functions().collect::<Vec<Function>>(), vec![f, sum]);

        assert!(
            m.verify(LLVMVerifierFailureAction::LLVMAbortProcessAction)
                .is_ok()
        );
        assert!(
            sum.verify(LLVMVerifierFailureAction::LLVMAbortProcessAction)
                .is_ok()
        );
    }
}
