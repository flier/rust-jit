use std::borrow::Cow;
use std::fmt;
use std::path::Path;

use llvm::core::*;
use llvm::prelude::*;

use context::{Context, GlobalContext};
use errors::Result;
use function::Function;
use function::FunctionType;
use global::GlobalVar;
use types::TypeRef;
use utils::{from_unchecked_cstr, AsRaw, AsResult, DisposableMessage, FromRaw, UncheckedCStr};

pub type AddressSpace = u32;

/// Modules represent the top-level structure in an LLVM program.
#[derive(Debug)]
pub struct Module(State);

#[derive(Debug)]
pub enum State {
    Owned(LLVMModuleRef),
    Borrowed(LLVMModuleRef),
}

impl AsRaw for Module {
    type RawType = LLVMModuleRef;

    fn as_raw(&self) -> Self::RawType {
        match self.0 {
            State::Owned(m) | State::Borrowed(m) => m,
        }
    }
}

impl Module {
    pub fn borrow(&self) -> Self {
        self.as_raw().into()
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
        let name = name.as_ref();

        unsafe { LLVMSetModuleIdentifier(self.as_raw(), cstr!(name), name.len()) }
    }

    /// Obtain the data layout for a module.
    pub fn data_layout_str(&self) -> Cow<str> {
        unsafe { LLVMGetDataLayoutStr(self.as_raw()) }.as_str()
    }

    /// Set the data layout for a module.
    pub fn set_data_layout_str<S: AsRef<str>>(&self, name: S) {
        unsafe { LLVMSetDataLayout(self.as_raw(), cstr!(name.as_ref())) }
    }

    /// Obtain the target triple for a module.
    pub fn target_triple(&self) -> Cow<str> {
        unsafe { LLVMGetTarget(self.as_raw()) }.as_str()
    }

    /// Set the target triple for a module.
    pub fn set_target_triple<S: AsRef<str>>(&self, triple: S) {
        unsafe { LLVMSetTarget(self.as_raw(), cstr!(triple.as_ref())) }
    }

    /// Dump a representation of a module to stderr.
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.as_raw()) }
    }

    /// Print a representation of a module to a file.
    pub fn print_to_file<P: AsRef<Path>>(&self, filename: P) -> Result<()> {
        let filename = filename.as_ref();
        let mut err = DisposableMessage::new();

        unsafe { LLVMPrintModuleToFile(self.as_raw(), cpath!(filename), &mut err) }.ok_or_else(|| {
            format!(
                "fail to print {:?} to file `{:?}`, {}",
                self,
                filename,
                err.into_string()
            ).into()
        })
    }

    /// Obtain a Type from a module by its registered name.
    pub fn get_type<S: AsRef<str>>(&self, name: S) -> Option<TypeRef> {
        let name = name.as_ref();

        unsafe { LLVMGetTypeByName(self.as_raw(), cstr!(name)) }.wrap()
    }

    /// Look up the specified function in the module symbol table.
    ///
    /// If it does not exist, add a prototype for the function and return it.
    /// This is nice because it allows most passes to get away with not handling
    /// the symbol table directly for this common task.
    pub fn get_or_insert_function<S: AsRef<str>, T: Into<TypeRef>>(
        &self,
        name: S,
        return_type: T,
        params_type: &[TypeRef],
    ) -> Function {
        self.get_function(name.as_ref()).unwrap_or_else(|| {
            self.add_function(
                name,
                FunctionType::new(return_type.into(), params_type, false),
            )
        })
    }

    /// Add a function to a module under a specified name.
    pub fn add_function<S: AsRef<str>>(&self, name: S, func_type: FunctionType) -> Function {
        let name = name.as_ref();
        let func = unsafe { LLVMAddFunction(self.as_raw(), cstr!(name), func_type.as_raw()) };

        trace!(
            "add function `{}`: {:?} to {:?}: Function({:?})",
            name,
            func_type,
            self,
            func,
        );

        func.into()
    }

    /// Obtain a Function value from a Module by its name.
    pub fn get_function<S: AsRef<str>>(&self, name: S) -> Option<Function> {
        let name = name.as_ref();

        unsafe { LLVMGetNamedFunction(self.as_raw(), cstr!(name)) }.wrap()
    }

    /// Obtain an iterator to the Function in a module.
    pub fn functions(&self) -> FuncIter {
        FuncIter::new(self.as_raw())
    }

    /// Add a global variable to a module under a specified name.
    pub fn add_global_var<S, T>(&self, name: S, ty: T) -> GlobalVar
    where
        S: AsRef<str>,
        T: Into<TypeRef>,
    {
        let name = name.as_ref();
        let ty = ty.into();

        let var = unsafe { LLVMAddGlobal(self.as_raw(), ty.as_raw(), cstr!(name)) };

        trace!(
            "add global var `{}`: {:?} to {:?}: ValueRef({:?})",
            name,
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
        let name = name.as_ref();
        let var = unsafe { LLVMAddGlobalInAddressSpace(self.as_raw(), ty.as_raw(), cstr!(name), address_space) };

        trace!(
            "add global var `{}`: {:?} to {:?}: ValueRef({:?})",
            name,
            ty,
            self,
            var,
        );

        var.into()
    }

    /// Obtain a global variable value from a Module by its name.
    pub fn get_global_var<S: AsRef<str>>(&self, name: S) -> Option<GlobalVar> {
        unsafe { LLVMGetNamedGlobal(self.as_raw(), cstr!(name.as_ref())) }.wrap()
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
    Function
);

impl_iter!(
    GlobalVarIter,
    LLVMGetFirstGlobal | LLVMGetLastGlobal[LLVMModuleRef],
    LLVMGetNextGlobal | LLVMGetPreviousGlobal[LLVMValueRef],
    GlobalVar
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
        Module(State::Borrowed(m))
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
            unsafe { LLVMPrintModuleToString(self.as_raw()) }.into_string()
        )
    }
}

impl Context {
    /// Create a new, empty module in a specific context.
    pub fn create_module<S: AsRef<str>>(&self, name: S) -> Module {
        let name = name.as_ref();
        let module = unsafe { LLVMModuleCreateWithNameInContext(cstr!(name), self.as_raw()) };

        trace!(
            "create `{}` module in {:?}: Module({:?})",
            name,
            self,
            module,
        );

        Module(State::Owned(module))
    }
}

impl GlobalContext {
    /// Create a new, empty module in the global context.
    pub fn create_module<S: AsRef<str>>(name: S) -> Module {
        let name = name.as_ref();
        let module = unsafe { LLVMModuleCreateWithName(cstr!(name)) };

        trace!(
            "create `{}` module in global context: Module({:?})",
            name,
            module
        );

        Module(State::Owned(module))
    }
}

#[cfg(test)]
mod tests {
    use std::io::prelude::*;

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

        assert!(m.verify().is_ok());
        assert!(sum.verify().is_ok());
    }
}
