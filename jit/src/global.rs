use std::borrow::Cow;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::*;

use crate::constant::{AsConstant, Constant};
use crate::module::{AddressSpace, Module};
use crate::types::TypeRef;
use crate::utils::{AsBool, AsLLVMBool, AsRaw, AsResult, UncheckedCStr};
use crate::value::{AsValueRef, ValueRef};

pub type Linkage = LLVMLinkage;
pub type Visibility = LLVMVisibility;
pub type DLLStorageClass = LLVMDLLStorageClass;
pub type UnnamedAddr = LLVMUnnamedAddr;

pub trait GlobalValue: AsValueRef {
    fn parent(&self) -> Module {
        unsafe { LLVMGetGlobalParent(self.as_raw()) }.into()
    }

    fn is_declaration(&self) -> bool {
        unsafe { LLVMIsDeclaration(self.as_raw()) }.as_bool()
    }

    fn linkage(&self) -> Linkage {
        unsafe { LLVMGetLinkage(self.as_raw()) }
    }

    fn set_linkage(&self, linkage: Linkage) -> &Self {
        unsafe { LLVMSetLinkage(self.as_raw(), linkage) };
        self
    }

    fn section(&self) -> Option<Cow<str>> {
        unsafe { LLVMGetSection(self.as_raw()).as_ref() }.map(|s| s.as_str())
    }

    fn set_section<S: AsRef<str>>(&self, section: S) -> &Self {
        unsafe { LLVMSetSection(self.as_raw(), cstr!(section)) };
        self
    }

    fn visibility(&self) -> Visibility {
        unsafe { LLVMGetVisibility(self.as_raw()) }
    }

    fn set_visibility(&self, visibility: Visibility) -> &Self {
        unsafe { LLVMSetVisibility(self.as_raw(), visibility) };
        self
    }

    fn dll_storage_class(&self) -> DLLStorageClass {
        unsafe { LLVMGetDLLStorageClass(self.as_raw()) }
    }

    fn set_dll_storage_class(&self, class: DLLStorageClass) -> &Self {
        unsafe { LLVMSetDLLStorageClass(self.as_raw(), class) };
        self
    }

    fn unnamed_addr(&self) -> UnnamedAddr {
        unsafe { LLVMGetUnnamedAddress(self.as_raw()) }
    }

    fn set_unnamed_addr(&self, unnamed_addr: UnnamedAddr) -> &Self {
        unsafe { LLVMSetUnnamedAddress(self.as_raw(), unnamed_addr) };
        self
    }
}

pub type ThreadLocalMode = LLVMThreadLocalMode;

/// Global Variables
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalVar(Constant);

inherit_from!(GlobalVar, Constant, ValueRef; LLVMValueRef);

impl AsConstant for GlobalVar {
    fn as_const(&self) -> &Constant {
        &self.0
    }
}
impl GlobalValue for GlobalVar {}

impl GlobalVar {
    pub fn delete(&self) {
        unsafe { LLVMDeleteGlobal(self.as_raw()) }
    }

    pub fn initializer(&self) -> Option<Constant> {
        unsafe { LLVMGetInitializer(self.as_raw()) }.ok()
    }

    pub fn set_initializer<C: Into<Constant>>(&self, initializer: C) -> &Self {
        unsafe { LLVMSetInitializer(self.as_raw(), initializer.into().as_raw()) };
        self
    }

    pub fn is_thread_local(&self) -> bool {
        unsafe { LLVMIsThreadLocal(self.as_raw()) }.as_bool()
    }

    pub fn set_thread_local(&self, is_thread_local: bool) -> &Self {
        unsafe { LLVMSetThreadLocal(self.as_raw(), is_thread_local.as_bool()) };
        self
    }

    pub fn is_global_constant(&self) -> bool {
        unsafe { LLVMIsGlobalConstant(self.as_raw()) }.as_bool()
    }

    pub fn set_global_constant(&self, is_global_constant: bool) -> &Self {
        unsafe { LLVMSetGlobalConstant(self.as_raw(), is_global_constant.as_bool()) };
        self
    }

    pub fn thread_local_mode(&self) -> ThreadLocalMode {
        unsafe { LLVMGetThreadLocalMode(self.as_raw()) }
    }

    pub fn set_thread_local_mode(&self, mode: ThreadLocalMode) -> &Self {
        unsafe { LLVMSetThreadLocalMode(self.as_raw(), mode) };
        self
    }

    pub fn is_externally_initialized(&self) -> bool {
        unsafe { LLVMIsExternallyInitialized(self.as_raw()) }.as_bool()
    }

    pub fn set_externally_initialized(&self, is_externally_initialized: bool) -> &Self {
        unsafe { LLVMSetExternallyInitialized(self.as_raw(), is_externally_initialized.as_bool()) };
        self
    }
}

/// Global Alias
///
/// a single function or variable alias in the IR.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalAlias(Constant);

inherit_from!(GlobalAlias, Constant, ValueRef; LLVMValueRef);

impl AsConstant for GlobalAlias {
    fn as_const(&self) -> &Constant {
        &self.0
    }
}
impl GlobalValue for GlobalAlias {}

impl GlobalAlias {
    /// Retrieve the target value of an alias.
    pub fn aliasee(&self) -> ValueRef {
        unsafe { LLVMAliasGetAliasee(self.as_raw()) }.into()
    }

    /// Set the target value of an alias.
    pub fn set_aliasee<V: AsRaw<RawType = LLVMValueRef>>(&self, aliasee: V) {
        unsafe { LLVMAliasSetAliasee(self.as_raw(), aliasee.as_raw()) }
    }
}

impl Module {
    /// Add a global variable to a module under a specified name.
    pub fn add_global_var<S, T>(&self, name: S, ty: T) -> GlobalVar
    where
        S: AsRef<str>,
        T: Into<TypeRef>,
    {
        let name = name.as_ref();
        let ty = ty.into();

        let var = unsafe { LLVMAddGlobal(self.as_raw(), ty.as_raw(), cstr!(name)) };

        trace!("add global var `{}`: {:?} to {:?}: ValueRef({:?})", name, ty, self, var,);

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

        trace!("add global var `{}`: {:?} to {:?}: ValueRef({:?})", name, ty, self, var,);

        var.into()
    }

    /// Obtain a global variable value from a Module by its name.
    pub fn global_var<S: AsRef<str>>(&self, name: S) -> Option<GlobalVar> {
        unsafe { LLVMGetNamedGlobal(self.as_raw(), cstr!(name)) }.ok()
    }

    /// Obtain an iterator to the global variables in a module.
    pub fn global_vars(&self) -> GlobalVarIter {
        GlobalVarIter::new(self.as_raw())
    }

    /// Obtain a `GlobalAlias` value from a Module by its name.
    pub fn global_alias<S: AsRef<str>>(&self, name: S) -> Option<GlobalAlias> {
        let name = name.as_ref();

        // NOTE: `LLVMGetNamedGlobalAlias` has a bug that not use the `NameLen`, we need NUL at end
        unsafe { LLVMGetNamedGlobalAlias(self.as_raw(), cstr!(name), name.len()) }.ok()
    }

    /// Add a `GlobalAlias` value to a Module by its name.
    pub fn add_global_alias<T: AsRaw<RawType = LLVMTypeRef>, V: AsRaw<RawType = LLVMValueRef>, S: AsRef<str>>(
        &self,
        ty: T,
        aliasee: V,
        name: S,
    ) -> GlobalAlias {
        unsafe { LLVMAddAlias(self.as_raw(), ty.as_raw(), aliasee.as_raw(), cstr!(name)) }.into()
    }

    /// Obtain an iterator to the global aliases in a module.
    pub fn global_aliases(&self) -> GlobalAliasIter {
        GlobalAliasIter::new(self.as_raw())
    }
}

impl_iter!(
    GlobalVarIter,
    LLVMGetFirstGlobal | LLVMGetLastGlobal[LLVMModuleRef],
    LLVMGetNextGlobal | LLVMGetPreviousGlobal[LLVMValueRef],
    GlobalVar
);

impl_iter!(
    GlobalAliasIter,
    LLVMGetFirstGlobalAlias | LLVMGetLastGlobalAlias[LLVMModuleRef],
    LLVMGetNextGlobalAlias | LLVMGetPreviousGlobalAlias[LLVMValueRef],
    GlobalAlias
);

#[cfg(test)]
mod tests {
    use crate::llvm;

    use super::*;
    use crate::prelude::*;
    use crate::types::*;

    #[test]
    fn global_var() {
        let c = Context::new();
        let m = c.create_module("global_var");

        assert_eq!(m.global_var("x"), None);

        let i64_t = c.int64_t();
        let f64_t = c.double_t();

        m.add_global_var("x", i64_t);
        m.add_global_var("y", f64_t);

        let x = m.global_var("x").unwrap();
        let y = m.global_var("y").unwrap();

        assert_eq!(m.global_vars().collect::<Vec<GlobalVar>>(), vec![x, y]);

        assert_eq!(x.name().unwrap(), "x");
        assert_eq!(y.name().unwrap(), "y");

        assert_eq!(x.to_string(), "@x = external global i64");

        // global value
        assert_eq!(x.parent(), m);
        assert!(x.is_declaration());
        assert!(matches!(x.linkage(), LLVMLinkage::LLVMExternalLinkage));
        assert_eq!(x.section(), None);
        assert!(matches!(x.visibility(), LLVMVisibility::LLVMDefaultVisibility));
        assert!(matches!(
            x.dll_storage_class(),
            LLVMDLLStorageClass::LLVMDefaultStorageClass
        ));
        assert_eq!(x.unnamed_addr(), LLVMUnnamedAddr::LLVMNoUnnamedAddr);

        // initializer
        assert_eq!(x.initializer(), None);

        let v = i64_t.uint(123).into();

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

        x.set_unnamed_addr(LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
        assert_eq!(x.unnamed_addr(), LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);

        assert_eq!(x.to_string(), "@x = unnamed_addr global i64 123");
    }

    #[test]
    fn global_alias() {
        let c = Context::new();

        let m = c.create_module("test");
        assert!(!m.as_raw().is_null());

        let i8_t = c.int8_t();
        let i8_ptr_t = i8_t.ptr_t();

        let foo = m.add_global_var("foo", i8_t);
        assert_eq!(m.global_var("foo"), Some(foo));
        foo.set_initializer(i8_t.uint(42));

        let aliasee = foo.bit_cast(i8_ptr_t);

        let bar = m.add_global_alias(i8_ptr_t, aliasee, "bar");
        assert_eq!(m.global_alias("bar"), Some(bar));
        assert_eq!(bar.aliasee(), foo.into());

        assert_eq!(m.global_aliases().collect::<Vec<_>>(), vec![bar]);

        m.verify().unwrap();

        assert_eq!(
            m.to_string(),
            r#"; ModuleID = 'test'
source_filename = "test"

@foo = global i8 42

@bar = alias i8, i8* @foo
"#
        );
    }
}
