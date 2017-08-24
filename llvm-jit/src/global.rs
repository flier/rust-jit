use std::borrow::Cow;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use constant::Constant;
use function::Function;
use module::Module;
use utils::{AsBool, AsLLVMBool, AsRaw, FromRaw, UncheckedCStr};
use value::{AsValueRef, ValueRef};

pub type Linkage = LLVMLinkage;
pub type Visibility = LLVMVisibility;
pub type DLLStorageClass = LLVMDLLStorageClass;

impl GlobalValue for GlobalVar {}
impl GlobalValue for Function {}

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

    fn set_section(&self, section: &str) -> &Self {
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

    fn has_unnamed_addr(&self) -> bool {
        unsafe { LLVMHasUnnamedAddr(self.as_raw()) }.as_bool()
    }

    fn set_unnamed_addr(&self, has_unnamed_addr: bool) -> &Self {
        unsafe { LLVMSetUnnamedAddr(self.as_raw(), has_unnamed_addr.as_bool()) };
        self
    }
}

pub type ThreadLocalMode = LLVMThreadLocalMode;

/// Global Variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalVar(Constant);

inherit_from!(GlobalVar, Constant, ValueRef, LLVMValueRef);

impl GlobalVar {
    pub fn delete(&self) {
        unsafe { LLVMDeleteGlobal(self.as_raw()) }
    }

    pub fn initializer(&self) -> Option<Constant> {
        unsafe { LLVMGetInitializer(self.as_raw()) }.wrap()
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

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use prelude::*;
    use types::*;

    #[test]
    fn global_var() {
        let c = Context::new();
        let m = c.create_module("global_var");

        assert_eq!(m.get_global_var("x"), None);

        let i64_t = c.int64_t();
        let f64_t = c.double_t();

        m.add_global_var("x", i64_t);
        m.add_global_var("y", f64_t);

        let x = m.get_global_var("x").unwrap();
        let y = m.get_global_var("y").unwrap();

        assert_eq!(m.global_vars().collect::<Vec<GlobalVar>>(), vec![x, y]);

        assert_eq!(x.name().unwrap(), "x");
        assert_eq!(y.name().unwrap(), "y");

        assert_eq!(x.to_string(), "@x = external global i64");

        // global value
        assert_eq!(x.parent(), m);
        assert!(x.is_declaration());
        assert!(matches!(x.linkage(), LLVMLinkage::LLVMExternalLinkage));
        assert_eq!(x.section(), None);
        assert!(matches!(
            x.visibility(),
            LLVMVisibility::LLVMDefaultVisibility
        ));
        assert!(matches!(
            x.dll_storage_class(),
            LLVMDLLStorageClass::LLVMDefaultStorageClass
        ));
        assert!(!x.has_unnamed_addr());

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
    }
}
