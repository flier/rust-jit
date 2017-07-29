use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use constant::Constant;
use utils::{AsBool, AsLLVMBool};
use value::ValueRef;

pub type ThreadLocalMode = LLVMThreadLocalMode;

/// Global Variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GlobalVar(ValueRef);

inherit_value_ref!(GlobalVar);

impl GlobalVar {
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
        unsafe { LLVMIsThreadLocal(self.as_raw()) }.as_bool()
    }

    pub fn set_thread_local(&self, is_thread_local: bool) {
        unsafe { LLVMSetThreadLocal(self.as_raw(), is_thread_local.as_bool()) }
    }

    pub fn is_global_constant(&self) -> bool {
        unsafe { LLVMIsGlobalConstant(self.as_raw()) }.as_bool()
    }

    pub fn set_global_constant(&self, is_global_constant: bool) {
        unsafe { LLVMSetGlobalConstant(self.as_raw(), is_global_constant.as_bool()) }
    }

    pub fn thread_local_mode(&self) -> ThreadLocalMode {
        unsafe { LLVMGetThreadLocalMode(self.as_raw()) }
    }

    pub fn set_thread_local_mode(&self, mode: ThreadLocalMode) {
        unsafe { LLVMSetThreadLocalMode(self.as_raw(), mode) }
    }

    pub fn is_externally_initialized(&self) -> bool {
        unsafe { LLVMIsExternallyInitialized(self.as_raw()) }.as_bool()
    }

    pub fn set_externally_initialized(&self, is_externally_initialized: bool) {
        unsafe { LLVMSetExternallyInitialized(self.as_raw(), is_externally_initialized.as_bool()) }
    }
}

#[cfg(test)]
mod tests {
    use llvm;

    use super::*;
    use context::Context;
    use module::Module;
    use prelude::*;
    use types::*;

    #[test]
    fn global_var() {
        let c = Context::new();
        let m = Module::with_name_in_context("test", &c);

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

        // set initializer
        assert_eq!(x.initializer(), None);

        let v = i64_t.uint(123);

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
