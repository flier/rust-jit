use std::borrow::Cow;
use std::ffi::CStr;
use std::ptr;

use libc::c_void;

use llvm::object::LLVMObjectFileRef;
use llvm::orc::*;
use llvm::prelude::LLVMModuleRef;
use llvm::target_machine::LLVMTargetMachineRef;

use errors::Result;
use utils::{AsMutPtr, AsRaw, unchecked_cstring};

pub type TargetAddress = LLVMOrcTargetAddress;
pub type LazyCompileCallback = LLVMOrcLazyCompileCallbackFn;
pub type SymbolResolver = LLVMOrcSymbolResolverFn;
pub type ModuleHandle = LLVMOrcModuleHandle;

#[derive(Debug)]
pub struct JITStack(LLVMOrcJITStackRef);

inherit_from!(JITStack, LLVMOrcJITStackRef);

impl Drop for JITStack {
    fn drop(&mut self) {
        unsafe { LLVMOrcDisposeInstance(self.as_raw()) }
    }
}

impl JITStack {
    /// Create an ORC JIT stack.
    pub fn new<T>(tm: T) -> Self
    where
        T: AsRaw<RawType = LLVMTargetMachineRef>,
    {
        unsafe { LLVMOrcCreateInstance(tm.as_raw()) }.into()
    }

    /// Get the error message for the most recent error (if any).
    pub fn err_msg(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMOrcGetErrorMsg(self.as_raw())).to_string_lossy() }
    }

    /// Mangle the given symbol.
    pub fn mangled_symbol<S: AsRef<str>>(&self, symbol: S) -> String {
        let mut mangled = ptr::null_mut();

        unsafe {
            LLVMOrcGetMangledSymbol(
                self.as_raw(),
                &mut mangled,
                unchecked_cstring(symbol).as_ptr(),
            );

            let s = String::from(CStr::from_ptr(mangled).to_string_lossy());

            LLVMOrcDisposeMangledSymbol(mangled);

            s
        }
    }

    /// Create a lazy compile callback.
    pub fn create_lazy_compile_callback<T>(
        &self,
        callback: LLVMOrcLazyCompileCallbackFn,
        ctx: Option<&mut T>,
    ) -> TargetAddress {
        unsafe {
            LLVMOrcCreateLazyCompileCallback(
                self.as_raw(),
                callback,
                ctx.as_mut_ptr() as *mut c_void,
            )
        }
    }

    /// Create a named indirect call stub.
    pub fn create_indirect_stub<S: AsRef<str>>(&self, name: S, addr: TargetAddress) -> Result<()> {
        self.check_error_code(unsafe {
            LLVMOrcCreateIndirectStub(self.as_raw(), unchecked_cstring(name).as_ptr(), addr)
        })
    }

    /// Set the pointer for the given indirect stub.
    pub fn set_indirect_stub_pointer<S: AsRef<str>>(
        &self,
        name: S,
        addr: TargetAddress,
    ) -> Result<()> {
        self.check_error_code(unsafe {
            LLVMOrcSetIndirectStubPointer(self.as_raw(), unchecked_cstring(name).as_ptr(), addr)
        })
    }

    fn check_error_code(&self, err: LLVMOrcErrorCode) -> Result<()> {
        match err {
            LLVMOrcErrorCode::LLVMOrcErrSuccess => Ok(()),
            _ => bail!("ORC JIT error, {}", self.err_msg()),
        }
    }

    /// Add a module to be eagerly compiled.
    pub fn add_eagerly_compiled_ir<M, T>(
        &self,
        module: M,
        resolver: SymbolResolver,
        ctx: Option<&mut T>,
    ) -> ModuleHandle
    where
        M: AsRaw<RawType = LLVMModuleRef>,
    {
        unsafe {
            LLVMOrcAddEagerlyCompiledIR(
                self.as_raw(),
                module.as_raw(),
                resolver,
                ctx.as_mut_ptr() as *mut c_void,
            )
        }
    }

    /// Add a module to be eagerly compiled.
    pub fn add_lazily_compiled_ir<M, T>(
        &self,
        module: M,
        resolver: SymbolResolver,
        ctx: Option<&mut T>,
    ) -> ModuleHandle
    where
        M: AsRaw<RawType = LLVMModuleRef>,
    {
        unsafe {
            LLVMOrcAddLazilyCompiledIR(
                self.as_raw(),
                module.as_raw(),
                resolver,
                ctx.as_mut_ptr() as *mut c_void,
            )
        }
    }

    /// Add an object file.
    pub fn add_object_file<O, T>(
        &self,
        obj: O,
        resolver: SymbolResolver,
        ctx: Option<&mut T>,
    ) -> ModuleHandle
    where
        O: AsRaw<RawType = LLVMObjectFileRef>,
    {
        unsafe {
            LLVMOrcAddObjectFile(
                self.as_raw(),
                obj.as_raw(),
                resolver,
                ctx.as_mut_ptr() as *mut c_void,
            )
        }
    }

    /// Remove a module set from the JIT.
    pub fn remove_module(&self, handle: ModuleHandle) {
        unsafe { LLVMRemoveModule(self.as_raw(), handle) }
    }

    /// Get symbol address from JIT instance.
    pub fn get_symbol_address<S: AsRef<str>>(&self, symbol: S) -> TargetAddress {
        unsafe { LLVMOrcGetSymbolAddress(self.as_raw(), unchecked_cstring(symbol).as_ptr()) }
    }
}
