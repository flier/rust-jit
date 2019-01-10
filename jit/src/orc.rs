use std::borrow::Cow;
use std::mem;
use std::ptr;
use std::result::Result as StdResult;

use crate::llvm::orc::*;
use boolinator::Boolinator;
use failure::Error;

use crate::errors::Result;
use crate::membuf::MemoryBuffer;
use crate::module::Module;
use crate::target::TargetMachine;
use crate::utils::{AsMutPtr, AsRaw, AsResult, IntoRaw, UncheckedCStr};

pub type TargetAddress = LLVMOrcTargetAddress;
pub type LazyCompileCallback = LLVMOrcLazyCompileCallbackFn;
pub type SymbolResolver = LLVMOrcSymbolResolverFn;
pub type ModuleHandle = LLVMOrcModuleHandle;

impl AsResult<()> for LLVMOrcErrorCode {
    fn is_ok(self) -> bool {
        self == LLVMOrcErrorCode::LLVMOrcErrSuccess
    }

    fn ok_or_else<F, E>(self, err: F) -> StdResult<(), E>
    where
        F: FnOnce() -> E,
    {
        if self.is_ok() {
            Ok(())
        } else {
            Err(err())
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct JITStack(LLVMOrcJITStackRef);

inherit_from!(JITStack, LLVMOrcJITStackRef);

impl Drop for JITStack {
    fn drop(&mut self) {
        let _ = self.dispose();
    }
}

impl JITStack {
    /// Create an ORC JIT stack.
    pub fn new(tm: &TargetMachine) -> Self {
        let stack = unsafe { LLVMOrcCreateInstance(tm.as_raw()) }.into();

        trace!("create {:?} for {:?}", stack, tm);

        stack
    }

    /// Dispose of an ORC JIT stack.
    pub fn dispose(&self) -> Result<()> {
        trace!("dispose {:?}", self);

        unsafe { LLVMOrcDisposeInstance(self.as_raw()) }.ok_or_else(|| self.err())
    }

    /// Get the error message for the most recent error (if any).
    pub fn err_msg(&self) -> Cow<str> {
        unsafe { LLVMOrcGetErrorMsg(self.as_raw()) }.as_str()
    }

    pub fn err(&self) -> Error {
        format_err!("ORC JIT error, {}", self.err_msg())
    }

    /// Mangle the given symbol.
    pub fn mangled_symbol<S: AsRef<str>>(&self, symbol: S) -> String {
        let symbol = symbol.as_ref();
        let mut mangled = ptr::null_mut();

        unsafe { LLVMOrcGetMangledSymbol(self.as_raw(), &mut mangled, cstr!(symbol)) }

        let s = mangled.as_str().into();

        unsafe { LLVMOrcDisposeMangledSymbol(mangled) }

        trace!("symbol `{}` mangled to `{}`", symbol, s);

        s
    }

    /// Create a lazy compile callback.
    pub fn create_lazy_compile_callback<T>(
        &self,
        callback: Option<LLVMOrcLazyCompileCallbackFn>,
        ctx: Option<&mut T>,
    ) -> Result<TargetAddress> {
        let mut addr = TargetAddress::default();

        trace!("create lazy compile callback @ {:?}", callback);

        unsafe {
            LLVMOrcCreateLazyCompileCallback(
                self.as_raw(),
                &mut addr,
                callback.unwrap_or_else(|| mem::zeroed()),
                ctx.as_mut_ptr(),
            )
        }
        .ok_or_else(|| self.err())
        .map(|_| addr)
    }

    /// Create a named indirect call stub.
    pub fn create_indirect_stub<S: AsRef<str>>(&self, name: S, addr: TargetAddress) -> Result<()> {
        let name = name.as_ref();

        trace!("create indirect stub `{}` @ {}", name, addr);

        unsafe { LLVMOrcCreateIndirectStub(self.as_raw(), cstr!(name), addr) }.ok_or_else(|| self.err())
    }

    /// Set the pointer for the given indirect stub.
    pub fn set_indirect_stub_pointer<S: AsRef<str>>(&self, name: S, addr: TargetAddress) -> Result<()> {
        let name = name.as_ref();

        trace!("set indirect stub `{}` pointer to {}", name, addr);

        unsafe { LLVMOrcSetIndirectStubPointer(self.as_raw(), cstr!(name), addr) }.ok_or_else(|| self.err())
    }

    /// Add a module to be eagerly compiled.
    pub fn add_eagerly_compiled_ir<T>(
        &self,
        module: Module,
        resolver: SymbolResolver,
        ctx: Option<&mut T>,
    ) -> Result<ModuleHandle> {
        let mut handle = ModuleHandle::default();

        unsafe {
            LLVMOrcAddEagerlyCompiledIR(
                self.as_raw(),
                &mut handle,
                module.into_raw(),
                resolver,
                ctx.as_mut_ptr(),
            )
        }
        .ok_or_else(|| self.err())
        .map(|_| handle)
    }

    /// Add a module to be lazily compiled.
    pub fn add_lazily_compiled_ir<T>(
        &self,
        module: Module,
        resolver: SymbolResolver,
        ctx: Option<&mut T>,
    ) -> Result<ModuleHandle> {
        let mut handle = ModuleHandle::default();

        unsafe {
            LLVMOrcAddLazilyCompiledIR(
                self.as_raw(),
                &mut handle,
                module.into_raw(),
                resolver,
                ctx.as_mut_ptr(),
            )
        }
        .ok_or_else(|| self.err())
        .map(|_| handle)
    }

    /// Add an object file.
    pub fn add_object_file<T>(
        &self,
        obj: MemoryBuffer,
        resolver: SymbolResolver,
        ctx: Option<&mut T>,
    ) -> Result<ModuleHandle> {
        let mut handle = ModuleHandle::default();

        unsafe { LLVMOrcAddObjectFile(self.as_raw(), &mut handle, obj.into_raw(), resolver, ctx.as_mut_ptr()) }
            .ok_or_else(|| self.err())
            .map(|_| handle)
    }

    /// Remove a module set from the JIT.
    pub fn remove_module(&self, handle: ModuleHandle) -> Result<()> {
        trace!("remove #{} module", handle);

        unsafe { LLVMOrcRemoveModule(self.as_raw(), handle) }.ok_or_else(|| self.err())
    }

    /// Get symbol address from JIT instance.
    pub fn symbol_address<S: AsRef<str>>(&self, symbol: S) -> Result<Option<TargetAddress>> {
        let mut addr = TargetAddress::default();
        let symbol = symbol.as_ref();

        trace!("search symbol `{}` in JIT engine", symbol);

        unsafe { LLVMOrcGetSymbolAddress(self.as_raw(), &mut addr, cstr!(symbol)) }
            .ok_or_else(|| self.err())
            .map(|_| (addr != 0).as_some(addr))
    }
}
