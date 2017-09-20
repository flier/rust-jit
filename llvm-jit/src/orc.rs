use std::borrow::Cow;
use std::mem;
use std::ptr;

use llvm::orc::*;
use llvm::prelude::*;

use errors::{Error, Result};
use module::Module;
use object::ObjectFile;
use target::TargetMachine;
use utils::{AsMutPtr, AsRaw, AsResult, IntoRaw, UncheckedCStr};

pub type TargetAddress = LLVMOrcTargetAddress;
pub type LazyCompileCallback = LLVMOrcLazyCompileCallbackFn;
pub type SymbolResolver = LLVMOrcSymbolResolverFn;
pub type ModuleHandle = LLVMOrcModuleHandle;

impl AsResult for LLVMOrcErrorCode {
    fn is_ok(self) -> bool {
        self == LLVMOrcErrorCode::LLVMOrcErrSuccess
    }
}

#[derive(Debug)]
pub struct SharedModule(LLVMSharedModuleRef);

inherit_from!(SharedModule, LLVMSharedModuleRef);

impl<T: AsRaw<RawType = LLVMModuleRef>> From<T> for SharedModule {
    fn from(module: T) -> Self {
        SharedModule(unsafe { LLVMOrcMakeSharedModule(module.as_raw()) })
    }
}

impl Drop for SharedModule {
    fn drop(&mut self) {
        unsafe { LLVMOrcDisposeSharedModuleRef(self.0) }
    }
}

#[derive(Debug)]
pub struct SharedObjectBuffer(LLVMSharedObjectBufferRef);

inherit_from!(SharedObjectBuffer, LLVMSharedObjectBufferRef);

impl<T: AsRaw<RawType = LLVMMemoryBufferRef>> From<T> for SharedObjectBuffer {
    fn from(buf: T) -> Self {
        SharedObjectBuffer(unsafe { LLVMOrcMakeSharedObjectBuffer(buf.as_raw()) })
    }
}

impl Drop for SharedObjectBuffer {
    fn drop(&mut self) {
        unsafe { LLVMOrcDisposeSharedObjectBufferRef(self.0) }
    }
}

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
        format!("ORC JIT error, {}", self.err_msg()).into()
    }

    /// Mangle the given symbol.
    pub fn get_mangled_symbol<S: AsRef<str>>(&self, symbol: S) -> String {
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
        }.ok_or_else(|| self.err())
            .map(|_| addr)
    }

    /// Create a named indirect call stub.
    pub fn create_indirect_stub<S: AsRef<str>>(&self, name: S, addr: TargetAddress) -> Result<()> {
        let name = name.as_ref();

        trace!("create indirect stub `{}` @ {}", name, addr);

        unsafe { LLVMOrcCreateIndirectStub(self.as_raw(), cstr!(name), addr) }
            .ok_or_else(|| self.err())
    }

    /// Set the pointer for the given indirect stub.
    pub fn set_indirect_stub_pointer<S: AsRef<str>>(
        &self,
        name: S,
        addr: TargetAddress,
    ) -> Result<()> {
        let name = name.as_ref();

        trace!("set indirect stub `{}` pointer to {}", name, addr);

        unsafe { LLVMOrcSetIndirectStubPointer(self.as_raw(), cstr!(name), addr) }
            .ok_or_else(|| self.err())
    }

    /// Add a module to be eagerly compiled.
    pub fn add_eagerly_compiled_ir<T>(
        &self,
        module: Module,
        resolver: Option<SymbolResolver>,
        ctx: Option<&mut T>,
    ) -> Result<ModuleHandle> {
        let mut handle = ModuleHandle::default();
        let module = module.into_raw();

        unsafe {
            LLVMOrcAddEagerlyCompiledIR(
                self.as_raw(),
                &mut handle,
                module,
                mem::transmute(resolver),
                ctx.as_mut_ptr(),
            )
        }.ok_or_else(|| self.err())
            .map(|_| handle)
    }

    /// Add a module to be lazily compiled.
    pub fn add_lazily_compiled_ir<T>(
        &self,
        module: Module,
        resolver: Option<SymbolResolver>,
        ctx: Option<&mut T>,
    ) -> Result<ModuleHandle> {
        let mut handle = ModuleHandle::default();
        let module = module.into_raw();

        unsafe {
            LLVMOrcAddLazilyCompiledIR(
                self.as_raw(),
                &mut handle,
                module,
                mem::transmute(resolver),
                ctx.as_mut_ptr(),
            )
        }.ok_or_else(|| self.err())
            .map(|_| handle)
    }

    /// Add an object file.
    pub fn add_object_file<T>(
        &self,
        obj: ObjectFile,
        resolver: Option<SymbolResolver>,
        ctx: Option<&mut T>,
    ) -> Result<ModuleHandle> {
        let mut handle = ModuleHandle::default();
        let obj = obj.into_raw();

        unsafe {
            LLVMOrcAddObjectFile(
                self.as_raw(),
                &mut handle,
                obj,
                mem::transmute(resolver),
                ctx.as_mut_ptr(),
            )
        }.ok_or_else(|| self.err())
            .map(|_| handle)
    }

    /// Remove a module set from the JIT.
    pub fn remove_module(&self, handle: ModuleHandle) -> Result<()> {
        trace!("remove #{} module", handle);

        unsafe { LLVMOrcRemoveModule(self.as_raw(), handle) }.ok_or_else(|| self.err())
    }

    /// Get symbol address from JIT instance.
    pub fn get_symbol_address<S: AsRef<str>>(&self, symbol: S) -> Result<Option<TargetAddress>> {
        let mut addr = TargetAddress::default();
        let symbol = symbol.as_ref();

        trace!("search symbol `{}` in JIT engine", symbol);

        unsafe { LLVMOrcGetSymbolAddress(self.as_raw(), &mut addr, cstr!(symbol)) }
            .ok_or_else(|| self.err())
            .map(|_| if addr == 0 { None } else { Some(addr) })
    }
}
