use std::borrow::Cow;
use std::mem;
use std::ptr;

use llvm::orc::*;

use errors::Result;
use module::Module;
use object::ObjectFile;
use target::TargetMachine;
use utils::{AsMutPtr, AsRaw, IntoRaw, UncheckedCStr};

pub type TargetAddress = LLVMOrcTargetAddress;
pub type LazyCompileCallback = LLVMOrcLazyCompileCallbackFn;
pub type SymbolResolver = LLVMOrcSymbolResolverFn;
pub type ModuleHandle = LLVMOrcModuleHandle;

#[derive(Debug)]
pub struct JITStack(LLVMOrcJITStackRef);

inherit_from!(JITStack, LLVMOrcJITStackRef);

impl Drop for JITStack {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMOrcDisposeInstance(self.as_raw()) }
    }
}

impl JITStack {
    /// Create an ORC JIT stack.
    pub fn new(tm: &TargetMachine) -> Self {
        let stack = unsafe { LLVMOrcCreateInstance(tm.as_raw()) }.into();

        trace!("create {:?} for {:?}", stack, tm);

        stack
    }

    /// Get the error message for the most recent error (if any).
    pub fn err_msg(&self) -> Cow<str> {
        unsafe { LLVMOrcGetErrorMsg(self.as_raw()) }.as_str()
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
    ) -> TargetAddress {
        trace!("create lazy compile callback @ {:?}", callback);

        unsafe {
            LLVMOrcCreateLazyCompileCallback(
                self.as_raw(),
                callback.unwrap_or_else(|| mem::zeroed()),
                ctx.as_mut_ptr(),
            )
        }
    }

    /// Create a named indirect call stub.
    pub fn create_indirect_stub<S: AsRef<str>>(&self, name: S, addr: TargetAddress) -> Result<()> {
        let name = name.as_ref();

        trace!("create indirect stub `{}` @ {}", name, addr);

        self.check_error_code(unsafe {
            LLVMOrcCreateIndirectStub(self.as_raw(), cstr!(name), addr)
        })
    }

    /// Set the pointer for the given indirect stub.
    pub fn set_indirect_stub_pointer<S: AsRef<str>>(
        &self,
        name: S,
        addr: TargetAddress,
    ) -> Result<()> {
        let name = name.as_ref();

        trace!("set indirect stub `{}` pointer to {}", name, addr);

        self.check_error_code(unsafe {
            LLVMOrcSetIndirectStubPointer(self.as_raw(), cstr!(name), addr)
        })
    }

    fn check_error_code(&self, err: LLVMOrcErrorCode) -> Result<()> {
        match err {
            LLVMOrcErrorCode::LLVMOrcErrSuccess => Ok(()),
            _ => bail!("ORC JIT error, {}", self.err_msg()),
        }
    }

    /// Add a module to be eagerly compiled.
    pub fn add_eagerly_compiled_ir<T>(
        &self,
        module: Module,
        resolver: Option<SymbolResolver>,
        ctx: Option<&mut T>,
    ) -> ModuleHandle {
        let module = module.into_raw();
        let handle = unsafe {
            LLVMOrcAddEagerlyCompiledIR(
                self.as_raw(),
                module,
                mem::transmute(resolver),
                ctx.as_mut_ptr(),
            )
        };

        trace!(
            "add eagerly compiled module {:?}, resolver = {:?}, handle = {}",
            module,
            resolver,
            handle
        );

        handle
    }

    /// Add a module to be lazily compiled.
    pub fn add_lazily_compiled_ir<T>(
        &self,
        module: Module,
        resolver: Option<SymbolResolver>,
        ctx: Option<&mut T>,
    ) -> ModuleHandle {
        let module = module.into_raw();
        let handle = unsafe {
            LLVMOrcAddLazilyCompiledIR(
                self.as_raw(),
                module,
                mem::transmute(resolver),
                ctx.as_mut_ptr(),
            )
        };

        trace!(
            "add lazily compiled module {:?}, resolver = {:?}, handle = {}",
            module,
            resolver,
            handle,
        );

        handle
    }

    /// Add an object file.
    pub fn add_object_file<T>(
        &self,
        obj: ObjectFile,
        resolver: Option<SymbolResolver>,
        ctx: Option<&mut T>,
    ) -> ModuleHandle {
        let obj = obj.into_raw();
        let handle = unsafe {
            LLVMOrcAddObjectFile(
                self.as_raw(),
                obj,
                mem::transmute(resolver),
                ctx.as_mut_ptr(),
            )
        };

        trace!(
            "add object file {:?}, resolver = {:?}, handle = {}",
            obj,
            resolver,
            handle,
        );

        handle
    }

    /// Remove a module set from the JIT.
    pub fn remove_module(&self, handle: ModuleHandle) {
        trace!("remove #{} module", handle);

        unsafe { LLVMOrcRemoveModule(self.as_raw(), handle) }
    }

    /// Get symbol address from JIT instance.
    pub fn get_symbol_address<S: AsRef<str>>(&self, symbol: S) -> Option<TargetAddress> {
        let symbol = symbol.as_ref();

        trace!("search symbol `{}` in JIT engine", symbol);

        match unsafe { LLVMOrcGetSymbolAddress(self.as_raw(), cstr!(symbol)) } {
            0 => None,
            addr => {
                trace!("got symbol `{}` from JIT engine @ {:?}", symbol, addr);

                Some(addr)
            }
        }
    }
}
