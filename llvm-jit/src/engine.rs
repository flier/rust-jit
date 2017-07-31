use std::ffi::CStr;
use std::mem;
use std::ptr;

use libc;
use llvm::execution_engine::*;

use errors::Result;
use function::Function;
use module::Module;
use types::TypeRef;
use utils::{AsBool, AsLLVMBool, AsResult, unchecked_cstring};

#[derive(Debug, PartialEq)]
pub struct GenericValue(LLVMGenericValueRef);

inherit_from!(GenericValue, LLVMGenericValueRef);

impl Drop for GenericValue {
    fn drop(&mut self) {
        unsafe { LLVMDisposeGenericValue(self.0) }
    }
}

impl GenericValue {
    pub fn from_int<T: Into<TypeRef>>(ty: T, n: u64, is_signed: bool) -> Self {
        unsafe { LLVMCreateGenericValueOfInt(ty.into().as_raw(), n, is_signed.as_bool()) }.into()
    }

    pub fn from_ptr<T>(p: *const T) -> Self {
        unsafe { LLVMCreateGenericValueOfPointer(p as *mut libc::c_void) }.into()
    }

    pub fn from_float<T: Into<TypeRef>>(ty: T, n: f64) -> Self {
        unsafe { LLVMCreateGenericValueOfFloat(ty.into().as_raw(), n) }.into()
    }

    pub fn int_width(&self) -> u32 {
        unsafe { LLVMGenericValueIntWidth(self.0) as u32 }
    }

    pub fn to_uint(self) -> u64 {
        unsafe { LLVMGenericValueToInt(self.0, false.as_bool()) }
    }

    pub fn to_int(self) -> i64 {
        unsafe { mem::transmute(LLVMGenericValueToInt(self.0, true.as_bool())) }
    }

    pub fn to_ptr<T>(self) -> *mut T {
        unsafe { LLVMGenericValueToPointer(self.0) as *mut T }
    }

    pub fn to_float<T: Into<TypeRef>>(self, ty: T) -> f64 {
        unsafe { LLVMGenericValueToFloat(ty.into().as_raw(), self.0) }
    }
}

pub struct MCJIT;

impl MCJIT {
    pub fn init() {
        unsafe { LLVMLinkInMCJIT() }
    }
}

pub struct Interpreter;

impl Interpreter {
    pub fn init() {
        unsafe { LLVMLinkInInterpreter() }
    }
}

/// Create an MCJIT execution engine for a module, with the given options.
#[derive(Debug)]
pub struct ExecutionEngine(LLVMExecutionEngineRef);

impl ExecutionEngine {
    pub fn for_module(module: &Module) -> Result<Self> {
        unsafe {
            let mut engine = mem::uninitialized();
            let mut err = mem::zeroed();

            if LLVMCreateExecutionEngineForModule(&mut engine, module.as_raw(), &mut err).is_ok() {
                trace!("create ExecutionEngine({:?}) for {:?}", engine, module);

                Ok(ExecutionEngine(engine))
            } else {
                bail!(format!(
                    "fail to create execution engine for {:?}, {}",
                    module,
                    CStr::from_ptr(err).to_string_lossy()
                ))
            }
        }
    }

    /// Wrap a raw execution engine reference.
    pub fn from_raw(engine: LLVMExecutionEngineRef) -> Self {
        ExecutionEngine(engine)
    }

    /// Extracts the raw execution engine reference.
    pub fn as_raw(&self) -> LLVMExecutionEngineRef {
        self.0
    }

    /// This method is used to execute all of the static constructors for a program.
    pub fn run_static_constructors(&self) -> &Self {
        unsafe { LLVMRunStaticConstructors(self.0) }

        self
    }

    /// This method is used to execute all of the static destructors for a program.
    pub fn run_static_destructors(&self) -> &Self {
        unsafe { LLVMRunStaticDestructors(self.0) }

        self
    }

    /// Add a Module to the list of modules that we can JIT from.
    pub fn add_module(&self, module: &Module) -> &Self {
        trace!("add {:?} to {:?}", module, self);

        unsafe { LLVMAddModule(self.0, module.as_raw()) }

        self
    }

    /// Remove a Module from the list of modules.
    pub fn remove_module(&self, module: Module) -> Result<Module> {
        let mut out = ptr::null_mut();
        let mut err = ptr::null_mut();

        if unsafe { LLVMRemoveModule(self.0, module.as_raw(), &mut out, &mut err) }.is_ok() {
            trace!("remove {:?} from {:?}", module, self);

            Ok(Module::from_raw(out))
        } else {
            bail!(format!("fail to remove {:?}, {}", module, unsafe {
                CStr::from_ptr(err).to_string_lossy()
            }))
        }
    }

    /// Search all of the active modules to find the function that defines `FnName`.
    ///
    /// This is very slow operation and shouldn't be used for general code.
    pub fn find_function<S: AsRef<str>>(&self, name: S) -> Option<Function> {
        let cname = unchecked_cstring(name);
        let mut func = ptr::null_mut();

        if unsafe { LLVMFindFunction(self.0, cname.as_ptr(), &mut func) }.as_bool() {
            let f = func.into();

            trace!(
                "found `{}` function in {:?}: {:?}",
                cname.to_string_lossy(),
                self,
                f
            );

            Some(f)
        } else {
            trace!(
                "not found `{}` function in {:?}",
                cname.to_string_lossy(),
                self
            );

            None
        }
    }

    /// Return the address of the specified global value.
    ///
    /// This may involve code generation.
    pub fn get_global_value_address<S: AsRef<str>>(&self, name: S) -> Option<u64> {
        let cname = unchecked_cstring(name);
        let addr = unsafe { LLVMGetGlobalValueAddress(self.0, cname.as_ptr()) };

        if addr == 0 {
            trace!(
                "not found `{}` global value in {:?}",
                cname.to_string_lossy(),
                self
            );

            None
        } else {
            trace!(
                "found `{}` global value in {:?}, Address({:?})",
                cname.to_string_lossy(),
                self,
                addr as *const u8
            );

            Some(addr)
        }
    }

    /// Return the address of the specified function.
    ///
    /// This may involve code generation.
    pub fn get_function_address<S: AsRef<str>>(&self, name: S) -> Option<u64> {
        let cname = unchecked_cstring(name);
        let addr = unsafe { LLVMGetFunctionAddress(self.0, cname.as_ptr()) };

        if addr == 0 {
            trace!(
                "not found `{}` function address in {:?}",
                cname.to_string_lossy(),
                self
            );

            None
        } else {
            trace!(
                "found `{}` function address in {:?}: Address({:p})",
                cname.to_string_lossy(),
                self,
                addr as *const u8
            );

            Some(addr)
        }
    }
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeExecutionEngine(self.0) }
    }
}
