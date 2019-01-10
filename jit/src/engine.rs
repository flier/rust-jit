use std::ffi::CString;
use std::fmt;
use std::mem;
use std::ptr;

use crate::llvm::core::LLVMShutdown;
use crate::llvm::execution_engine::*;
use boolinator::Boolinator;
use libc;

use crate::errors::Result;
use crate::function::Function;
use crate::global::GlobalValue;
use crate::module::Module;
use crate::target::{self, TargetData, TargetMachine};
use crate::types::TypeRef;
use crate::utils::{unchecked_cstring, AsMutPtr, AsRaw, AsResult, DisposableMessage, IntoRaw, FALSE, TRUE};

/// Deallocate and destroy all `ManagedStatic` variables.
pub fn shutdown() {
    unsafe { LLVMShutdown() }
}

/// The `GenericValue` class is used to represent an LLVM value of arbitrary type.
#[repr(transparent)]
#[derive(Debug, PartialEq)]
pub struct GenericValue(LLVMGenericValueRef);

inherit_from!(GenericValue, LLVMGenericValueRef);

impl Drop for GenericValue {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeGenericValue(self.0) }
    }
}

impl GenericValue {
    pub fn from_uint<T: fmt::Debug + Into<TypeRef>>(ty: T, n: u64) -> Self {
        let ty = ty.into();
        let gv = unsafe { LLVMCreateGenericValueOfInt(ty.as_raw(), n, FALSE) }.into();

        trace!("{:?} from {} : {}", gv, n, ty);

        gv
    }

    pub fn from_int<T: fmt::Debug + Into<TypeRef>>(ty: T, n: i64) -> Self {
        let ty = ty.into();
        let gv = unsafe { LLVMCreateGenericValueOfInt(ty.as_raw(), mem::transmute(n), TRUE) }.into();

        trace!("{:?} from {} : {}", gv, n, ty);

        gv
    }

    pub fn from_ptr<T>(p: *const T) -> Self {
        let gv = unsafe { LLVMCreateGenericValueOfPointer(p as *mut libc::c_void) }.into();

        trace!("{:?} from pointer {:p}", gv, p);

        gv
    }

    pub fn from_float<T: Into<TypeRef>>(ty: T, n: f64) -> Self {
        let ty = ty.into();
        let gv = unsafe { LLVMCreateGenericValueOfFloat(ty.as_raw(), n) }.into();

        trace!("{:?} from {} : {}", gv, n, ty);

        gv
    }

    pub fn int_width(&self) -> u32 {
        unsafe { LLVMGenericValueIntWidth(self.0) as u32 }
    }

    pub fn to_uint(&self) -> u64 {
        unsafe { LLVMGenericValueToInt(self.0, FALSE) }
    }

    pub fn to_int(&self) -> i64 {
        unsafe { mem::transmute(LLVMGenericValueToInt(self.0, TRUE)) }
    }

    pub fn to_ptr<T>(&self) -> *mut T {
        unsafe { LLVMGenericValueToPointer(self.0) as *mut T }
    }

    pub fn to_float<T: Into<TypeRef>>(&self, ty: T) -> f64 {
        unsafe { LLVMGenericValueToFloat(ty.into().as_raw(), self.0) }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Interpreter(ExecutionEngine);

inherit_from!(Interpreter, ExecutionEngine, LLVMExecutionEngineRef);

impl Interpreter {
    pub fn link_in() {
        unsafe { LLVMLinkInInterpreter() }
    }

    pub fn for_module(module: Module) -> Result<Self> {
        let module = module.into_raw();
        let mut engine = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe { LLVMCreateInterpreterForModule(&mut engine, module, &mut err) }
            .ok_or_else(|| {
                format_err!(
                    "fail to create interpreter for Module({:?}), {}",
                    module,
                    err.into_string()
                )
            })
            .map(|_| {
                trace!("create Interpreter({:?}) for Module({:?})", engine, module);

                Interpreter(ExecutionEngine(engine))
            })
    }
}

/// Code generation optimization level.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CodeGenOptLevel {
    None = 0,       // -O0
    Less = 1,       // -O1
    Moderate = 2,   // -O2, -Os
    Aggressive = 3, // -O3
}

impl Default for CodeGenOptLevel {
    fn default() -> Self {
        CodeGenOptLevel::Moderate
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct JITCompiler(ExecutionEngine);

inherit_from!(JITCompiler, ExecutionEngine, LLVMExecutionEngineRef);

impl JITCompiler {
    pub fn for_module(module: Module, opt_level: CodeGenOptLevel) -> Result<Self> {
        let module = module.into_raw();
        let mut engine = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe { LLVMCreateJITCompilerForModule(&mut engine, module, opt_level as u32, &mut err) }
            .ok_or_else(|| {
                format_err!(
                    "fail to create JITCompiler for Module({:?}), {}",
                    module,
                    err.into_string()
                )
            })
            .map(|_| {
                trace!("create JITCompiler({:?}) for Module({:?})", engine, module);

                JITCompiler(ExecutionEngine(engine))
            })
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct MCJITMemoryManager(LLVMMCJITMemoryManagerRef);

inherit_from!(MCJITMemoryManager, LLVMMCJITMemoryManagerRef);

impl Drop for MCJITMemoryManager {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMCJITMemoryManager(self.0) }
    }
}

impl MCJITMemoryManager {
    pub fn new<T>(
        data: Option<&mut T>,
        alloc_code: LLVMMemoryManagerAllocateCodeSectionCallback,
        alloc_data: LLVMMemoryManagerAllocateDataSectionCallback,
        free: LLVMMemoryManagerFinalizeMemoryCallback,
        destroy: LLVMMemoryManagerDestroyCallback,
    ) -> Self {
        MCJITMemoryManager(unsafe {
            LLVMCreateSimpleMCJITMemoryManager(data.as_mut_ptr(), alloc_code, alloc_data, free, destroy)
        })
    }

    /// Consumes the wrapper, returning the wrapped raw pointer.
    pub fn into_raw(self) -> LLVMMCJITMemoryManagerRef {
        let raw = self.0;
        mem::forget(self);
        raw
    }
}

pub mod mmap {
    use std::alloc::Layout;
    use std::ops::Deref;
    use std::ptr::{self, NonNull};
    use std::slice;

    use boolinator::Boolinator;
    use llvm_sys::prelude::*;
    use memmap::{Mmap, MmapMut};

    use crate::engine::MCJITMemoryManager;
    use crate::errors::Result;
    use crate::utils::{AsBool, UncheckedCStr, TRUE};

    pub struct MemorySection(Option<State>);

    enum State {
        Init(Init),
        Finalized(Finalized),
    }

    enum Init {
        Code(MmapMut),
        Data(MmapMut, bool),
    }

    enum Finalized {
        Executable(Mmap),
        ReadOnly(Mmap),
        Writable(MmapMut),
    }

    impl Deref for MemorySection {
        type Target = [u8];

        fn deref(&self) -> &Self::Target {
            let (data, len) = match self.0.as_ref() {
                Some(State::Init(Init::Code(mm)))
                | Some(State::Init(Init::Data(mm, _)))
                | Some(State::Finalized(Finalized::Writable(mm))) => (mm.as_ptr(), mm.len()),
                Some(State::Finalized(Finalized::Executable(mm))) | Some(State::Finalized(Finalized::ReadOnly(mm))) => {
                    (mm.as_ptr(), mm.len())
                }
                None => unreachable!(),
            };

            unsafe { slice::from_raw_parts(data, len) }
        }
    }

    impl MemorySection {
        pub fn code(size: usize, alignment: usize) -> Result<Self> {
            let layout = Layout::from_size_align(size, alignment)?;
            let mm = MmapMut::map_anon(layout.size())?;

            Ok(MemorySection(Some(State::Init(Init::Code(mm)))))
        }

        pub fn data(size: usize, alignment: usize, read_only: bool) -> Result<Self> {
            let layout = Layout::from_size_align(size, alignment)?;
            let mm = MmapMut::map_anon(layout.size())?;

            Ok(MemorySection(Some(State::Init(Init::Data(mm, read_only)))))
        }

        pub fn finalize(&mut self) -> Result<()> {
            self.0 = match self.0.take() {
                Some(State::Init(Init::Code(mm))) => Some(State::Finalized(Finalized::Executable(mm.make_exec()?))),
                Some(State::Init(Init::Data(mm, read_only))) => {
                    if read_only {
                        Some(State::Finalized(Finalized::ReadOnly(mm.make_read_only()?)))
                    } else {
                        Some(State::Finalized(Finalized::Writable(mm)))
                    }
                }
                state => state,
            };

            Ok(())
        }

        pub fn begin(&self) -> *const u8 {
            self.as_ptr()
        }

        pub fn end(&self) -> *const u8 {
            unsafe { self.as_ptr().add(self.len()) }
        }

        pub fn contains<T>(&self, p: *const T) -> bool {
            let p = p as *const u8;

            self.begin() <= p && p < self.end()
        }
    }

    #[derive(Default)]
    pub struct MemoryManager {
        pub code_sections: Vec<MemorySection>,
        pub data_sections: Vec<MemorySection>,
    }

    impl From<&mut MemoryManager> for MCJITMemoryManager {
        fn from(mm: &mut MemoryManager) -> Self {
            MCJITMemoryManager::new(Some(mm), mm_alloc_code, mm_alloc_data, mm_finalize, Some(mm_destroy))
        }
    }

    extern "C" fn mm_alloc_code(
        data: *mut ::libc::c_void,
        size: ::libc::uintptr_t,
        alignment: ::libc::c_uint,
        section_id: ::libc::c_uint,
        section_name: *const ::libc::c_char,
    ) -> *mut u8 {
        if let Some(mm) = NonNull::new(data) {
            let section = MemorySection::code(size as usize, alignment as usize).unwrap();
            let p = section.as_ptr();

            trace!(
                "allocate {} bytes (align to {}) code section `{}` #{} @ {:?}, mm @ {:?}",
                size,
                alignment,
                section_name.as_str(),
                section_id,
                p,
                data,
            );

            unsafe {
                mm.cast::<MemoryManager>().as_mut().code_sections.push(section);
            }

            p as *mut _
        } else {
            ptr::null_mut()
        }
    }
    extern "C" fn mm_alloc_data(
        data: *mut ::libc::c_void,
        size: ::libc::uintptr_t,
        alignment: ::libc::c_uint,
        section_id: ::libc::c_uint,
        section_name: *const ::libc::c_char,
        is_read_only: LLVMBool,
    ) -> *mut u8 {
        if let Some(mm) = NonNull::new(data) {
            let section = MemorySection::data(size as usize, alignment as usize, is_read_only.as_bool()).unwrap();
            let p = section.as_ptr();

            trace!(
                "allocated {} bytes (align to {}) {} data section `{}` #{} @ {:?}, mm @ {:?}",
                size,
                alignment,
                is_read_only.as_bool().as_some("readonly").unwrap_or("writable"),
                section_name.as_str(),
                section_id,
                p,
                data,
            );

            unsafe {
                mm.cast::<MemoryManager>().as_mut().data_sections.push(section);
            }

            p as *mut _
        } else {
            ptr::null_mut()
        }
    }
    extern "C" fn mm_finalize(data: *mut ::libc::c_void, err_msg: *mut *mut ::libc::c_char) -> LLVMBool {
        trace!("finalize memory, mm @ {:?}", data);

        if let Some(mm) = NonNull::new(data) {
            let mut mm = mm.cast::<MemoryManager>();
            let mm = unsafe { mm.as_mut() };

            for section in &mut mm.code_sections {
                section.finalize().unwrap();
            }
            for section in &mut mm.data_sections {
                section.finalize().unwrap();
            }
        }

        unsafe {
            *err_msg = ptr::null_mut();
        }

        TRUE
    }
    extern "C" fn mm_destroy(data: *mut ::libc::c_void) {
        trace!("destroy memory, mm @ {:?}", data);

        if let Some(mm) = NonNull::new(data) {
            let mut mm = mm.cast::<MemoryManager>();

            unsafe {
                mm.as_mut().code_sections.clear();
                mm.as_mut().data_sections.clear();
            }
        }
    }
}

#[repr(transparent)]
pub struct MCJITCompilerOptions(LLVMMCJITCompilerOptions);

inherit_from!(MCJITCompilerOptions, LLVMMCJITCompilerOptions);

impl Default for MCJITCompilerOptions {
    fn default() -> Self {
        unsafe {
            let mut options: LLVMMCJITCompilerOptions = mem::uninitialized();

            LLVMInitializeMCJITCompilerOptions(&mut options, mem::size_of_val(&options));

            options.into()
        }
    }
}

pub type MCJIT = MCJITCompiler;

#[repr(transparent)]
#[derive(Debug)]
pub struct MCJITCompiler(ExecutionEngine);

inherit_from!(MCJITCompiler, ExecutionEngine, LLVMExecutionEngineRef);

impl MCJITCompiler {
    pub fn link_in() {
        unsafe { LLVMLinkInMCJIT() }
    }

    pub fn for_module(module: Module, mut options: MCJITCompilerOptions) -> Result<Self> {
        let module = module.into_raw();
        let mut engine = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe {
            LLVMCreateMCJITCompilerForModule(
                &mut engine,
                module,
                &mut options.0,
                mem::size_of::<LLVMMCJITCompilerOptions>(),
                &mut err,
            )
        }
        .ok_or_else(|| {
            format_err!(
                "fail to create MCJITCompiler for Module({:?}), {}",
                module,
                err.into_string()
            )
        })
        .map(|_| {
            trace!("create MCJITCompiler({:?}) for Module({:?})", engine, module);

            MCJITCompiler(ExecutionEngine(engine))
        })
    }
}

/// Create an MCJIT execution engine for a module, with the given options.
#[repr(transparent)]
#[derive(Debug)]
pub struct ExecutionEngine(LLVMExecutionEngineRef);

inherit_from!(ExecutionEngine, LLVMExecutionEngineRef);

unsafe impl Send for ExecutionEngine {}
unsafe impl Sync for ExecutionEngine {}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeExecutionEngine(self.0) }
    }
}

impl ExecutionEngine {
    pub fn for_module(module: Module) -> Result<Self> {
        target::init();

        let module = module.into_raw();
        let mut engine = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe { LLVMCreateExecutionEngineForModule(&mut engine, module, &mut err) }
            .ok_or_else(|| {
                format_err!(
                    "fail to create execution engine for Module({:?}), {}",
                    module,
                    err.into_string()
                )
            })
            .map(|_| {
                trace!("create ExecutionEngine({:?}) for Module({:?})", engine, module);

                engine.into()
            })
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

    /// This is a helper function which wraps runFunction to handle the common task of
    /// starting up main with the specified rgc, argv, and envp parameters.
    pub fn run_function_as_main(&self, func: Function, args: &[&str], env_vars: &[&str]) -> i32 {
        trace!(
            "run function {:?} as main with args {:?} and env {:?}",
            func,
            args,
            env_vars
        );

        let args = args
            .iter()
            .map(|s| unsafe { unchecked_cstring(s) })
            .collect::<Vec<CString>>();

        let mut argv = args
            .iter()
            .map(|arg| arg.as_ptr())
            .collect::<Vec<*const libc::c_char>>();

        argv.push(ptr::null());

        let env_vars = env_vars
            .iter()
            .map(|s| unsafe { unchecked_cstring(s) })
            .collect::<Vec<CString>>();

        let mut environ = env_vars
            .iter()
            .map(|var| var.as_ptr())
            .collect::<Vec<*const libc::c_char>>();

        environ.push(ptr::null());

        unsafe {
            LLVMRunFunctionAsMain(
                self.as_raw(),
                func.as_raw(),
                args.len() as u32,
                argv.as_ptr(),
                environ.as_ptr(),
            )
        }
    }

    /// Execute the specified function with the specified arguments, and return the result.
    ///
    /// For MCJIT execution engines, clients are encouraged to use the "GetFunctionAddress" method
    /// (rather than runFunction) and cast the returned uint64_t to the desired function pointer type.
    /// However, for backwards compatibility MCJIT's implementation can execute 'main-like' function
    /// (i.e. those returning void or int, and taking either no arguments or (int, char*[])).
    pub fn run_function(&self, func: &Function, args: Vec<GenericValue>) -> GenericValue {
        trace!("run function {:?} with args {:?}", func, args);

        let mut args = args
            .into_iter()
            .map(|arg| arg.into_raw())
            .collect::<Vec<LLVMGenericValueRef>>();
        let argc = args.len() as u32;
        let args = args.as_mut_slice();

        unsafe { LLVMRunFunction(self.as_raw(), func.as_raw(), argc, args.as_mut_ptr()) }.into()
    }

    /// Add a Module to the list of modules that we can JIT from.
    pub fn add_module(&self, module: Module) -> Module {
        trace!("add {:?} to {:?}", module, self);

        unsafe { LLVMAddModule(self.0, module.as_raw()) };

        module.into_raw().into()
    }

    /// Remove a Module from the list of modules.
    pub fn remove_module(&self, module: Module) -> Result<Module> {
        let module = module.into_raw();
        let mut out = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe { LLVMRemoveModule(self.0, module, &mut out, &mut err) }
            .ok_or_else(|| format_err!("fail to remove {:?}, {}", module, err.into_string()))
            .map(|_| {
                trace!("remove Module({:?}) from {:?}", module, self);

                module.into()
            })
    }

    /// Search all of the active modules to find the function that defines `FnName`.
    ///
    /// This is very slow operation and shouldn't be used for general code.
    pub fn find_function<S: AsRef<str>>(&self, name: S) -> Option<Function> {
        let name = name.as_ref();
        let mut func = ptr::null_mut();

        unsafe { LLVMFindFunction(self.0, cstr!(name), &mut func) }
            .ok()
            .map(|_| {
                let f = func.into();

                trace!("found `{}` function in {:?}: {:?}", name, self, f);

                f
            })
            .or_else(|| {
                trace!("not found `{}` function in {:?}", name, self);

                None
            })
    }

    pub fn target_data(&self) -> TargetData {
        unsafe { LLVMGetExecutionEngineTargetData(self.as_raw()) }.into()
    }

    pub fn target_machine(&self) -> TargetMachine {
        unsafe { LLVMGetExecutionEngineTargetMachine(self.as_raw()) }.into()
    }

    /// Tell the execution engine that the specified global is at the specified location.
    ///
    /// This is used internally as functions are JIT'd and as global variables are laid out in memory.
    /// It can and should also be used by clients of the EE
    /// that want to have an LLVM global overlay existing data in memory.
    /// Values to be mapped should be named, and have external or weak linkage.
    /// Mappings are automatically removed when their GlobalValue is destroyed.
    pub fn add_global_mapping<V: GlobalValue, T>(&self, var: &V, addr: *const T) {
        unsafe { LLVMAddGlobalMapping(self.as_raw(), var.as_raw(), addr as *mut libc::c_void) }
    }

    /// This returns the address of the specified global value.
    ///
    /// This may involve code generation if it's a function.
    pub fn ptr_to_global<V: GlobalValue, T>(&self, var: &V) -> *mut T {
        unsafe { LLVMGetPointerToGlobal(self.as_raw(), var.as_raw()) as *mut T }
    }

    /// Return the address of the specified global value.
    ///
    /// This may involve code generation.
    pub fn global_value_address<S: AsRef<str>>(&self, name: S) -> Option<u64> {
        let addr = unsafe { LLVMGetGlobalValueAddress(self.0, cstr!(name)) };

        (addr != 0).as_some(addr)
    }

    /// Return the address of the specified function.
    ///
    /// This may involve code generation.
    pub fn function_address<S: AsRef<str>>(&self, name: S) -> Option<u64> {
        let addr = unsafe { LLVMGetFunctionAddress(self.0, cstr!(name)) };

        (addr != 0).as_some(addr)
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use hamcrest::prelude::*;

    use super::*;
    use crate::insts::*;
    use crate::prelude::*;
    use crate::target::{NativeAsmPrinter, NativeTarget};
    use crate::types::*;

    #[test]
    fn generic_value() {
        let c = Context::new();

        let v = GenericValue::from_int(c.int64_t(), -123);

        assert_eq!(v.int_width(), 64);
        assert_eq!(v.to_int(), -123);

        let v = GenericValue::from_uint(c.int32_t(), 456);

        assert_eq!(v.int_width(), 32);
        assert_eq!(v.to_uint(), 456);

        let mut i = 123;
        let v = GenericValue::from_ptr(&i);

        assert_eq!(v.to_ptr(), &mut i as *mut i32);

        let double_t = c.double_t();
        let v = GenericValue::from_float(double_t, -123.0);

        assert_that!(v.to_float(double_t), is(close_to(-123.0, f64::EPSILON)));
    }

    #[test]
    fn module() {
        let c = Context::new();
        let m = c.create_module("module");

        // add it to our module
        let opts = MCJITCompilerOptions::default();
        let ee = MCJITCompiler::for_module(m, opts).unwrap();

        // module
        let test = c.create_module("test");

        let test = ee.add_module(test);

        assert_eq!(ee.remove_module(test).unwrap().name(), "test");
    }

    #[test]
    fn call_function_with_address() {
        MCJITCompiler::link_in();
        NativeTarget::init().unwrap();
        NativeAsmPrinter::init().unwrap();

        let c = Context::new();
        let m = c.create_module("call_function_with_address");

        // get a type for sum function
        let i64_t = c.int64_t();
        let argts = [i64_t, i64_t, i64_t];
        let function_type = FunctionType::new(i64_t, &argts, false);

        let f = m.add_function("sum", function_type);

        let builder = c.create_builder();
        let bb = f.append_basic_block_in_context("entry", &c);
        builder.position_at_end(bb);

        // add it to our module
        let opts = MCJITCompilerOptions::default();
        let ee = MCJITCompiler::for_module(m, opts).unwrap();

        // get the function's arguments
        let x = f.param(0).unwrap();
        let y = f.param(1).unwrap();
        let z = f.param(2).unwrap();

        let sum = add(x, y, "sum.1").emit_to(&builder);
        let sum = add(sum, z, "sum.2").emit_to(&builder);

        // Emit a `ret` into the function
        ret!(sum).emit_to(&builder);

        // call with address
        assert_eq!(ee.find_function("sum"), Some(f));

        let addr = ee.function_address("sum").unwrap();

        let sum: extern "C" fn(u64, u64, u64) -> u64 = unsafe { mem::transmute(addr) };

        assert_eq!(sum(1, 2, 3), 6);
    }

    #[test]
    fn run_function_without_args() {
        Interpreter::link_in();

        let c = Context::new();
        let m = c.create_module("run_function_without_args");

        let f64_t = c.double_t();
        let pi = m.add_function("pi", FunctionType::new(f64_t, &[], false));

        let builder = c.create_builder();
        let bb = pi.append_basic_block_in_context("entry", &c);
        builder.position_at_end(bb);

        ret!(f64_t.real(f64::consts::PI)).emit_to(&builder);

        // add it to our module
        let ee = Interpreter::for_module(m).unwrap();

        // run function
        assert_that!(
            ee.run_function(&pi, vec![]).to_float(f64_t),
            is(close_to(f64::consts::PI, f64::EPSILON))
        );
    }

    #[test]
    fn run_function_as_main() {
        let c = Context::new();
        let m = c.create_module("run_function_as_main");

        let i32_t = c.int32_t();
        let pp_char_t = c.int8_t().ptr_t().ptr_t();
        let main = m.add_function(
            "main",
            FunctionType::new(i32_t, types![i32_t, pp_char_t, pp_char_t], false),
        );

        let mut builder = c.create_builder();
        let bb = main.append_basic_block_in_context("entry", &c);
        builder.position_at_end(bb);

        let argc = main.param(0).unwrap();

        builder <<= ret!(argc);

        let ee = ExecutionEngine::for_module(m).unwrap();

        assert_eq!(ee.run_function_as_main(main, &["123", "456", "789"], &[]), 3);
    }

    #[test]
    fn global_vars() {
        let c = Context::new();
        let m = c.create_module("global_vars");

        let i64_t = c.int64_t();
        let x = m.add_global_var("x", i64_t);

        let ee = ExecutionEngine::for_module(m).unwrap();

        let v: i64 = 123;

        {
            ee.add_global_mapping(&x, &v);
        }

        assert_eq!(unsafe { ptr::read::<i64>(ee.ptr_to_global(&x)) }, 123);
        assert_eq!(ee.global_value_address("x").unwrap() as *const i64, &v);
    }

    #[test]
    fn memory_manager() {
        MCJITCompiler::link_in();
        NativeTarget::init().unwrap();
        NativeAsmPrinter::init().unwrap();

        let c = Context::new();
        let m = c.create_module("memory_manager");

        let i64_t = c.int64_t();

        let f = m.add_function("test", FunctionType::new(i64_t, &[i64_t], false));

        let builder = c.create_builder();
        let bb = f.append_basic_block_in_context("entry", &c);
        builder.position_at_end(bb);

        let arg0_i64 = f.param(0).unwrap();

        let n = mul(arg0_i64, arg0_i64, "n").emit_to(&builder);

        let p = malloc!(i64_t; "p").emit_to(&builder);
        store(n, p).emit_to(&builder);
        free(p).emit_to(&builder);
        ret!(n).emit_to(&builder);

        let mut opts = MCJITCompilerOptions::default();
        let mut mm = mmap::MemoryManager::default();
        opts.MCJMM = MCJITMemoryManager::from(&mut mm).into_raw();
        let ee = MCJITCompiler::for_module(m, opts).unwrap();

        let addr = ee.function_address("test").unwrap();

        let test: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(addr) };

        assert_eq!(test(3), 9);
    }
}
