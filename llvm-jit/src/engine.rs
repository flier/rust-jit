use std::ffi::CString;
use std::mem;
use std::ptr;

use libc;
use llvm::core::LLVMShutdown;
use llvm::execution_engine::*;

use errors::Result;
use function::Function;
use global::GlobalValue;
use module::Module;
use target::{TargetData, TargetMachine};
use types::TypeRef;
use utils::{AsLLVMBool, AsMutPtr, AsRaw, AsResult, DisposableMessage, FromRaw, IntoRaw,
            unchecked_cstring};

/// Deallocate and destroy all `ManagedStatic` variables.
pub fn shutdown() {
    unsafe { LLVMShutdown() }
}

/// The `GenericValue` class is used to represent an LLVM value of arbitrary type.
#[derive(Debug, PartialEq)]
pub struct GenericValue(LLVMGenericValueRef);

inherit_from!(GenericValue, LLVMGenericValueRef);

impl Drop for GenericValue {
    fn drop(&mut self) {
        unsafe { LLVMDisposeGenericValue(self.0) }
    }
}

impl GenericValue {
    pub fn from_uint<T: Into<TypeRef>>(ty: T, n: u64) -> Self {
        unsafe { LLVMCreateGenericValueOfInt(ty.into().as_raw(), n, false.as_bool()) }.into()
    }

    pub fn from_int<T: Into<TypeRef>>(ty: T, n: i64) -> Self {
        unsafe {
            LLVMCreateGenericValueOfInt(ty.into().as_raw(), mem::transmute(n), true.as_bool())
        }.into()
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

    pub fn to_uint(&self) -> u64 {
        unsafe { LLVMGenericValueToInt(self.0, false.as_bool()) }
    }

    pub fn to_int(&self) -> i64 {
        unsafe { mem::transmute(LLVMGenericValueToInt(self.0, true.as_bool())) }
    }

    pub fn to_ptr<T>(&self) -> *mut T {
        unsafe { LLVMGenericValueToPointer(self.0) as *mut T }
    }

    pub fn to_float<T: Into<TypeRef>>(&self, ty: T) -> f64 {
        unsafe { LLVMGenericValueToFloat(ty.into().as_raw(), self.0) }
    }
}

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
                format!(
                    "fail to create interpreter for Module({:?}), {}",
                    module,
                    err.into_string()
                ).into()
            })
            .map(|_| {
                trace!("create Interpreter({:?}) for Module({:?})", engine, module);

                Interpreter(ExecutionEngine(engine))
            })
    }
}

#[derive(Debug)]
pub struct JITCompiler(ExecutionEngine);

inherit_from!(JITCompiler, ExecutionEngine, LLVMExecutionEngineRef);

impl JITCompiler {
    pub fn for_module(module: Module, opt_level: u32) -> Result<Self> {
        let module = module.into_raw();
        let mut engine = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe { LLVMCreateJITCompilerForModule(&mut engine, module, opt_level, &mut err) }
            .ok_or_else(|| {
                format!(
                    "fail to create JITCompiler for Module({:?}), {}",
                    module,
                    err.into_string()
                ).into()
            })
            .map(|_| {
                trace!("create JITCompiler({:?}) for Module({:?})", engine, module);

                JITCompiler(ExecutionEngine(engine))
            })
    }
}

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
    ) -> Option<Self> {
        unsafe {
            LLVMCreateSimpleMCJITMemoryManager(
                data.as_mut_ptr() as *mut libc::c_void,
                alloc_code,
                alloc_data,
                free,
                destroy,
            )
        }.wrap()
    }

    /// Consumes the wrapper, returning the wrapped raw pointer.
    pub fn into_raw(self) -> LLVMMCJITMemoryManagerRef {
        let raw = self.as_raw();
        ::std::mem::forget(self);
        raw
    }
}

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
        }.ok_or_else(|| {
            format!(
                "fail to create MCJITCompiler for Module({:?}), {}",
                module,
                err.into_string()
            ).into()
        })
            .map(|_| {
                trace!(
                    "create MCJITCompiler({:?}) for Module({:?})",
                    engine,
                    module
                );

                MCJITCompiler(ExecutionEngine(engine))
            })
    }
}

/// Create an MCJIT execution engine for a module, with the given options.
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
        let module = module.into_raw();
        let mut engine = ptr::null_mut();
        let mut err = DisposableMessage::new();

        unsafe { LLVMCreateExecutionEngineForModule(&mut engine, module, &mut err) }
            .ok_or_else(|| {
                format!(
                    "fail to create execution engine for Module({:?}), {}",
                    module,
                    err.into_string()
                ).into()
            })
            .map(|_| {
                trace!(
                    "create ExecutionEngine({:?}) for Module({:?})",
                    engine,
                    module
                );

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
        let args = args.iter()
            .map(|arg| unchecked_cstring(arg))
            .collect::<Vec<CString>>();

        let mut argv = args.iter()
            .map(|arg| arg.as_ptr())
            .collect::<Vec<*const libc::c_char>>();

        argv.push(ptr::null());

        let env_vars = env_vars
            .iter()
            .map(|var| unchecked_cstring(var))
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
    pub fn run_function(&self, func: Function, args: &[GenericValue]) -> GenericValue {
        let mut args = args.iter()
            .map(|arg| arg.as_raw())
            .collect::<Vec<LLVMGenericValueRef>>();

        unsafe {
            LLVMRunFunction(
                self.as_raw(),
                func.as_raw(),
                args.len() as u32,
                args.as_mut_ptr(),
            )
        }.into()
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
            .ok_or_else(|| {
                format!("fail to remove {:?}, {}", module, err.into_string()).into()
            })
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
    pub fn add_global_mapping<V: GlobalValue, T>(&self, var: V, addr: *const T) {
        unsafe { LLVMAddGlobalMapping(self.as_raw(), var.as_raw(), addr as *mut libc::c_void) }
    }

    /// This returns the address of the specified global value.
    ///
    /// This may involve code generation if it's a function.
    pub fn get_ptr_to_global<V: GlobalValue, T>(&self, var: V) -> *mut T {
        unsafe { LLVMGetPointerToGlobal(self.as_raw(), var.as_raw()) as *mut T }
    }

    /// Return the address of the specified global value.
    ///
    /// This may involve code generation.
    pub fn get_global_value_address<S: AsRef<str>>(&self, name: S) -> Option<u64> {
        let name = name.as_ref();
        let addr = unsafe { LLVMGetGlobalValueAddress(self.0, cstr!(name)) };

        if addr == 0 {
            trace!("not found `{}` global value in {:?}", name, self);

            None
        } else {
            trace!(
                "found `{}` global value in {:?}, Address({:?})",
                name,
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
        let name = name.as_ref();
        let addr = unsafe { LLVMGetFunctionAddress(self.0, cstr!(name)) };

        if addr == 0 {
            trace!("not found `{}` function address in {:?}", name, self);

            None
        } else {
            trace!(
                "found `{}` function address in {:?}: Address({:p})",
                name,
                self,
                addr as *const u8
            );

            Some(addr)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use hamcrest::prelude::*;
    use llvm::prelude::*;
    use mmap;

    use super::*;
    use insts::*;
    use prelude::*;
    use target::{NativeAsmPrinter, NativeTarget};
    use types::*;
    use utils::{AsBool, UncheckedCStr};

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
        let x = f.get_param(0).unwrap();
        let y = f.get_param(1).unwrap();
        let z = f.get_param(2).unwrap();

        let sum = add(x, y, "sum.1").emit_to(&builder);
        let sum = add(sum, z, "sum.2").emit_to(&builder);

        // Emit a `ret` into the function
        ret!(sum).emit_to(&builder);

        // call with address
        assert_eq!(ee.find_function("sum"), Some(f));

        let addr = ee.get_function_address("sum").unwrap();

        let sum: extern "C" fn(u64, u64, u64) -> u64 = unsafe { mem::transmute(addr) };

        assert_eq!(sum(1, 2, 3), 6);
    }

    #[test]
    fn run_function_without_args() {
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
            ee.run_function(pi, &[]).to_float(f64_t),
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

        let argc = main.get_param(0).unwrap();

        builder <<= ret!(argc);

        let ee = ExecutionEngine::for_module(m).unwrap();

        assert_eq!(
            ee.run_function_as_main(main, &["123", "456", "789"], &[]),
            3
        );
    }

    #[test]
    fn global_vars() {
        let c = Context::new();
        let m = c.create_module("global_vars");

        let i64_t = c.int64_t();
        let x = m.add_global_var("x", i64_t);

        let ee = ExecutionEngine::for_module(m).unwrap();

        let mut v: i64 = 123;

        {
            ee.add_global_mapping(x, &mut v);
        }

        assert_eq!(unsafe { ptr::read::<i64>(ee.get_ptr_to_global(x)) }, 123);
        assert_eq!(ee.get_global_value_address("x").unwrap() as *const i64, &v);
    }

    struct MemorySection(mmap::MemoryMap);

    impl MemorySection {
        pub fn code(size: usize, alignment: usize) -> Self {
            let aligned = (size + alignment - 1) / alignment * alignment;

            MemorySection(
                mmap::MemoryMap::new(
                    aligned,
                    &[mmap::MapOption::MapWritable, mmap::MapOption::MapExecutable],
                ).unwrap(),
            )
        }

        pub fn data(size: usize, alignment: usize, is_read_only: bool) -> Self {
            let mut options = vec![mmap::MapOption::MapReadable];

            if !is_read_only {
                options.push(mmap::MapOption::MapWritable);
            };

            let aligned = (size + alignment - 1) / alignment * alignment;

            MemorySection(mmap::MemoryMap::new(aligned, &options).unwrap())
        }

        pub fn base(&self) -> *mut u8 {
            self.0.data()
        }
    }

    #[derive(Default)]
    struct MemoryManager {
        code_sections: Vec<MemorySection>,
        data_sections: Vec<MemorySection>,
    }

    extern "C" fn mm_alloc_code(
        data: *mut ::libc::c_void,
        size: ::libc::uintptr_t,
        alignment: ::libc::c_uint,
        section_id: ::libc::c_uint,
        section_name: *const ::libc::c_char,
    ) -> *mut u8 {
        let section = MemorySection::code(size as usize, alignment as usize);
        let p = section.base();

        trace!(
            "allocate {} bytes (align to {}) code section `{}` #{} @ {:?}",
            size,
            alignment,
            section_name.as_str(),
            section_id,
            p,
        );

        if let Some(mm) = unsafe { (data as *mut MemoryManager).as_mut() } {
            mm.code_sections.push(section);
        }

        p
    }
    extern "C" fn mm_alloc_data(
        data: *mut ::libc::c_void,
        size: ::libc::uintptr_t,
        alignment: ::libc::c_uint,
        section_id: ::libc::c_uint,
        section_name: *const ::libc::c_char,
        is_read_only: LLVMBool,
    ) -> *mut u8 {
        let section =
            MemorySection::data(size as usize, alignment as usize, is_read_only.as_bool());
        let p = section.base();

        trace!(
            "allocated {} bytes (align to {}) {} data section `{}` #{} @ {:?}",
            size,
            alignment,
            if is_read_only.as_bool() {
                "readonly"
            } else {
                "writable"
            },
            section_name.as_str(),
            section_id,
            p,
        );

        if let Some(mm) = unsafe { (data as *mut MemoryManager).as_mut() } {
            mm.data_sections.push(section);
        }

        p
    }
    extern "C" fn mm_finalize(
        opaque: *mut ::libc::c_void,
        err_msg: *mut *mut ::libc::c_char,
    ) -> LLVMBool {
        trace!("finalize memory @ {:?}", opaque);

        unsafe {
            *err_msg = ptr::null_mut();
        }

        true.as_bool()
    }
    extern "C" fn mm_destroy(opaque: *mut ::libc::c_void) {
        trace!("destroy memory @ {:?}", opaque);
    }

    #[test]
    fn memory_manager() {
        MCJITCompiler::link_in();
        NativeTarget::init().unwrap();
        NativeAsmPrinter::init().unwrap();

        let c = Context::new();
        let m = c.create_module("memory_manager");

        let mut opts = MCJITCompilerOptions::default();
        let mut ctxt = MemoryManager::default();
        let mm = MCJITMemoryManager::new(
            Some(&mut ctxt),
            mm_alloc_code,
            mm_alloc_data,
            mm_finalize,
            mm_destroy,
        ).unwrap();

        opts.MCJMM = mm.into_raw();

        let i64_t = c.int64_t();

        let f = m.add_function("test", FunctionType::new(i64_t, &[i64_t], false));

        let builder = c.create_builder();
        let bb = f.append_basic_block_in_context("entry", &c);
        builder.position_at_end(bb);

        let arg0_i64 = f.get_param(0).unwrap();

        let n = mul(arg0_i64, arg0_i64, "n").emit_to(&builder);

        let p = malloc!(i64_t; "p").emit_to(&builder);
        store(n, p).emit_to(&builder);
        free(p).emit_to(&builder);
        ret!(n).emit_to(&builder);

        let ee = MCJITCompiler::for_module(m, opts).unwrap();

        let addr = ee.get_function_address("test").unwrap();

        let test: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(addr) };

        assert_eq!(test(3), 9);
    }
}
