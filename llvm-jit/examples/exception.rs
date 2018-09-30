#![allow(unused_variables)]

//===----------------------------------------------------------------------===//
//
// Demo program which implements an example LLVM exception implementation, and
// shows several test cases including the handling of foreign exceptions.
// It is run with type info types arguments to throw. A test will
// be run for each given type info type. While type info types with the value
// of -1 will trigger a foreign C++ exception to be thrown; type info types
// <= 6 and >= 1 will cause the associated generated exceptions to be thrown
// and caught by generated test functions; and type info types > 6
// will result in exceptions which pass through to the test harness. All other
// type info types are not supported and could cause a crash. In all cases,
// the "finally" blocks of every generated test functions will executed
// regardless of whether or not that test function ignores or catches the
// thrown exception.
//
// examples:
//
// ExceptionDemo
//
//     causes a usage to be printed to stderr
//
// ExceptionDemo 2 3 7 -1
//
//     results in the following cases:
//         - Value 2 causes an exception with a type info type of 2 to be
//           thrown and caught by an inner generated test function.
//         - Value 3 causes an exception with a type info type of 3 to be
//           thrown and caught by an outer generated test function.
//         - Value 7 causes an exception with a type info type of 7 to be
//           thrown and NOT be caught by any generated function.
//         - Value -1 causes a foreign C++ exception to be thrown and not be
//           caught by any generated function
//
//     Cases -1 and 7 are caught by a C++ test harness where the validity of
//         of a C++ catch(...) clause catching a generated exception with a
//         type info type of 7 is explained by: example in rules 1.6.4 in
//         http://mentorembedded.github.com/cxx-abi/abi-eh.html (v1.22)
//
// This code uses code from the llvm compiler-rt project and the llvm
// Kaleidoscope project.
//
//===----------------------------------------------------------------------===//

extern crate libc;
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_sys as llvm;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use std::env;
use std::ffi::CStr;
use std::mem;
use std::panic;

use jit::insts::*;
use jit::prelude::*;
use jit::{ConstantInt, FunctionPassManager, StructType};

const USE_GLOBAL_STR_CONSTS: bool = true;
const OUR_BASE_EXCEPTION_CLASS: u64 = 0;

/// This is our simplistic type info
#[repr(C)]
pub struct OurExceptionType_t {
    /// type info type
    type_id: i32,
}

/// This is our Exception class which relies on a negative offset to calculate
/// pointers to its instances from pointers to its unwind_exception member.
///
/// Note: The above unwind.h defines struct _Unwind_Exception to be aligned
///       on a double word boundary. This is necessary to match the standard:
///       http://mentorembedded.github.com/cxx-abi/abi-eh.html
#[repr(C)]
pub struct OurBaseException_t {
    exception_type: OurExceptionType_t,

    // Note: This is properly aligned in unwind.h
    unwind_exception: _Unwind_Exception,
}

pub type OurException = OurBaseException_t;
pub type OurUnwindException = _Unwind_Exception;

impl OurBaseException_t {
    fn base_from_unwind_offset() -> isize {
        let dummy_exception: OurBaseException_t = unsafe { mem::zeroed() };

        unsafe {
            mem::transmute::<_, isize>(&dummy_exception.unwind_exception) - mem::transmute::<_, isize>(&dummy_exception)
        }
    }
}

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum UnwindReasonCode {
    _URC_NO_REASON = 0,
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
    _URC_FATAL_PHASE2_ERROR = 2,
    _URC_FATAL_PHASE1_ERROR = 3,
    _URC_NORMAL_STOP = 4,
    _URC_END_OF_STACK = 5,
    _URC_HANDLER_FOUND = 6,
    _URC_INSTALL_CONTEXT = 7,
    _URC_CONTINUE_UNWIND = 8,
    _URC_FAILURE = 9, // used only by ARM EHABI
}

pub type UnwindExceptionClass = u64;
pub type UnwindWord = libc::uintptr_t;
pub type UnwindPtr = libc::uintptr_t;
pub type UnwindTraceFn = extern "C" fn(ctx: *mut UnwindContext, arg: *mut libc::c_void) -> UnwindReasonCode;
#[cfg(target_arch = "x86")]
pub const UNWINDER_PRIVATE_DATA_SIZE: usize = 5;

#[cfg(target_arch = "x86_64")]
pub const UNWINDER_PRIVATE_DATA_SIZE: usize = 6;

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum _Unwind_Action {
    _UA_SEARCH_PHASE = 1,
    _UA_CLEANUP_PHASE = 2,
    _UA_HANDLER_FRAME = 4,
    _UA_FORCE_UNWIND = 8,
    _UA_END_OF_STACK = 16,
}

#[repr(C)]
pub struct _Unwind_Exception {
    pub exception_class: UnwindExceptionClass,
    pub exception_cleanup: UnwindExceptionCleanupFn,
    pub private: [UnwindWord; UNWINDER_PRIVATE_DATA_SIZE],
}

pub enum UnwindContext {}

pub type UnwindExceptionCleanupFn = extern "C" fn(unwind_code: UnwindReasonCode, exception: *mut _Unwind_Exception);
extern "C" {
    pub fn _Unwind_Resume(exception: *mut _Unwind_Exception) -> !;
    pub fn _Unwind_DeleteException(exception: *mut _Unwind_Exception);
    pub fn _Unwind_GetLanguageSpecificData(ctx: *mut UnwindContext) -> *mut libc::c_void;
    pub fn _Unwind_GetRegionStart(ctx: *mut UnwindContext) -> UnwindPtr;
    pub fn _Unwind_GetTextRelBase(ctx: *mut UnwindContext) -> UnwindPtr;
    pub fn _Unwind_GetDataRelBase(ctx: *mut UnwindContext) -> UnwindPtr;
}

// Note: Better ways to decide on bit width
//
/// Prints a 32 bit number, according to the format, to stderr.
/// @param intToPrint integer to print
/// @param format printf like format to use when printing
extern "C" fn print_int32(i: i32, f: *const libc::c_char) {
    if !f.is_null() {
        print!(
            "{}",
            unsafe { CStr::from_ptr(f).to_string_lossy() }.replace("{}", &i.to_string())
        )
    } else {
        println!("::print32_int(...):NULL arg.")
    }
}

// Note: Better ways to decide on bit width
//
/// Prints a 64 bit number, according to the format, to stderr.
/// @param intToPrint integer to print
/// @param format printf like format to use when printing
extern "C" fn print_int64(i: i64, f: *const libc::c_char) {
    if !f.is_null() {
        print!(
            "{}",
            unsafe { CStr::from_ptr(f).to_string_lossy() }.replace("{}", &i.to_string())
        )
    } else {
        println!("::print64_int(...):NULL arg.")
    }
}

/// Prints a C string to stderr
/// @param to_print string to print
extern "C" fn print_str(s: *const libc::c_char) {
    if !s.is_null() {
        print!("{}", unsafe { &CStr::from_ptr(s).to_string_lossy() })
    } else {
        println!("::print_str(...):NULL arg.")
    }
}

/// Deletes the true previously allocated exception whose address
/// is calculated from the supplied OurBaseException_t::unwind_exception
/// member address. Handles (ignores), NULL pointers.
/// @param expToDelete exception to delete
extern "C" fn delete_our_exception(exc: *mut OurUnwindException) {
    trace!("delete_our_exception({:?})", exc);

    unsafe {
        if !exc.is_null() && (*exc).exception_class == OUR_BASE_EXCEPTION_CLASS {
            libc::free(
                (exc as *mut libc::c_char).offset(OurBaseException_t::base_from_unwind_offset()) as *mut libc::c_void,
            )
        }
    }
}

/// This function is the struct _Unwind_Exception API mandated delete function
/// used by foreign exception handlers when deleting our exception
/// (OurException), instances.
/// @param reason See @link http://mentorembedded.github.com/cxx-abi/abi-eh.html
/// @unlink
/// @param expToDelete exception instance to delete
extern "C" fn delete_from_unwind_our_exception(reason: UnwindReasonCode, exc: *mut OurUnwindException) {
    trace!("deleteFromUnwindOurException({:?})", exc);

    delete_our_exception(exc)
}

/// Creates (allocates on the heap), an exception (OurException instance),
/// of the supplied type info type.
/// @param type type info type
extern "C" fn create_our_exception(type_id: i32) -> *const OurUnwindException {
    unsafe {
        let size = mem::size_of::<OurException>();
        let p = libc::malloc(size);
        libc::memset(p, 0, size);
        let exc = p as *mut OurException;

        (*exc).exception_type.type_id = type_id;
        (*exc).unwind_exception.exception_class = OUR_BASE_EXCEPTION_CLASS;
        (*exc).unwind_exception.exception_cleanup = delete_from_unwind_our_exception;

        &(*exc).unwind_exception
    }
}

/// This is the personality function which is embedded (dwarf emitted), in the
/// dwarf unwind info block. Again see: JITDwarfEmitter.cpp.
/// See @link http://mentorembedded.github.com/cxx-abi/abi-eh.html @unlink
/// @param version unsupported (ignored), unwind version
/// @param _Unwind_Action actions minimally supported unwind stage
///        (forced specifically not supported)
/// @param exception_class exception class (_Unwind_Exception::exception_class)
///        of thrown exception.
/// @param exception_object thrown _Unwind_Exception instance.
/// @param context unwind system context
/// @returns minimally supported unwinding control indicator
extern "C" fn our_personality(
    version: i32,
    actions: _Unwind_Action,
    exception_class: UnwindExceptionClass,
    exception_object: *const _Unwind_Exception,
    context: *mut UnwindContext,
) -> UnwindReasonCode {
    trace!("We are in our_personality(...):actions is {:?}.", actions);

    if actions == _Unwind_Action::_UA_SEARCH_PHASE {
        trace!("our_personality(...):In search phase.")
    } else {
        trace!("our_personality(...):In non-search phase.")
    }

    let lsda = unsafe { _Unwind_GetLanguageSpecificData(context) };

    trace!("our_personality(...):lsda = <{:?}>", lsda);

    // The real work of the personality function is captured here
    return handle_lsda(
        version,
        lsda as *const libc::c_char,
        actions,
        exception_class,
        exception_object,
        context,
    );
}

/// Deals with the Language specific data portion of the emitted dwarf code.
/// See @link http://mentorembedded.github.com/cxx-abi/abi-eh.html @unlink
/// @param version unsupported (ignored), unwind version
/// @param lsda language specific data area
/// @param _Unwind_Action actions minimally supported unwind stage
///        (forced specifically not supported)
/// @param exception_class exception class (_Unwind_Exception::exception_class)
///        of thrown exception.
/// @param exception_object thrown _Unwind_Exception instance.
/// @param context unwind system context
/// @returns minimally supported unwinding control indicator
fn handle_lsda(
    version: i32,
    lsda: *const libc::c_char,
    actions: _Unwind_Action,
    exception_class: UnwindExceptionClass,
    exception_object: *const _Unwind_Exception,
    context: *mut UnwindContext,
) -> UnwindReasonCode {
    let reason = UnwindReasonCode::_URC_CONTINUE_UNWIND;

    if lsda.is_null() {
        trace!("handleLsda(...):lsda is non-zero.");
    }

    reason
}

#[derive(Debug)]
struct Example {
    context: Context,
    module: Module,
    fpm: FunctionPassManager,

    our_type_info_names: Vec<String>,

    our_type_info_type: Option<StructType>,
    our_caught_result_type: Option<StructType>,
    our_exception_type: Option<StructType>,
    our_unwind_exception_type: Option<StructType>,

    our_exception_not_thrown_state: Option<ConstantInt>,
    our_exception_thrown_state: Option<ConstantInt>,
    our_exception_caught_state: Option<ConstantInt>,
}

impl Example {
    pub fn new() -> Self {
        let context = Context::new();

        // Make the module, which holds all the code.
        let module = context.create_module("my cool jit");

        let fpm = FunctionPassManager::for_module(&module);

        if cfg!(add_opt_passes) {
            debug!("Optimizations turned on");

            // Basic AliasAnslysis support for GVN.
            fpm.add(jit::Pass::BasicAliasAnalysis);

            // Promote allocas to registers.
            fpm.add(jit::Pass::PromoteMemoryToRegister);

            // Do simple "peephole" optimizations and bit-twiddling optzns.
            fpm.add(jit::Pass::InstructionCombining);

            // Reassociate expressions.
            fpm.add(jit::Pass::Reassociate);

            // Eliminate Common SubExpressions.
            fpm.add(jit::Pass::GVN);

            // Simplify the control flow graph (deleting unreachable blocks, etc).
            fpm.add(jit::Pass::CFGSimplification);
        } else {
            debug!("Optimizations turned off");
        }

        fpm.init();

        Example {
            context: context,
            module: module,
            fpm: fpm,

            our_type_info_names: vec![],

            our_type_info_type: None,
            our_caught_result_type: None,
            our_exception_type: None,
            our_unwind_exception_type: None,

            our_exception_not_thrown_state: None,
            our_exception_thrown_state: None,
            our_exception_caught_state: None,
        }
    }

    fn dump(&self) {
        self.module.dump();
    }

    /// Creates test code by generating and organizing these functions into the
    /// test case. The test case consists of an outer function setup to invoke
    /// an inner function within an environment having multiple catch and single
    /// finally blocks. This inner function is also setup to invoke a throw
    /// function within an evironment similar in nature to the outer function's
    /// catch and finally blocks. Each of these two functions catch mutually
    /// exclusive subsets (even or odd) of the type info types configured
    /// for this this. All generated functions have a runtime argument which
    /// holds a type info type to throw that each function takes and passes it
    /// to the inner one if such a inner function exists. This type info type is
    /// looked at by the generated throw function to see whether or not it should
    /// throw a generated exception with the same type info type, or instead call
    /// a supplied a function which in turn will throw a foreign exception.
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param fpm a function pass manager holding optional IR to IR transformations
    /// @param native_throw_funct_name name of external function which will throw a foreign exception
    /// @returns outermost generated test function.
    fn create_unwind_exception_test(&mut self, native_throw_funct_name: &str) -> Function {
        let mut builder = self.context.create_builder();

        // Number of type infos to generate
        let num_type_infos = 6;

        // Initialze intrisics and external functions to use along with exception and type info globals.
        self.create_standard_utility_functions(&builder, num_type_infos, native_throw_funct_name);

        let native_throw_func_t = self.module.get_function(native_throw_funct_name).unwrap();

        // Create exception throw function using the value ~0 to cause foreign exceptions to be thrown.
        let throw_func_t = self.create_throw_exception_function(&builder, "throw_func_t", !0, native_throw_func_t);

        // Inner function will catch even type infos
        let inner_exception_types_to_catch = &[6, 2, 4];

        // Generate inner function.
        let inner_catch_func_t = self.create_catch_wrapped_invoke_function(
            &mut builder,
            throw_func_t,
            "inner_catch_func_t",
            inner_exception_types_to_catch,
        );

        throw_func_t
    }

    /// Generates a function which invokes a function (to_invoke) and, whose
    /// unwind block will "catch" the type info types correspondingly held in the
    /// exception_types_to_catch argument. If the to_invoke function throws an
    /// exception which does not match any type info types contained in
    /// exception_types_to_catch, the generated code will call _Unwind_Resume
    /// with the raised exception. On the other hand the generated code will
    /// normally exit if the to_invoke function does not throw an exception.
    /// The generated "finally" block is always run regardless of the cause of
    /// the generated function exit.
    /// The generated function is returned after being verified.
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param fpm a function pass manager holding optional IR to IR
    ///        transformations
    /// @param to_invoke inner function to invoke
    /// @param our_id id used to printing purposes
    /// @param numExceptionsToCatch length of exception_types_to_catch array
    /// @param exception_types_to_catch array of type info types to "catch"
    /// @returns generated function
    fn create_catch_wrapped_invoke_function(
        &mut self,
        builder: &mut IRBuilder,
        to_invoke: Function,
        our_id: &str,
        exception_types_to_catch: &[i32],
    ) -> Function {
        let void_t = self.context.void_t();
        let i32_t = self.context.int32_t();
        let i64_t = self.context.int64_t();

        let to_print32_int = self.module.get_function("print32_int").unwrap();

        let func = self.module.get_or_insert_function(our_id, void_t, &[i32_t]);

        let except_type = func.get_param(0).unwrap();

        except_type.set_name("except_typeToThrow");

        // Block which calls invoke
        let entry_block = func.append_basic_block_in_context("entry", &self.context);

        // Normal block for invoke
        let normal_block = func.append_basic_block_in_context("normal", &self.context);

        // Unwind block for invoke
        let exception_block = func.append_basic_block_in_context("exception", &self.context);

        // Block which routes exception to correct catch handler block
        let exception_route_block = func.append_basic_block_in_context("exceptionRoute", &self.context);

        // Foreign exception handler
        let external_exception_block = func.append_basic_block_in_context("externalException", &self.context);

        // Block which calls _Unwind_Resume
        let unwind_resume_block = func.append_basic_block_in_context("unwindResume", &self.context);

        // Clean up block which delete exception if needed
        let end_block = func.append_basic_block_in_context("end", &self.context);

        // Finally block which will branch to unwind_resume_block if
        // exception is not caught. Initializes/allocates stack locations.
        let (finally_block, exception_caught, exception_storage, caught_result_storage) =
            self.create_finally_block(&builder, func, "finally", our_id, end_block, unwind_resume_block);

        let catch_blocks: Vec<BasicBlock> = exception_types_to_catch
            .iter()
            .map(|&i| {
                let next_name = &self.our_type_info_names[i as usize];

                // One catch block per type info to be caught
                self.create_catch_block(&builder, func, next_name, our_id, finally_block, exception_caught)
            }).collect();

        // Entry Block
        builder.position_at_end(entry_block);

        invoke!(to_invoke(except_type) to label normal_block unwind label exception_block).emit_to(&builder);

        // End Block
        builder.position_at_end(end_block);

        self.generate_string_print(
            &builder,
            format!("Gen: In end block: exiting in {}.\n", our_id),
            USE_GLOBAL_STR_CONSTS,
        );

        let delete_our_exception = self.module.get_function("delete_our_exception").unwrap();

        // Note: function handles NULL exceptions
        let load = load!(exception_storage).emit_to(&builder);
        call!(delete_our_exception, load).emit_to(&builder);
        ret!().emit_to(&builder);

        // Normal Block
        builder.position_at_end(normal_block);

        self.generate_string_print(
            &builder,
            format!("Gen: No exception in {}!\n", our_id),
            USE_GLOBAL_STR_CONSTS,
        );

        // Finally block is always called
        br!(finally_block).emit_to(&builder);

        // Unwind Resume Block
        builder.position_at_end(unwind_resume_block);

        let load = load!(caught_result_storage).emit_to(&builder);

        resume!(load).emit_to(&builder);

        // Exception Block
        builder.position_at_end(exception_block);

        let personality = self.module.get_function("our_personality").unwrap();

        func.set_personality_function(personality);

        let caught_result = landing_pad!(self.our_caught_result_type.unwrap(); "landingPad").emit_to(&builder);
        caught_result.set_cleanup(true);

        for i in exception_types_to_catch {
            caught_result.add_clause(
                self.module
                    .get_global_var(self.our_type_info_names[*i as usize].as_str())
                    .unwrap(),
            );
        }

        let unwind_exception = extract_value!(caught_result, 0).emit_to(&builder);
        let ret_type_info_index = extract_value!(caught_result, 1).emit_to(&builder);

        // FIXME: Redundant storage which, beyond utilizing value of
        //        caught_resultStore for unwind_exception storage, may be alleviated
        //        altogether with a block rearrangement
        store!(caught_result, caught_result_storage).emit_to(&builder);
        store!(unwind_exception, exception_storage).emit_to(&builder);
        store!(self.our_exception_thrown_state.unwrap(), exception_caught).emit_to(&builder);

        // Retrieve exception_class member from thrown exception
        // (_Unwind_Exception instance). This member tells us whether or not
        // the exception is foreign.
        let p = ptr_cast!(unwind_exception, self.our_unwind_exception_type.unwrap().ptr_t()).emit_to(&builder);
        let p = struct_gep!(p, 0).emit_to(&builder);
        let unwind_exception_class = load!(p).emit_to(&builder);

        // Branch to the external_exception_block if the exception is foreign or
        // to a catch router if not. Either way the finally block will be run.
        let eq = icmp!(EQ unwind_exception_class, i64_t.int(OUR_BASE_EXCEPTION_CLASS as i64)).emit_to(&builder);
        br!(
                eq => exception_route_block,
                _ => external_exception_block
            ).emit_to(&builder);

        // External Exception Block
        builder.position_at_end(external_exception_block);

        self.generate_string_print(&builder, "Gen: Foreign exception received.\n", USE_GLOBAL_STR_CONSTS);

        br!(finally_block).emit_to(&builder);

        // Exception Route Block
        builder.position_at_end(exception_route_block);

        // Casts exception pointer (_Unwind_Exception instance) to parent
        // (OurException instance).
        //
        // Note: ourBaseFromUnwindOffset is usually negative
        let p = gep!(
            unwind_exception,
            i64_t.int(OurBaseException_t::base_from_unwind_offset() as i64)
        ).emit_to(&builder);
        let type_info_thrown =
            ptr_cast!(p, self.our_exception_type.unwrap().ptr_t(); "type_info_thrown").emit_to(&builder);

        // Retrieve thrown exception type info type
        //
        // Note: Index is not relative to pointer but instead to structure
        //       unlike a true getelementptr (GEP) instruction
        let type_info_thrown = struct_gep!(type_info_thrown, 0; "type_info_thrown").emit_to(&builder);
        let type_info_thrown_type = struct_gep!(type_info_thrown, 0; "type_info_thrown_type").emit_to(&builder);
        let load = load!(type_info_thrown_type).emit_to(&builder);

        self.generate_integer_print(
            &builder,
            to_print32_int,
            load,
            format!("Gen: Exception type <%d> received (stack unwound) in {}.\n", our_id),
            USE_GLOBAL_STR_CONSTS,
        );

        // Route to matched type info catch block or run cleanup finally block
        let switch = switch!(ret_type_info_index;
            _ => finally_block
        ).emit_to(&builder);

        for i in 0..exception_types_to_catch.len() {
            switch.add_case(i32_t.int(i as i64 + 1), catch_blocks[i]);
        }

        //func.verify().unwrap();

        func
    }

    /// Generates catch block semantics which print a string to indicate type of
    /// catch executed, sets an exception caught flag, and executes passed in
    /// end block (terminator block).
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param to_add_to parent function to add block to
    /// @param block_name block name of new "catch" block.
    /// @param function_id output id used for printing
    /// @param terminator_block terminator "end" block
    /// @param exception_caughtFlag exception caught/thrown status
    /// @returns newly created block
    fn create_catch_block(
        &self,
        builder: &IRBuilder,
        to_add_to: Function,
        block_name: &str,
        function_id: &str,
        terminator_block: BasicBlock,
        exception_caught: AllocaInst,
    ) -> BasicBlock {
        let bb = to_add_to.append_basic_block_in_context(block_name, &self.context);

        builder.position_at_end(bb);

        self.generate_string_print(
            &builder,
            format!("Gen: Executing catch block {} in {}", block_name, function_id),
            USE_GLOBAL_STR_CONSTS,
        );

        store!(self.our_exception_caught_state.unwrap(), exception_caught).emit_to(&builder);
        br!(terminator_block).emit_to(&builder);

        bb
    }

    /// Generates code to handle finally block type semantics: always runs
    /// regardless of whether a thrown exception is passing through or the
    /// parent function is simply exiting. In addition to printing some state
    /// to stderr, this code will resume the exception handling--runs the
    /// unwind resume block, if the exception has not been previously caught
    /// by a catch clause, and will otherwise execute the end block (terminator
    /// block). In addition this function creates the corresponding function's
    /// stack storage for the exception pointer and catch flag status.
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param to_add_to parent function to add block to
    /// @param block_name block name of new "finally" block.
    /// @param function_id output id used for printing
    /// @param terminator_block terminator "end" block
    /// @param unwind_resume_block unwind resume block
    /// @param exception_caughtFlag reference exception caught/thrown status storage
    /// @param exception_storage reference to exception pointer storage
    /// @param caught_result_storage reference to landingpad result storage
    /// @returns newly created block
    fn create_finally_block(
        &self,
        builder: &IRBuilder,
        to_add_to: Function,
        block_name: &str,
        function_id: &str,
        terminator_block: BasicBlock,
        unwind_resume_block: BasicBlock,
    ) -> (BasicBlock, AllocaInst, AllocaInst, AllocaInst) {
        let exception_caught = self.create_entry_block_alloca(
            to_add_to,
            "exception_caught",
            self.our_exception_not_thrown_state.unwrap().type_of(),
            Some(self.our_exception_not_thrown_state.unwrap().into()),
        );
        let exception_storage_type = self.context.int8_t().ptr_t();
        let exception_storage = self.create_entry_block_alloca(
            to_add_to,
            "exception_storage",
            exception_storage_type.into(),
            Some(exception_storage_type.null()),
        );
        let caught_result_storage = self.create_entry_block_alloca(
            to_add_to,
            "caught_result_storage",
            self.our_caught_result_type.unwrap().into(),
            Some(self.our_caught_result_type.unwrap().null()),
        );

        let bb = to_add_to.append_basic_block_in_context(block_name, &self.context);

        builder.position_at_end(bb);

        self.generate_string_print(
            &builder,
            format!("Gen: Executing finally block {} in {}", block_name, function_id),
            USE_GLOBAL_STR_CONSTS,
        );

        switch!(load!(exception_caught).emit_to(&builder);
            _ => terminator_block,
            self.our_exception_caught_state.unwrap() => terminator_block,
            self.our_exception_thrown_state.unwrap() => unwind_resume_block
        ).emit_to(&builder);

        (bb, exception_caught, exception_storage, caught_result_storage)
    }

    fn create_entry_block_alloca(
        &self,
        func: Function,
        var_name: &str,
        ty: TypeRef,
        init: Option<jit::Constant>,
    ) -> AllocaInst {
        let builder = self.context.create_builder();

        builder.position_at_end(func.entry().unwrap());

        let p = alloca!(ty; var_name).emit_to(&builder);

        if let Some(v) = init {
            store!(v, p).emit_to(&builder);
        }

        p
    }

    /// Generates function which throws either an exception matched to a runtime
    /// determined type info type (argument to generated function), or if this
    /// runtime value matches native_throw_type, throws a foreign exception by
    /// calling native_throw_func_t.
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param fpm a function pass manager holding optional IR to IR
    ///        transformations
    /// @param our_id id used to printing purposes
    /// @param native_throw_type a runtime argument of this value results in
    ///        native_throw_func_t being called to generate/throw exception.
    /// @param native_throw_func_t function which will throw a foreign exception
    ///        if the above native_throw_type matches generated function's arg.
    /// @returns generated function
    fn create_throw_exception_function(
        &mut self,
        builder: &IRBuilder,
        our_id: &str,
        native_throw_type: i32,
        native_throw_func_t: Function,
    ) -> Function {
        let void_t = self.context.void_t();
        let i32_t = self.context.int32_t();

        let func = self.module.get_or_insert_function(our_id, void_t, &[i32_t]);

        let exception_type = func.get_param(0).unwrap();

        exception_type.set_name("except_typeToThrow");

        // Throws either one of our exception or a native C++ exception depending
        // on a runtime argument value containing a type info type.
        let entry_block = func.append_basic_block_in_context("entry", &self.context);

        // Throws a foreign exception
        let native_throw_block = func.append_basic_block_in_context("nativeThrow", &self.context);

        // Throws one of our Exceptions
        let generated_throw_block = func.append_basic_block_in_context("generatedThrow", &self.context);

        // native_throw_block block
        builder.position_at_end(native_throw_block);
        {
            // Throws foreign exception
            call!(native_throw_func_t, exception_type).emit_to(&builder);
            unreachable().emit_to(&builder);
        }

        // entry block
        builder.position_at_end(entry_block);
        {
            let to_print32_int = self.module.get_function("print32_int").unwrap();

            self.generate_integer_print(
                &builder,
                to_print32_int,
                exception_type,
                format!("\nGen: About to throw exception type <%d> in {} .\n", our_id),
                USE_GLOBAL_STR_CONSTS,
            );

            // Switches on runtime type info type value to determine whether or not
            // a foreign exception is thrown. Defaults to throwing one of our generated exceptions.
            let switch = switch!(exception_type;
            _ => generated_throw_block,
            i32_t.int(native_throw_type as i64) => native_throw_block
        ).emit_to(&builder);
        }

        // generatedThrow block
        builder.position_at_end(generated_throw_block);
        {
            let create_our_exception = self.module.get_function("create_our_exception").unwrap();
            let raise_our_exception = self.module.get_function("_Unwind_RaiseException").unwrap();

            // Creates exception to throw with runtime type info type.
            let exception = call!(create_our_exception, exception_type).emit_to(&builder);

            // Throw generated Exception
            call!(raise_our_exception, exception).emit_to(&builder);
            unreachable().emit_to(&builder);
        }

        //func.verify().unwrap();

        func
    }

    /// Generates code to print given runtime integer according to constant
    /// string format, and a given print function.
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param print_func_t function used to "print" integer
    /// @param to_print string to print
    /// @param format printf like formating string for print
    /// @param useGlobal A value of true (default) indicates a GlobalValue is
    ///        generated, and is used to hold the constant string. A value of
    ///        false indicates that the constant string will be stored on the
    ///        stack.
    fn generate_integer_print<S: AsRef<str>, V: Into<ValueRef>>(
        &self,
        builder: &IRBuilder,
        print_func_t: Function,
        to_print: V,
        format: S,
        use_global: bool,
    ) {
        let i8_t = self.context.int8_t();

        let string_var = if use_global {
            global_str_ptr!(format.as_ref()).emit_to(&builder)
        } else {
            let string_var = alloca!(i8_t.array_t(format.as_ref().len())).emit_to(&builder);
            store(self.context.str(format.as_ref()), string_var).emit_to(&builder);
            string_var.into()
        };

        let cast = bit_cast!(string_var, i8_t.ptr_t()).emit_to(&builder);

        call!(print_func_t, to_print.into(), string_var).emit_to(&builder);
    }

    /// Generates code to print given constant string
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param to_print string to print
    /// @param useGlobal A value of true (default) indicates a GlobalValue is
    ///        generated, and is used to hold the constant string. A value of
    ///        false indicates that the constant string will be stored on the
    ///        stack.
    fn generate_string_print<S: AsRef<str>>(&self, builder: &IRBuilder, to_print: S, use_global: bool) {
        let i8_t = self.context.int8_t();
        let print_func_t = self.module.get_function("print_str").unwrap();

        let string_var = if use_global {
            global_str!(to_print.as_ref()).emit_to(&builder)
        } else {
            let string_var = alloca!(i8_t.array_t(to_print.as_ref().len())).emit_to(&builder);
            store(self.context.str(to_print.as_ref()), string_var).emit_to(&builder);
            string_var.into()
        };

        let cast = ptr_cast!(string_var, i8_t.ptr_t()).emit_to(&builder);

        call!(print_func_t, cast).emit_to(&builder);
    }

    /// This initialization routine creates type info globals and
    /// adds external function declarations to module.
    /// @param num_type_infos number of linear type info associated type info types
    ///        to create as GlobalVariable instances, starting with the value 1.
    /// @param module code for module instance
    /// @param builder builder instance
    fn create_standard_utility_functions(
        &mut self,
        builder: &IRBuilder,
        num_type_infos: usize,
        native_throw_funct_name: &str,
    ) {
        let void_t = self.context.void_t();
        let i8_t = self.context.int8_t();
        let i32_t = self.context.int32_t();
        let i64_t = self.context.int64_t();

        // Exception initializations

        // Setup exception catch state
        self.our_exception_not_thrown_state = Some(i8_t.int(0));
        self.our_exception_thrown_state = Some(i8_t.int(1));
        self.our_exception_caught_state = Some(i8_t.int(2));

        // Create our type info type
        self.our_type_info_type = Some(self.context.named_struct_t("TypeInfo_t", &[i32_t], false));

        // Create our landingpad result type
        self.our_caught_result_type = Some(self.context.named_struct_t(
            "CaughtResult_t",
            types![i8_t.ptr_t(), i32_t],
            false,
        ));

        // Create OurException type
        self.our_exception_type = Some(self.context.named_struct_t(
            "OurBaseException_t",
            types![self.our_type_info_type.unwrap()],
            false,
        ));

        // Create portion of _Unwind_Exception type
        //
        // Note: Declaring only a portion of the _Unwind_Exception struct.
        //       Does this cause problems?
        self.our_unwind_exception_type = Some(self.context.named_struct_t("_Unwind_Exception", &[i64_t], false));

        // Generate each type info
        //
        // Note: First type info is not used.
        self.our_type_info_names = (0..num_type_infos + 1)
            .map(|i| {
                let type_info_name = format!("typeInfo{}", i);

                self.module
                    .add_global_var(&type_info_name, self.our_type_info_type.unwrap())
                    .set_initializer(self.our_type_info_type.unwrap().struct_of(values![i32_t.int(i as i64)]));

                type_info_name
            }).collect();

        // print32_int
        self.module
            .get_or_insert_function("print32_int", void_t, types![i32_t, i8_t.ptr_t()]);

        // print64_int
        self.module
            .get_or_insert_function("print64_int", void_t, types![i64_t, i8_t.ptr_t()]);

        // print_str
        self.module
            .get_or_insert_function("print_str", void_t, types![i8_t.ptr_t()]);

        // native_throw_funct_name
        self.module
            .get_or_insert_function(native_throw_funct_name, void_t, &[i32_t]);

        // delete_our_exception
        self.module
            .get_or_insert_function("delete_our_exception", void_t, types![i8_t.ptr_t()]);

        // create_our_exception
        self.module
            .get_or_insert_function("create_our_exception", i8_t.ptr_t(), types![i32_t]);

        let no_return = self.context.create_enum_attribute(*jit::attrs::NoReturn, 0);

        // _Unwind_RaiseException
        self.module
            .get_or_insert_function("_Unwind_RaiseException", i32_t, types![i8_t.ptr_t()])
            .add_attribute(no_return);

        // _Unwind_Resume
        self.module
            .get_or_insert_function("_Unwind_Resume", i32_t, types![i8_t.ptr_t()])
            .add_attribute(no_return);

        // our_personality
        self.module.get_or_insert_function(
            "our_personality",
            i32_t,
            types![i32_t, i32_t, i64_t, i8_t.ptr_t(), i8_t.ptr_t()],
        );
    }

    fn create_execution_engine(self) -> ExecutionEngine {
        let print32_int_func = self.module.get_function("print32_int").unwrap();
        let print64_int_func = self.module.get_function("print64_int").unwrap();
        let print_str_func = self.module.get_function("print_str").unwrap();
        let throw_cpp_exception_func = self.module.get_function("throw_cpp_exception").unwrap();
        let delete_our_exception_func = self.module.get_function("delete_our_exception").unwrap();
        let create_our_exception_func = self.module.get_function("create_our_exception").unwrap();
        let our_personality_func = self.module.get_function("our_personality").unwrap();

        let ee = ExecutionEngine::for_module(self.module).unwrap();

        ee.add_global_mapping(&print32_int_func, &mut print_int32);
        ee.add_global_mapping(&print64_int_func, &mut print_int64);
        ee.add_global_mapping(&print_str_func, &mut print_str);
        ee.add_global_mapping(&throw_cpp_exception_func, &mut panic_in_rust);
        ee.add_global_mapping(&delete_our_exception_func, &mut delete_our_exception);
        ee.add_global_mapping(&create_our_exception_func, &mut create_our_exception);
        ee.add_global_mapping(&our_personality_func, &mut our_personality);

        ee.run_static_destructors();

        ee
    }
}

extern "C" fn panic_in_rust(_: i32) {
    panic!("thrown by panic_in_rust(...)")
}

/// This is a test harness which runs test by executing generated
/// function with a type info type to throw. Harness wraps the execution
/// of generated function in a C++ try catch clause.
/// @param engine execution engine to use for executing generated function.
///        This demo program expects this to be a JIT instance for demo
///        purposes.
/// @param function generated test function to run
/// @param typeToThrow type info type of generated exception to throw, or
///        indicator to cause foreign exception to be thrown.
fn run_exception_throw(ee: &ExecutionEngine, func: &Function, type_to_throw: i32) {
    // Find test's function pointer
    let addr: *const u8 = ee.get_ptr_to_global(func);
    let func: extern "C" fn(type_to_throw: i32) = unsafe { mem::transmute(addr) };

    let result = panic::catch_unwind(|| func(type_to_throw));

    println!(
        "\nrunExceptionThrow(...):In C++ catch OurCppRunException with reason: {:?}",
        result
    );
}

fn main() {
    pretty_env_logger::init();

    jit::target::NativeTarget::init().unwrap();
    jit::target::NativeAsmPrinter::init().unwrap();

    let mut example = Example::new();

    let func = example.create_unwind_exception_test("throw_cpp_exception");

    println!("\nBegin module dump:\n\n");

    example.dump();

    println!("\nEnd module dump:\n");

    println!("\n\nBegin Test:\n");

    let ee = example.create_execution_engine();

    for arg in env::args().skip(1) {
        // Run test for each argument whose value is the exception type to throw.
        run_exception_throw(&ee, &func, arg.parse().unwrap())
    }

    println!("\nEnd Test:\n\n");
}
