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

#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_sys as llvm;
extern crate libc;

use std::env;
use std::mem;

use jit::{Constant, ConstantInt, FunctionPassManager, StructType};
use jit::insts::*;
use jit::prelude::*;

const USE_GLOBAL_STR_CONSTS: bool = true;
const ourBaseExceptionClass: u64 = 0;

/// This is our simplistic type info
#[repr(C)]
pub struct OurExceptionType_t {
    /// type info type
    type_info: i32,
}

/// This is our Exception class which relies on a negative offset to calculate
/// pointers to its instances from pointers to its unwindException member.
///
/// Note: The above unwind.h defines struct _Unwind_Exception to be aligned
///       on a double word boundary. This is necessary to match the standard:
///       http://mentorembedded.github.com/cxx-abi/abi-eh.html
#[repr(C)]
pub struct OurBaseException_t {
    exceptionType: OurExceptionType_t,

    // Note: This is properly aligned in unwind.h
    unwindException: _Unwind_Exception,
}

pub type OurException = OurBaseException_t;
pub type OurUnwindException = _Unwind_Exception;

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum _Unwind_Reason_Code {
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

pub type _Unwind_Exception_Class = u64;
pub type _Unwind_Word = libc::uintptr_t;
pub type _Unwind_Ptr = libc::uintptr_t;
pub type _Unwind_Trace_Fn = extern "C" fn(ctx: *mut _Unwind_Context, arg: *mut libc::c_void)
                                          -> _Unwind_Reason_Code;
#[cfg(target_arch = "x86")]
pub const unwinder_private_data_size: usize = 5;

#[cfg(target_arch = "x86_64")]
pub const unwinder_private_data_size: usize = 6;

#[cfg(all(target_arch = "arm", not(target_os = "ios")))]
pub const unwinder_private_data_size: usize = 20;

#[cfg(all(target_arch = "arm", target_os = "ios"))]
pub const unwinder_private_data_size: usize = 5;

#[cfg(target_arch = "aarch64")]
pub const unwinder_private_data_size: usize = 2;

#[cfg(target_arch = "mips")]
pub const unwinder_private_data_size: usize = 2;

#[cfg(target_arch = "mips64")]
pub const unwinder_private_data_size: usize = 2;

#[cfg(any(target_arch = "powerpc", target_arch = "powerpc64"))]
pub const unwinder_private_data_size: usize = 2;

#[cfg(target_arch = "s390x")]
pub const unwinder_private_data_size: usize = 2;

#[cfg(target_arch = "sparc64")]
pub const unwinder_private_data_size: usize = 2;

#[cfg(target_os = "emscripten")]
pub const unwinder_private_data_size: usize = 20;

#[repr(C)]
pub struct _Unwind_Exception {
    pub exception_class: _Unwind_Exception_Class,
    pub exception_cleanup: _Unwind_Exception_Cleanup_Fn,
    pub private: [_Unwind_Word; unwinder_private_data_size],
}

pub enum _Unwind_Context {}

pub type _Unwind_Exception_Cleanup_Fn = extern "C" fn(unwind_code: _Unwind_Reason_Code,
                                                      exception: *mut _Unwind_Exception);

#[derive(Debug)]
struct Example {
    context: Context,
    module: Module,
    fpm: FunctionPassManager,

    ourTypeInfoNames: Vec<String>,

    ourBaseFromUnwindOffset: usize,

    ourTypeInfoType: Option<StructType>,
    ourCaughtResultType: Option<StructType>,
    ourExceptionType: Option<StructType>,
    ourUnwindExceptionType: Option<StructType>,

    ourExceptionNotThrownState: Option<ConstantInt>,
    ourExceptionThrownState: Option<ConstantInt>,
    ourExceptionCaughtState: Option<ConstantInt>,
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

            ourTypeInfoNames: vec![],

            ourBaseFromUnwindOffset: 0,

            ourTypeInfoType: None,
            ourCaughtResultType: None,
            ourExceptionType: None,
            ourUnwindExceptionType: None,

            ourExceptionNotThrownState: None,
            ourExceptionThrownState: None,
            ourExceptionCaughtState: None,
        }
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
    /// @param nativeThrowFunctName name of external function which will throw a foreign exception
    /// @returns outermost generated test function.
    fn create_unwind_exception_test(&mut self, nativeThrowFunctName: &str) -> Function {
        let mut builder = self.context.create_builder();

        // Number of type infos to generate
        let numTypeInfos = 6;

        // Initialze intrisics and external functions to use along with exception and type info globals.
        self.create_standard_utility_functions(&builder, numTypeInfos, nativeThrowFunctName);

        let nativeThrowFunct = self.module.get_function(nativeThrowFunctName).unwrap();

        // Create exception throw function using the value ~0 to cause foreign exceptions to be thrown.
        let throwFunct =
            self.create_throw_exception_function(&builder, "throwFunct", !0, nativeThrowFunct);

        // Inner function will catch even type infos
        let innerExceptionTypesToCatch = &[6, 2, 4];

        // Generate inner function.
        let innerCatchFunct = self.create_catch_wrapped_invoke_function(
            &mut builder,
            throwFunct,
            "innerCatchFunct",
            innerExceptionTypesToCatch,
        );

        throwFunct
    }

    /// Generates a function which invokes a function (toInvoke) and, whose
    /// unwind block will "catch" the type info types correspondingly held in the
    /// exceptionTypesToCatch argument. If the toInvoke function throws an
    /// exception which does not match any type info types contained in
    /// exceptionTypesToCatch, the generated code will call _Unwind_Resume
    /// with the raised exception. On the other hand the generated code will
    /// normally exit if the toInvoke function does not throw an exception.
    /// The generated "finally" block is always run regardless of the cause of
    /// the generated function exit.
    /// The generated function is returned after being verified.
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param fpm a function pass manager holding optional IR to IR
    ///        transformations
    /// @param toInvoke inner function to invoke
    /// @param ourId id used to printing purposes
    /// @param numExceptionsToCatch length of exceptionTypesToCatch array
    /// @param exceptionTypesToCatch array of type info types to "catch"
    /// @returns generated function
    fn create_catch_wrapped_invoke_function(
        &mut self,
        builder: &mut IRBuilder,
        toInvoke: Function,
        ourId: &str,
        exceptionTypesToCatch: &[i32],
    ) -> Function {
        let void_t = self.context.void_t();
        let i8_t = self.context.int8_t();
        let i32_t = self.context.int32_t();
        let i64_t = self.context.int64_t();

        let toPrint32Int = self.module.get_function("print32Int").unwrap();

        let func = self.module.get_or_insert_function(ourId, void_t, &[i32_t]);

        let exceptType = func.get_param(0).unwrap();

        exceptType.set_name("exceptTypeToThrow");

        // Block which calls invoke
        let entryBlock = func.append_basic_block_in_context("entry", &self.context);

        // Normal block for invoke
        let normalBlock = func.append_basic_block_in_context("normal", &self.context);

        // Unwind block for invoke
        let exceptionBlock = func.append_basic_block_in_context("exception", &self.context);

        // Block which routes exception to correct catch handler block
        let exceptionRouteBlock =
            func.append_basic_block_in_context("exceptionRoute", &self.context);

        // Foreign exception handler
        let externalExceptionBlock =
            func.append_basic_block_in_context("externalException", &self.context);

        // Block which calls _Unwind_Resume
        let unwindResumeBlock = func.append_basic_block_in_context("unwindResume", &self.context);

        // Clean up block which delete exception if needed
        let endBlock = func.append_basic_block_in_context("end", &self.context);

        // Finally block which will branch to unwindResumeBlock if
        // exception is not caught. Initializes/allocates stack locations.
        let (finallyBlock, exceptionCaught, exceptionStorage, caughtResultStorage) =
            self.create_finally_block(
                &builder,
                func,
                "finally",
                ourId,
                endBlock,
                unwindResumeBlock,
            );

        let catchBlocks: Vec<BasicBlock> = exceptionTypesToCatch
            .iter()
            .map(|&i| {
                let nextName = &self.ourTypeInfoNames[i as usize];

                // One catch block per type info to be caught
                self.create_catch_block(
                    &builder,
                    func,
                    nextName,
                    ourId,
                    finallyBlock,
                    exceptionCaught,
                )
            })
            .collect();

        // Entry Block
        builder.position_at_end(entryBlock);

        invoke!(toInvoke, exceptType; to normalBlock; unwind exceptionBlock).emit_to(&builder);

        // End Block
        builder.position_at_end(endBlock);

        self.generate_string_print(
            &builder,
            format!("Gen: In end block: exiting in {}.\n", ourId),
            USE_GLOBAL_STR_CONSTS,
        );

        let deleteOurException = self.module.get_function("deleteOurException").unwrap();

        // Note: function handles NULL exceptions
        let load = load!(exceptionStorage).emit_to(&builder);
        call!(deleteOurException, load).emit_to(&builder);
        ret!().emit_to(&builder);

        // Normal Block
        builder.position_at_end(normalBlock);

        self.generate_string_print(
            &builder,
            format!("Gen: No exception in {}!\n", ourId),
            USE_GLOBAL_STR_CONSTS,
        );

        // Finally block is always called
        br!(finallyBlock).emit_to(&builder);

        // Unwind Resume Block
        builder.position_at_end(unwindResumeBlock);

        let load = load!(caughtResultStorage).emit_to(&builder);

        resume!(load).emit_to(&builder);

        // Exception Block
        builder.position_at_end(exceptionBlock);

        let personality = self.module.get_function("ourPersonality").unwrap();

        func.set_personality_function(personality);

        let caughtResult = landing_pad!(self.ourCaughtResultType.unwrap(); "landingPad")
            .emit_to(&builder);
        caughtResult.set_cleanup(true);

        for i in exceptionTypesToCatch {
            caughtResult.add_clause(
                self.module
                    .get_global_var(self.ourTypeInfoNames[*i as usize].as_str())
                    .unwrap(),
            );
        }

        let unwindException = extract_value!(caughtResult, 0).emit_to(&builder);
        let retTypeInfoIndex = extract_value!(caughtResult, 1).emit_to(&builder);

        // FIXME: Redundant storage which, beyond utilizing value of
        //        caughtResultStore for unwindException storage, may be alleviated
        //        altogether with a block rearrangement
        store!(caughtResult, caughtResultStorage).emit_to(&builder);
        store!(unwindException, exceptionStorage).emit_to(&builder);
        store!(self.ourExceptionThrownState.unwrap(), exceptionCaught).emit_to(&builder);

        // Retrieve exception_class member from thrown exception
        // (_Unwind_Exception instance). This member tells us whether or not
        // the exception is foreign.
        let p = ptr_cast!(
            unwindException,
            self.ourUnwindExceptionType.unwrap().ptr_t()
        ).emit_to(&builder);
        let p = gep!(structure p, 0).emit_to(&builder);
        let unwindExceptionClass = load!(p).emit_to(&builder);

        // Branch to the externalExceptionBlock if the exception is foreign or
        // to a catch router if not. Either way the finally block will be run.
        let eq = icmp!(eq unwindExceptionClass, i64_t.int(ourBaseExceptionClass as i64))
            .emit_to(&builder);
        br!(
                eq => exceptionRouteBlock,
                _ => externalExceptionBlock
            ).emit_to(&builder);

        // External Exception Block
        builder.position_at_end(externalExceptionBlock);

        self.generate_string_print(
            &builder,
            "Gen: Foreign exception received.\n",
            USE_GLOBAL_STR_CONSTS,
        );

        br!(finallyBlock).emit_to(&builder);

        // Exception Route Block
        builder.position_at_end(exceptionRouteBlock);

        // Casts exception pointer (_Unwind_Exception instance) to parent
        // (OurException instance).
        //
        // Note: ourBaseFromUnwindOffset is usually negative
        let p = gep!(
            unwindException,
            i64_t.int(self.ourBaseFromUnwindOffset as i64)
        ).emit_to(&builder);
        let typeInfoThrown = ptr_cast!(p, self.ourExceptionType.unwrap().ptr_t()).emit_to(&builder);

        // Retrieve thrown exception type info type
        //
        // Note: Index is not relative to pointer but instead to structure
        //       unlike a true getelementptr (GEP) instruction
        let typeInfoThrown = gep!(structure typeInfoThrown, 0).emit_to(&builder);
        let p = ptr_cast!(typeInfoThrown, i8_t.ptr_t()).emit_to(&builder);
        let typeInfoThrownType = gep!(structure p, 0).emit_to(&builder);
        let load = load!(typeInfoThrownType).emit_to(&builder);

        self.generate_integer_print(
            &builder,
            toPrint32Int,
            load,
            format!(
                "Gen: Exception type <%d> received (stack unwound) in {}.\n",
                ourId
            ),
            USE_GLOBAL_STR_CONSTS,
        );

        // Route to matched type info catch block or run cleanup finally block
        let switch = switch!(retTypeInfoIndex;
            _ => finallyBlock
        ).emit_to(&builder);

        for i in 0..exceptionTypesToCatch.len() {
            switch.add_case(i32_t.int(i as i64 + 1), catchBlocks[i]);
        }

        //func.verify().unwrap();

        //self.fpm.run(func);

        func
    }

    /// Generates catch block semantics which print a string to indicate type of
    /// catch executed, sets an exception caught flag, and executes passed in
    /// end block (terminator block).
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param toAddTo parent function to add block to
    /// @param blockName block name of new "catch" block.
    /// @param functionId output id used for printing
    /// @param terminatorBlock terminator "end" block
    /// @param exceptionCaughtFlag exception caught/thrown status
    /// @returns newly created block
    fn create_catch_block(
        &self,
        builder: &IRBuilder,
        toAddTo: Function,
        blockName: &str,
        functionId: &str,
        terminatorBlock: BasicBlock,
        exceptionCaught: AllocaInst,
    ) -> BasicBlock {
        let bb = toAddTo.append_basic_block_in_context(blockName, &self.context);

        builder.position_at_end(bb);

        self.generate_string_print(
            &builder,
            format!("Gen: Executing catch block {} in {}", blockName, functionId),
            USE_GLOBAL_STR_CONSTS,
        );

        store!(self.ourExceptionCaughtState.unwrap(), exceptionCaught).emit_to(&builder);
        br!(terminatorBlock).emit_to(&builder);

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
    /// @param toAddTo parent function to add block to
    /// @param blockName block name of new "finally" block.
    /// @param functionId output id used for printing
    /// @param terminatorBlock terminator "end" block
    /// @param unwindResumeBlock unwind resume block
    /// @param exceptionCaughtFlag reference exception caught/thrown status storage
    /// @param exceptionStorage reference to exception pointer storage
    /// @param caughtResultStorage reference to landingpad result storage
    /// @returns newly created block
    fn create_finally_block(
        &self,
        builder: &IRBuilder,
        toAddTo: Function,
        blockName: &str,
        functionId: &str,
        terminatorBlock: BasicBlock,
        unwindResumeBlock: BasicBlock,
    ) -> (BasicBlock, AllocaInst, AllocaInst, AllocaInst) {
        let exceptionCaught = self.create_entry_block_alloca(
            toAddTo,
            "exceptionCaught",
            self.ourExceptionNotThrownState.unwrap().type_of(),
            Some(self.ourExceptionNotThrownState.unwrap().into()),
        );
        let exceptionStorageType = self.context.int8_t().ptr_t();
        let exceptionStorage = self.create_entry_block_alloca(
            toAddTo,
            "exceptionStorage",
            exceptionStorageType.into(),
            Some(exceptionStorageType.null()),
        );
        let caughtResultStorage = self.create_entry_block_alloca(
            toAddTo,
            "caughtResultStorage",
            self.ourCaughtResultType.unwrap().into(),
            Some(self.ourCaughtResultType.unwrap().null()),
        );

        let bb = toAddTo.append_basic_block_in_context(blockName, &self.context);

        builder.position_at_end(bb);

        self.generate_string_print(
            &builder,
            format!(
                "Gen: Executing finally block {} in {}",
                blockName,
                functionId
            ),
            USE_GLOBAL_STR_CONSTS,
        );

        switch!(load!(exceptionCaught).emit_to(&builder);
            _ => terminatorBlock,
            self.ourExceptionCaughtState.unwrap() => terminatorBlock,
            self.ourExceptionThrownState.unwrap() => unwindResumeBlock
        ).emit_to(&builder);

        (bb, exceptionCaught, exceptionStorage, caughtResultStorage)
    }

    fn create_entry_block_alloca(
        &self,
        func: Function,
        varName: &str,
        ty: TypeRef,
        init: Option<jit::Constant>,
    ) -> AllocaInst {
        let builder = self.context.create_builder();

        builder.position_at_end(func.entry().unwrap());

        let p = alloca!(ty; varName).emit_to(&builder);

        if let Some(v) = init {
            store!(v, p).emit_to(&builder);
        }

        p
    }

    /// Generates function which throws either an exception matched to a runtime
    /// determined type info type (argument to generated function), or if this
    /// runtime value matches nativeThrowType, throws a foreign exception by
    /// calling nativeThrowFunct.
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param fpm a function pass manager holding optional IR to IR
    ///        transformations
    /// @param ourId id used to printing purposes
    /// @param nativeThrowType a runtime argument of this value results in
    ///        nativeThrowFunct being called to generate/throw exception.
    /// @param nativeThrowFunct function which will throw a foreign exception
    ///        if the above nativeThrowType matches generated function's arg.
    /// @returns generated function
    fn create_throw_exception_function(
        &mut self,
        builder: &IRBuilder,
        ourId: &str,
        nativeThrowType: i32,
        nativeThrowFunct: Function,
    ) -> Function {
        let void_t = self.context.void_t();
        let i32_t = self.context.int32_t();

        let func = self.module.get_or_insert_function(ourId, void_t, &[i32_t]);

        let exceptionType = func.get_param(0).unwrap();

        exceptionType.set_name("exceptTypeToThrow");

        // Throws either one of our exception or a native C++ exception depending
        // on a runtime argument value containing a type info type.
        let entryBlock = func.append_basic_block_in_context("entry", &self.context);

        // Throws a foreign exception
        let nativeThrowBlock = func.append_basic_block_in_context("nativeThrow", &self.context);

        // Throws one of our Exceptions
        let generatedThrowBlock =
            func.append_basic_block_in_context("generatedThrow", &self.context);

        // nativeThrowBlock block
        builder.position_at_end(nativeThrowBlock);
        {
            // Throws foreign exception
            call!(nativeThrowFunct, exceptionType).emit_to(&builder);
            unreachable().emit_to(&builder);
        }

        // entry block
        builder.position_at_end(entryBlock);
        {
            let toPrint32Int = self.module.get_function("print32Int").unwrap();

            self.generate_integer_print(
                &builder,
                toPrint32Int,
                exceptionType,
                format!("\nGen: About to throw exception type <%d> in {} .\n", ourId),
                USE_GLOBAL_STR_CONSTS,
            );

            // Switches on runtime type info type value to determine whether or not
            // a foreign exception is thrown. Defaults to throwing one of our generated exceptions.
            let switch = switch!(exceptionType;
            _ => generatedThrowBlock,
            i32_t.int(nativeThrowType as i64) => nativeThrowBlock
        ).emit_to(&builder);
        }

        // generatedThrow block
        builder.position_at_end(generatedThrowBlock);
        {
            let createOurException = self.module.get_function("createOurException").unwrap();
            let raiseOurException = self.module.get_function("_Unwind_RaiseException").unwrap();

            // Creates exception to throw with runtime type info type.
            let exception = call!(createOurException, exceptionType).emit_to(&builder);

            // Throw generated Exception
            call!(raiseOurException, exception).emit_to(&builder);
            unreachable().emit_to(&builder);
        }

        //func.verify().unwrap();

        //self.fpm.run(func);

        func
    }

    /// Generates code to print given runtime integer according to constant
    /// string format, and a given print function.
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param printFunct function used to "print" integer
    /// @param toPrint string to print
    /// @param format printf like formating string for print
    /// @param useGlobal A value of true (default) indicates a GlobalValue is
    ///        generated, and is used to hold the constant string. A value of
    ///        false indicates that the constant string will be stored on the
    ///        stack.
    fn generate_integer_print<S: AsRef<str>, V: Into<ValueRef>>(
        &self,
        builder: &IRBuilder,
        printFunct: Function,
        toPrint: V,
        format: S,
        use_global: bool,
    ) {
        let i8_t = self.context.int8_t();

        let stringVar = if use_global {
            global_str_ptr!(format.as_ref()).emit_to(&builder)
        } else {
            let stringVar = alloca!(i8_t.array_t(format.as_ref().len())).emit_to(&builder);
            store(self.context.str(format.as_ref()), stringVar).emit_to(&builder);
            stringVar.into()
        };

        let cast = bit_cast!(stringVar, i8_t.ptr_t()).emit_to(&builder);

        call!(printFunct, toPrint, stringVar).emit_to(&builder);
    }

    /// Generates code to print given constant string
    /// @param context llvm context
    /// @param module code for module instance
    /// @param builder builder instance
    /// @param toPrint string to print
    /// @param useGlobal A value of true (default) indicates a GlobalValue is
    ///        generated, and is used to hold the constant string. A value of
    ///        false indicates that the constant string will be stored on the
    ///        stack.
    fn generate_string_print<S: AsRef<str>>(
        &self,
        builder: &IRBuilder,
        toPrint: S,
        use_global: bool,
    ) {
        let i8_t = self.context.int8_t();
        let printFunct = self.module.get_function("printStr").unwrap();

        let stringVar = if use_global {
            global_str!(toPrint.as_ref()).emit_to(&builder)
        } else {
            let stringVar = alloca!(i8_t.array_t(toPrint.as_ref().len())).emit_to(&builder);
            store(self.context.str(toPrint.as_ref()), stringVar).emit_to(&builder);
            stringVar.into()
        };

        let cast = ptr_cast!(stringVar, i8_t.ptr_t()).emit_to(&builder);

        call!(printFunct, cast).emit_to(&builder);
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
        numTypeInfos: usize,
        nativeThrowFunctName: &str,
    ) {
        let void_t = self.context.void_t();
        let i8_t = self.context.int8_t();
        let i32_t = self.context.int32_t();
        let i64_t = self.context.int64_t();

        // Exception initializations

        let dummyException: OurBaseException_t = unsafe { mem::zeroed() };

        self.ourBaseFromUnwindOffset = unsafe {
            mem::transmute::<_, usize>(&dummyException.unwindException) -
                mem::transmute::<_, usize>(&dummyException)
        };

        // Setup exception catch state
        self.ourExceptionNotThrownState = Some(i8_t.int(0));
        self.ourExceptionThrownState = Some(i8_t.int(1));
        self.ourExceptionCaughtState = Some(i8_t.int(2));

        // Create our type info type
        self.ourTypeInfoType = Some(self.context.named_struct_t("TypeInfo_t", &[i32_t], false));

        // Create our landingpad result type
        self.ourCaughtResultType = Some(self.context.named_struct_t(
            "CaughtResult_t",
            types![i8_t.ptr_t(), i32_t],
            false,
        ));

        // Create OurException type
        self.ourExceptionType = Some(self.context.named_struct_t(
            "OurBaseException_t",
            types![self.ourTypeInfoType.unwrap()],
            false,
        ));

        // Create portion of _Unwind_Exception type
        //
        // Note: Declaring only a portion of the _Unwind_Exception struct.
        //       Does this cause problems?
        self.ourUnwindExceptionType = Some(self.context.named_struct_t(
            "_Unwind_Exception",
            &[i64_t],
            false,
        ));

        // Generate each type info
        //
        // Note: First type info is not used.
        self.ourTypeInfoNames = (0..numTypeInfos + 1)
            .map(|i| {
                let typeInfoName = format!("typeInfo{}", i);

                self.module
                    .add_global_var(&typeInfoName, self.ourTypeInfoType.unwrap())
                    .set_initializer(self.ourTypeInfoType.unwrap().struct_of(
                        values![i32_t.int(i as i64)],
                    ));

                typeInfoName
            })
            .collect();

        // print32Int
        self.module.get_or_insert_function(
            "print32Int",
            void_t,
            types![i32_t, i8_t.ptr_t()],
        );

        // print64Int
        self.module.get_or_insert_function(
            "print64Int",
            void_t,
            types![i64_t, i8_t.ptr_t()],
        );

        // printStr
        self.module.get_or_insert_function(
            "printStr",
            void_t,
            types![i8_t.ptr_t()],
        );

        // nativeThrowFunctName
        self.module.get_or_insert_function(
            nativeThrowFunctName,
            void_t,
            &[i32_t],
        );

        // deleteOurException
        self.module.get_or_insert_function(
            "deleteOurException",
            void_t,
            types![i8_t.ptr_t()],
        );

        // createOurException
        self.module.get_or_insert_function(
            "createOurException",
            i8_t.ptr_t(),
            types![i32_t],
        );

        let no_return = self.context.create_enum_attribute(*jit::attrs::NoReturn, 0);

        // _Unwind_RaiseException
        self.module
            .get_or_insert_function("_Unwind_RaiseException", i32_t, types![i8_t.ptr_t()])
            .add_attribute(no_return);

        // _Unwind_Resume
        self.module
            .get_or_insert_function("_Unwind_Resume", i32_t, types![i8_t.ptr_t()])
            .add_attribute(no_return);

        // ourPersonality
        self.module.get_or_insert_function(
            "ourPersonality",
            i32_t,
            types![i32_t, i32_t, i64_t, i8_t.ptr_t(), i8_t.ptr_t()],
        );
    }
}

extern "C" fn panic_in_rust(_: i32) {
    panic!("thrown by panic_in_rust(...)")
}

fn run_exception_throw(ee: &ExecutionEngine, func: &Function, type_to_throw: u32) {}

fn main() {
    pretty_env_logger::init().unwrap();

    jit::target::NativeTarget::init().unwrap();
    jit::target::NativeAsmPrinter::init().unwrap();

    let mut example = Example::new();

    let func = example.create_unwind_exception_test("panic_in_rust");

    println!("\nBegin module dump:\n\n");

    example.module.dump();

    println!("\nEnd module dump:\n");

    println!("\n\nBegin Test:\n");

    let ee = ExecutionEngine::for_module(example.module).unwrap();

    ee.run_static_destructors();

    for arg in env::args().skip(1) {
        // Run test for each argument whose value is the exception type to throw.
        run_exception_throw(&ee, &func, arg.parse().unwrap())
    }

    println!("\nEnd Test:\n\n");
}
