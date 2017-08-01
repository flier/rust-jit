use std::ops::Deref;

use llvm::core::*;
use llvm::prelude::*;
use llvm::transforms::ipo::*;
use llvm::transforms::pass_manager_builder::*;

use function::Function;
use module::Module;
use utils::{AsBool, AsLLVMBool};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pass {
    /// Promote ‘by reference’ arguments to scalars
    ///
    /// This pass promotes “by reference” arguments to be “by value” arguments.
    ///
    /// In practice, this means looking for internal functions that have pointer arguments.
    /// If it can prove, through the use of alias analysis, that an argument is only loaded,
    /// then it can pass the value into the function instead of the address of the value.
    /// This can cause recursive simplification of code and lead to the elimination of allocas
    /// (especially in C++ template code like the STL).
    ArgumentPromotion,

    /// Merge Duplicate Global Constants
    ///
    /// Merges duplicate global constants together into a single constant that is shared.
    ///
    /// This is useful because some passes (i.e., TraceValues) insert a lot of string constants into the program,
    /// regardless of whether or not an existing string is available.
    ConstantMerge,

    /// Dead Argument Elimination
    ///
    /// This pass deletes dead arguments from internal functions.
    ///
    /// Dead argument elimination removes arguments which are directly dead,
    /// as well as arguments only passed into function calls as dead arguments of other functions.
    /// This pass also deletes dead arguments in a similar way.
    DeadArgElimination,

    /// Deduce function attributes
    ///
    /// A simple interprocedural pass which walks the call-graph,
    /// looking for functions which do not access or only read non-local memory,
    /// and marking them readnone/readonly.
    FunctionAttrs,

    /// Function Integration/Inlining
    ///
    /// Bottom-up inlining of functions into callees.
    FunctionInlining,

    /// Inliner for always_inline functions
    ///
    /// A custom inliner that handles only functions that are marked as “always inline”.
    AlwaysInliner,

    /// Dead Global Elimination
    ///
    /// This transform is designed to eliminate unreachable internal globals from the program.
    ///
    /// It uses an aggressive algorithm, searching out globals that are known to be alive.
    /// After it finds all of the globals which are needed, it deletes whatever is left over.
    /// This allows it to delete recursive chunks of the program which are unreachable.
    GlobalDCE,

    /// Global Variable Optimizer
    ///
    /// This pass transforms simple global variables that never have their address taken.
    ///
    /// If obviously true, it marks read/write globals as constant, deletes variables only stored to, etc.
    GlobalOptimizer,

    /// Interprocedural constant propagation
    ///
    /// This pass implements an extremely simple interprocedural constant propagation pass.
    ///
    /// It could certainly be improved in many different ways, like using a worklist.
    /// This pass makes arguments dead, but does not remove them.
    /// The existing dead argument elimination pass should be run after this to clean up the mess.
    IPConstantPropagation,

    /// Remove unused exception handling info
    ///
    /// This file implements a simple interprocedural pass which walks the call-graph,
    /// turning invoke instructions into call instructions if and only if the callee cannot throw an exception.
    /// It implements this as a bottom-up traversal of the call-graph.
    PruneEH,

    /// Interprocedural Sparse Conditional Constant Propagation
    ///
    /// An interprocedural variant of Sparse Conditional Constant Propagation.
    ///
    /// @see: https://llvm.org/docs/Passes.html#passes-sccp
    IPSCCP,

    /// Internalize Global Symbols
    ///
    /// This pass loops over all of the functions in the input module, looking for a main function.
    /// If a main function is found, all other functions and all global variables with initializers are marked as internal.
    Internalize(bool),

    /// Strip Unused Function Prototypes
    ///
    /// This pass loops over all of the functions in the input module, looking for dead declarations and removes them.
    /// Dead declarations are declarations of functions for which no implementation is available
    /// (i.e., declarations for unused library functions).
    StripDeadPrototypes,

    /// Strip all symbols from a module
    ///
    /// Performs code stripping. This transformation can delete:
    ///
    ///  - names for virtual registers
    ///  - symbols for internal globals and functions
    ///  - debug information
    ///
    /// Note that this transformation makes code much less readable,
    /// so it should only be used in situations where the strip utility would be used,
    /// such as reducing code size or making it harder to reverse engineer code.
    StripSymbols,
}

#[derive(Debug)]
pub struct PassManager(LLVMPassManagerRef);

inherit_from!(PassManager, LLVMPassManagerRef);

impl Drop for PassManager {
    fn drop(&mut self) {
        unsafe { LLVMDisposePassManager(self.0) }
    }
}

impl PassManager {
    /// Constructs a new whole-module pass pipeline.
    ///
    /// This type of pipeline is suitable for link-time optimization and whole-module transformations.
    pub fn new() -> Self {
        unsafe { LLVMCreatePassManager() }.into()
    }

    /// Initializes, executes on the provided module, and finalizes all of the passes scheduled in the pass manager.
    pub fn run(&self, module: &Module) -> bool {
        unsafe { LLVMRunPassManager(self.0, module.as_raw()) }.as_bool()
    }

    pub fn add_pass(&self, pass: Pass) {
        match pass {
            Pass::ArgumentPromotion => unsafe { LLVMAddArgumentPromotionPass(self.as_raw()) },
            Pass::ConstantMerge => unsafe { LLVMAddConstantMergePass(self.as_raw()) },
            Pass::DeadArgElimination => unsafe { LLVMAddDeadArgEliminationPass(self.as_raw()) },
            Pass::FunctionAttrs => unsafe { LLVMAddFunctionAttrsPass(self.as_raw()) },
            Pass::FunctionInlining => unsafe { LLVMAddFunctionInliningPass(self.as_raw()) },
            Pass::AlwaysInliner => unsafe { LLVMAddAlwaysInlinerPass(self.as_raw()) },
            Pass::GlobalDCE => unsafe { LLVMAddGlobalDCEPass(self.as_raw()) },
            Pass::GlobalOptimizer => unsafe { LLVMAddGlobalOptimizerPass(self.as_raw()) },
            Pass::IPConstantPropagation => unsafe {
                LLVMAddIPConstantPropagationPass(self.as_raw())
            },
            Pass::PruneEH => unsafe { LLVMAddPruneEHPass(self.as_raw()) },
            Pass::IPSCCP => unsafe { LLVMAddIPSCCPPass(self.as_raw()) },
            Pass::Internalize(all_but_main) => unsafe {
                LLVMAddInternalizePass(self.as_raw(), all_but_main.as_bool() as u32)
            },
            Pass::StripDeadPrototypes => unsafe { LLVMAddStripDeadPrototypesPass(self.as_raw()) },
            Pass::StripSymbols => unsafe { LLVMAddStripSymbolsPass(self.as_raw()) },
        }
    }
}

#[derive(Debug)]
pub struct FunctionPassManager(PassManager);

inherit_from!(FunctionPassManager, PassManager, LLVMPassManagerRef);

impl FunctionPassManager {
    /// Constructs a new function-by-function pass pipeline over the module provider.
    ///
    /// It does not take ownership of the module provider.
    /// This type of pipeline is suitable for code generation and JIT compilation tasks.
    pub fn for_module(module: &Module) -> Self {
        unsafe { LLVMCreateFunctionPassManagerForModule(module.as_raw()) }.into()
    }

    /// Initializes all of the function passes scheduled in the function pass manager.
    pub fn init(&self) -> bool {
        unsafe { LLVMInitializeFunctionPassManager(self.as_raw()) }.as_bool()
    }

    /// Executes all of the function passes scheduled in the function pass manager on the provided function.
    pub fn run(&self, func: Function) -> bool {
        unsafe { LLVMRunFunctionPassManager(self.as_raw(), func.as_raw()) }.as_bool()
    }

    /// Finalizes all of the function passes scheduled in in the function pass manager.
    pub fn finalize(&self) -> bool {
        unsafe { LLVMFinalizeFunctionPassManager(self.as_raw()) }.as_bool()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PassRegistry(LLVMPassRegistryRef);

inherit_from!(PassRegistry, LLVMPassRegistryRef);

impl PassRegistry {
    /// Return the global pass registry, for use with initialization functions.
    pub fn global() -> Self {
        unsafe { LLVMGetGlobalPassRegistry() }.into()
    }
}

#[derive(Debug)]
pub struct PassManagerBuilder(LLVMPassManagerBuilderRef);

inherit_from!(PassManagerBuilder, LLVMPassManagerBuilderRef);

impl Drop for PassManagerBuilder {
    fn drop(&mut self) {
        unsafe { LLVMPassManagerBuilderDispose(self.as_raw()) }
    }
}

impl PassManagerBuilder {
    pub fn new() -> Self {
        unsafe { LLVMPassManagerBuilderCreate() }.into()
    }

    /// The Optimization Level - Specify the basic optimization level.
    ///
    ///  - 0: -O0
    ///  - 1: -O1
    ///  - 2: -O2
    ///  - 3: -O3
    pub fn set_opt_level(&mut self, opt_level: u32) -> &mut Self {
        unsafe { LLVMPassManagerBuilderSetOptLevel(self.as_raw(), opt_level) };

        self
    }

    /// How much we're optimizing for size.
    ///
    ///  - 0: none
    ///  - 1: -Os
    ///  - 2: -Oz
    pub fn set_size_level(&mut self, size_level: u32) -> &mut Self {
        unsafe { LLVMPassManagerBuilderSetSizeLevel(self.as_raw(), size_level) };

        self
    }

    pub fn set_disable_unit_at_atime(&mut self, v: bool) -> &mut Self {
        unsafe { LLVMPassManagerBuilderSetDisableUnitAtATime(self.as_raw(), v.as_bool()) };

        self
    }

    pub fn set_disable_unroll_loops(&mut self, v: bool) -> &mut Self {
        unsafe { LLVMPassManagerBuilderSetDisableUnrollLoops(self.as_raw(), v.as_bool()) };

        self
    }

    pub fn use_inliner_with_threshold(&mut self, threshold: u32) -> &mut Self {
        unsafe { LLVMPassManagerBuilderUseInlinerWithThreshold(self.as_raw(), threshold) };

        self
    }

    pub fn populate_function_pass_manager<M: AsRef<Deref<Target = PassManager>>>(&self, mgr: M) {
        unsafe {
            LLVMPassManagerBuilderPopulateFunctionPassManager(self.as_raw(), mgr.as_ref().as_raw())
        }
    }

    pub fn populate_module_pass_manager<M: AsRef<Deref<Target = PassManager>>>(&self, mgr: M) {
        unsafe {
            LLVMPassManagerBuilderPopulateModulePassManager(self.as_raw(), mgr.as_ref().as_raw())
        }
    }

    pub fn populate_lto_pass_manager<M: AsRef<Deref<Target = PassManager>>>(
        &self,
        mgr: M,
        internalize: bool,
        run_inliner: bool,
    ) {
        unsafe {
            LLVMPassManagerBuilderPopulateLTOPassManager(
                self.as_raw(),
                mgr.as_ref().as_raw(),
                internalize.as_bool(),
                run_inliner.as_bool(),
            )
        }
    }
}
