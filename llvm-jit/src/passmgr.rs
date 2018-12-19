use std::ops::Deref;
use std::sync::{Once, ONCE_INIT};

use crate::llvm::core::*;
use crate::llvm::initialization::*;
use crate::llvm::prelude::*;
use crate::llvm::transforms::ipo::*;
use crate::llvm::transforms::pass_manager_builder::*;
use crate::llvm::transforms::scalar::*;
use crate::llvm::transforms::util::*;
use crate::llvm::transforms::vectorize::*;

use crate::context::Context;
use crate::function::Function;
use crate::module::Module;
use crate::utils::{AsBool, AsLLVMBool, AsRaw};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pass {
    /// Promote `by reference’ arguments to scalars
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

    /// Attach metadata to indirct call sites indicating the set of functions they may target at run-time.
    CalledValuePropagation,

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

    /// Create a loop vectorization pass.
    LoopVectorize,

    /// Create a bottom-up SLP vectorizer pass.
    SLPVectorizer,

    /// Aggressive Dead Code Elimination
    ///
    /// ADCE aggressively tries to eliminate code.
    ///
    /// This pass is similar to DCE but it assumes that values are dead until proven otherwise.
    /// This is similar to SCCP, except applied to the liveness of values.
    AggressiveDCE,

    /// Bit Tracking Dead Code Elimination
    ///
    /// This pass uses a bit-tracking DCE algorithm in order to remove computations of dead bits.
    BitTrackingDCE,

    /// Alignment From Assumptions
    ///
    /// Use assume intrinsics to set load/store alignments.
    AlignmentFromAssumptions,

    /// Simplify the CFG
    ///
    /// Merge basic blocks, eliminate unreachable blocks, simplify terminator instructions, etc...
    CFGSimplification,

    /// Dead Store Elimination
    ///
    /// A trivial dead store elimination that only considers basic-block local redundant stores.
    /// This pass deletes stores that are post-dominated by must-aliased stores and are not loaded used between the stores.
    DeadStoreElimination,

    /// Converts vector operations into scalar operations
    Scalarizer,

    /// Merged Load Store Motion
    ///
    /// This pass merges loads and stores in diamonds.
    /// Loads are hoisted into the header, while stores sink into the footer.
    MergedLoadStoreMotion,

    /// Global Value Numbering
    ///
    /// This pass performs global value numbering to eliminate fully and partially redundant instructions.
    /// It also performs redundant load elimination.
    GVN,

    /// This pass performs global value numbering and redundant load elimination cotemporaneously.
    NewGVN,

    /// Induction Variable Simplify
    ///
    /// Transform induction variables in a program to all use a single canonical induction variable per loop.
    IndVarSimplify,

    /// Instruction Combining
    ///
    /// Combine instructions to form fewer, simple instructions.
    /// This pass does not modify the CFG, and has a tendency to make instructions dead,
    /// so a subsequent DCE pass is useful.
    ///
    /// This pass combines things like:
    ///    %Y = add int 1, %X
    ///    %Z = add int 1, %Y
    /// into:
    ///    %Z = add int 2, %X
    InstructionCombining,

    /// Jump Threading
    ///
    /// Thread control through mult-pred/multi-succ blocks where some preds always go to some succ.
    /// Thresholds other than minus one override the internal BB duplication default threshold.
    JumpThreading,

    /// Loop Invariant Code Motion
    ///
    /// This pass is a loop invariant code motion and memory promotion pass.
    ///
    /// This pass performs loop invariant code motion,
    /// attempting to remove as much code from the body of a loop as possible.
    /// It does this by either hoisting code into the preheader block,
    /// or by sinking code to the exit blocks if it is safe.
    /// This pass also promotes must-aliased memory locations in the loop to live in registers,
    /// thus hoisting and sinking “invariant” loads and stores.
    LICM,

    /// Loop Deletion
    ///
    /// This pass performs DCE of non-infinite loops that it can prove are dead.
    LoopDeletion,

    /// LoopIdiom - This pass recognizes and replaces idioms in loops.
    LoopIdiom,

    /// LoopRotate - This pass is a simple loop rotating pass.
    LoopRotate,

    /// LoopReroll - This pass is a simple loop rerolling pass.
    LoopReroll,

    /// LoopUnroll - This pass is a simple loop unrolling pass.
    LoopUnroll,

    /// LoopUnswitch - This pass is a simple loop unswitching pass.
    LoopUnswitch,

    /// MemCpyOpt - This pass performs optimizations related to eliminating memcpy
    /// calls and/or combining multiple stores into memset's.
    MemCpyOpt,

    /// PartiallyInlineLibCalls - Tries to inline the fast path of library calls such as sqrt.
    PartiallyInlineLibCalls,

    /// LowerSwitch - This pass converts SwitchInst instructions into a sequence of chained binary branch instructions.
    LowerSwitch,

    /// PromoteMemoryToRegister - This pass is used to promote memory references to be register references.
    /// A simple example of the transformation performed by this pass is:
    /// ```
    ///        FROM CODE                           TO CODE
    ///   %X = alloca i32, i32 1                 ret i32 42
    ///   store i32 42, i32 *%X
    ///   %Y = load i32* %X
    ///   ret i32 %Y
    /// ```
    PromoteMemoryToRegister,

    /// Reassociate - This pass reassociates commutative expressions in an order
    /// that is designed to promote better constant propagation, GCSE, LICM, PRE...
    ///
    /// For example:  4 + (x + 5)  ->  x + (4 + 5)
    ///
    Reassociate,

    /// SCCP - Sparse conditional constant propagation.
    SCCP,

    /// SROA - Replace aggregates or pieces of aggregates with scalar SSA values.
    ScalarReplAggregates,

    /// SROA - Replace aggregates or pieces of aggregates with scalar SSA values.
    ScalarReplAggregatesSSA,

    /// SROA - Replace aggregates or pieces of aggregates with scalar SSA values.
    ScalarReplAggregatesWithThreshold(i32),

    /// The simplify-libcalls pass has been removed.
    SimplifyLibCalls,

    /// TailCallElimination - This pass eliminates call instructions to the current
    /// function which occur immediately before return instructions.
    TailCallElimination,

    /// ConstantPropagation - A worklist driven constant propagation pass
    ConstantPropagation,

    /// DemoteRegisterToMemoryPass - This pass is used to demote registers to memory references.
    /// In basically undoes the PromoteMemoryToRegister pass to make cfg hacking easier.
    DemoteMemoryToRegister,

    /// Check a module for errors, and report separate error states for IR and debug info errors.
    Verifier,

    /// ValuePropagation - Propagate CFG-derived value information
    CorrelatedValuePropagation,

    /// EarlyCSE - This pass performs a simple and fast CSE pass over the dominator tree.
    EarlyCSE,

    /// EarlyCSE - This pass performs a simple and fast CSE pass over the dominator tree.
    EarlyCSEMemSSA,

    /// LowerExpectIntrinsics - Removes llvm.expect intrinsics and creates "block_weights" metadata.
    LowerExpectIntrinsic,

    /// TypeBasedAAWrapper - This pass implements metadata-based type-based alias analysis.
    TypeBasedAliasAnalysis,

    /// ScopedNoAliasAAWrapperPass - This pass implements metadata-based scoped noalias analysis.
    ScopedNoAliasAA,

    /// Basic Alias Analysis (stateless AA impl)
    ///
    /// A basic alias analysis pass that implements identities (two different globals cannot alias, etc),
    /// but does no stateful analysis.
    BasicAliasAnalysis,
}

#[repr(transparent)]
#[derive(Debug)]
pub struct PassManager(LLVMPassManagerRef);

inherit_from!(PassManager, LLVMPassManagerRef);

impl Drop for PassManager {
    fn drop(&mut self) {
        unsafe { LLVMDisposePassManager(self.0) }
    }
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PassManager {
    /// Constructs a new whole-module pass pipeline.
    ///
    /// This type of pipeline is suitable for link-time optimization and whole-module transformations.
    pub fn new() -> Self {
        init();

        unsafe { LLVMCreatePassManager() }.into()
    }

    /// Initializes, executes on the provided module, and finalizes all of the passes scheduled in the pass manager.
    pub fn run(&self, module: &Module) -> bool {
        unsafe { LLVMRunPassManager(self.0, module.as_raw()) }.as_bool()
    }

    /// add pass to pipeline
    ///
    /// @see: https://llvm.org/docs/Passes.html#bb-vectorize-basic-block-vectorization
    pub fn add(&self, pass: Pass) {
        match pass {
            // Interprocedural transformations
            Pass::ArgumentPromotion => unsafe { LLVMAddArgumentPromotionPass(self.as_raw()) },
            Pass::ConstantMerge => unsafe { LLVMAddConstantMergePass(self.as_raw()) },
            Pass::CalledValuePropagation => unsafe { LLVMAddCalledValuePropagationPass(self.as_raw()) },
            Pass::DeadArgElimination => unsafe { LLVMAddDeadArgEliminationPass(self.as_raw()) },
            Pass::FunctionAttrs => unsafe { LLVMAddFunctionAttrsPass(self.as_raw()) },
            Pass::FunctionInlining => unsafe { LLVMAddFunctionInliningPass(self.as_raw()) },
            Pass::AlwaysInliner => unsafe { LLVMAddAlwaysInlinerPass(self.as_raw()) },
            Pass::GlobalDCE => unsafe { LLVMAddGlobalDCEPass(self.as_raw()) },
            Pass::GlobalOptimizer => unsafe { LLVMAddGlobalOptimizerPass(self.as_raw()) },
            Pass::IPConstantPropagation => unsafe { LLVMAddIPConstantPropagationPass(self.as_raw()) },
            Pass::PruneEH => unsafe { LLVMAddPruneEHPass(self.as_raw()) },
            Pass::IPSCCP => unsafe { LLVMAddIPSCCPPass(self.as_raw()) },
            Pass::Internalize(all_but_main) => unsafe {
                LLVMAddInternalizePass(self.as_raw(), all_but_main.as_bool() as u32)
            },
            Pass::StripDeadPrototypes => unsafe { LLVMAddStripDeadPrototypesPass(self.as_raw()) },
            Pass::StripSymbols => unsafe { LLVMAddStripSymbolsPass(self.as_raw()) },

            // Vectorization transformations
            Pass::LoopVectorize => unsafe { LLVMAddLoopVectorizePass(self.as_raw()) },
            Pass::SLPVectorizer => unsafe { LLVMAddSLPVectorizePass(self.as_raw()) },

            // Scalar transformations
            Pass::AggressiveDCE => unsafe { LLVMAddAggressiveDCEPass(self.as_raw()) },
            Pass::BitTrackingDCE => unsafe { LLVMAddBitTrackingDCEPass(self.as_raw()) },
            Pass::AlignmentFromAssumptions => unsafe { LLVMAddAlignmentFromAssumptionsPass(self.as_raw()) },
            Pass::CFGSimplification => unsafe { LLVMAddCFGSimplificationPass(self.as_raw()) },
            Pass::DeadStoreElimination => unsafe { LLVMAddDeadStoreEliminationPass(self.as_raw()) },
            Pass::Scalarizer => unsafe { LLVMAddScalarizerPass(self.as_raw()) },
            Pass::MergedLoadStoreMotion => unsafe { LLVMAddMergedLoadStoreMotionPass(self.as_raw()) },
            Pass::GVN => unsafe { LLVMAddGVNPass(self.as_raw()) },
            Pass::NewGVN => unsafe { LLVMAddNewGVNPass(self.as_raw()) },
            Pass::IndVarSimplify => unsafe { LLVMAddIndVarSimplifyPass(self.as_raw()) },
            Pass::InstructionCombining => unsafe { LLVMAddInstructionCombiningPass(self.as_raw()) },
            Pass::JumpThreading => unsafe { LLVMAddJumpThreadingPass(self.as_raw()) },
            Pass::LICM => unsafe { LLVMAddLICMPass(self.as_raw()) },
            Pass::LoopDeletion => unsafe { LLVMAddLoopDeletionPass(self.as_raw()) },
            Pass::LoopIdiom => unsafe { LLVMAddLoopIdiomPass(self.as_raw()) },
            Pass::LoopRotate => unsafe { LLVMAddLoopRotatePass(self.as_raw()) },
            Pass::LoopReroll => unsafe { LLVMAddLoopRerollPass(self.as_raw()) },
            Pass::LoopUnroll => unsafe { LLVMAddLoopUnrollPass(self.as_raw()) },
            Pass::LoopUnswitch => unsafe { LLVMAddLoopUnswitchPass(self.as_raw()) },
            Pass::MemCpyOpt => unsafe { LLVMAddMemCpyOptPass(self.as_raw()) },
            Pass::PartiallyInlineLibCalls => unsafe { LLVMAddPartiallyInlineLibCallsPass(self.as_raw()) },
            Pass::LowerSwitch => unsafe { LLVMAddLowerSwitchPass(self.as_raw()) },
            Pass::PromoteMemoryToRegister => unsafe { LLVMAddPromoteMemoryToRegisterPass(self.as_raw()) },
            Pass::Reassociate => unsafe { LLVMAddReassociatePass(self.as_raw()) },
            Pass::SCCP => unsafe { LLVMAddSCCPPass(self.as_raw()) },
            Pass::ScalarReplAggregates => unsafe { LLVMAddScalarReplAggregatesPass(self.as_raw()) },
            Pass::ScalarReplAggregatesSSA => unsafe { LLVMAddScalarReplAggregatesPassSSA(self.as_raw()) },
            Pass::ScalarReplAggregatesWithThreshold(threshold) => unsafe {
                LLVMAddScalarReplAggregatesPassWithThreshold(self.as_raw(), threshold)
            },
            Pass::SimplifyLibCalls => unsafe { LLVMAddSimplifyLibCallsPass(self.as_raw()) },
            Pass::TailCallElimination => unsafe { LLVMAddTailCallEliminationPass(self.as_raw()) },
            Pass::ConstantPropagation => unsafe { LLVMAddConstantPropagationPass(self.as_raw()) },
            Pass::DemoteMemoryToRegister => unsafe { LLVMAddDemoteMemoryToRegisterPass(self.as_raw()) },
            Pass::Verifier => unsafe { LLVMAddVerifierPass(self.as_raw()) },
            Pass::CorrelatedValuePropagation => unsafe { LLVMAddCorrelatedValuePropagationPass(self.as_raw()) },
            Pass::EarlyCSE => unsafe { LLVMAddEarlyCSEPass(self.as_raw()) },
            Pass::EarlyCSEMemSSA => unsafe { LLVMAddEarlyCSEMemSSAPass(self.as_raw()) },
            Pass::LowerExpectIntrinsic => unsafe { LLVMAddLowerExpectIntrinsicPass(self.as_raw()) },
            Pass::TypeBasedAliasAnalysis => unsafe { LLVMAddTypeBasedAliasAnalysisPass(self.as_raw()) },
            Pass::ScopedNoAliasAA => unsafe { LLVMAddScopedNoAliasAAPass(self.as_raw()) },
            Pass::BasicAliasAnalysis => unsafe { LLVMAddBasicAliasAnalysisPass(self.as_raw()) },
        }
    }
}

#[repr(transparent)]
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
    pub fn run(&self, func: &Function) -> bool {
        unsafe { LLVMRunFunctionPassManager(self.as_raw(), func.as_raw()) }.as_bool()
    }

    /// Finalizes all of the function passes scheduled in in the function pass manager.
    pub fn finalize(&self) -> bool {
        unsafe { LLVMFinalizeFunctionPassManager(self.as_raw()) }.as_bool()
    }
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq)]
pub struct PassRegistry(LLVMPassRegistryRef);

inherit_from!(PassRegistry, LLVMPassRegistryRef);

impl PassRegistry {
    /// Return the global pass registry, for use with initialization functions.
    pub fn global() -> Self {
        unsafe { LLVMGetGlobalPassRegistry() }.into()
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct PassManagerBuilder(LLVMPassManagerBuilderRef);

inherit_from!(PassManagerBuilder, LLVMPassManagerBuilderRef);

impl Drop for PassManagerBuilder {
    fn drop(&mut self) {
        unsafe { LLVMPassManagerBuilderDispose(self.as_raw()) }
    }
}

impl Default for PassManagerBuilder {
    fn default() -> Self {
        Self::new()
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
    pub fn set_opt_level(&self, opt_level: u32) -> &Self {
        unsafe { LLVMPassManagerBuilderSetOptLevel(self.as_raw(), opt_level) };

        self
    }

    /// How much we're optimizing for size.
    ///
    ///  - 0: none
    ///  - 1: -Os
    ///  - 2: -Oz
    pub fn set_size_level(&self, size_level: u32) -> &Self {
        unsafe { LLVMPassManagerBuilderSetSizeLevel(self.as_raw(), size_level) };

        self
    }

    pub fn set_disable_unit_at_atime(&self, v: bool) -> &Self {
        unsafe { LLVMPassManagerBuilderSetDisableUnitAtATime(self.as_raw(), v.as_bool()) };

        self
    }

    pub fn set_disable_unroll_loops(&self, v: bool) -> &Self {
        unsafe { LLVMPassManagerBuilderSetDisableUnrollLoops(self.as_raw(), v.as_bool()) };

        self
    }

    pub fn use_inliner_with_threshold(&self, threshold: u32) -> &Self {
        unsafe { LLVMPassManagerBuilderUseInlinerWithThreshold(self.as_raw(), threshold) };

        self
    }

    pub fn populate_function_pass_manager<M: Deref<Target = PassManager>>(&self, mgr: M) {
        unsafe { LLVMPassManagerBuilderPopulateFunctionPassManager(self.as_raw(), mgr.as_raw()) }
    }

    pub fn populate_module_pass_manager<M: Deref<Target = PassManager>>(&self, mgr: M) {
        unsafe { LLVMPassManagerBuilderPopulateModulePassManager(self.as_raw(), mgr.as_raw()) }
    }

    pub fn populate_lto_pass_manager<M: Deref<Target = PassManager>>(
        &self,
        mgr: M,
        internalize: bool,
        run_inliner: bool,
    ) {
        unsafe {
            LLVMPassManagerBuilderPopulateLTOPassManager(
                self.as_raw(),
                mgr.as_raw(),
                internalize.as_bool(),
                run_inliner.as_bool(),
            )
        }
    }
}

static INIT: Once = ONCE_INIT;

fn init() {
    INIT.call_once(|| {
        let _ = Context::global();

        PassRegistry::global().with(|p| unsafe {
            LLVMInitializeCore(p);
            LLVMInitializeTransformUtils(p);
            LLVMInitializeScalarOpts(p);
            LLVMInitializeObjCARCOpts(p);
            LLVMInitializeVectorization(p);
            LLVMInitializeInstCombine(p);;
            LLVMInitializeIPO(p);
            LLVMInitializeInstrumentation(p);
            LLVMInitializeAnalysis(p);
            LLVMInitializeCodeGen(p);
            LLVMInitializeTarget(p);
        })
    })
}
