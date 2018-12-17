use crate::llvm::{LLVMAtomicOrdering, LLVMAtomicRMWBinOp};
use crate::llvm::core::*;

use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::utils::{AsLLVMBool, AsRaw, IntoRaw};
use crate::value::Instruction;

/// The `fence` instruction is used to introduce happens-before edges between operations.
#[derive(Clone, Debug, PartialEq)]
pub struct Fence {
    ordering: LLVMAtomicOrdering,
    single_thread: bool,
}

impl Fence {
    /// The `fence` instruction is used to introduce happens-before edges between operations.
    pub fn new(ordering: LLVMAtomicOrdering, single_thread: bool) -> Self {
        Fence {
            ordering,
            single_thread,
        }
    }
}

impl InstructionBuilder for Fence {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildFence(builder.as_raw(), self.ordering, self.single_thread.as_bool(), cstr!("")) }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicRMW<'a> {
    op: LLVMAtomicRMWBinOp,
    ptr: Box<AstNode<'a>>,
    value: Box<AstNode<'a>>,
    ordering: LLVMAtomicOrdering,
    single_thread: bool,
}

impl<'a> AtomicRMW<'a> {
    pub fn new<P, V>(
        op: LLVMAtomicRMWBinOp,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Self
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        AtomicRMW {
            op,
            ptr: Box::new(ptr.into()),
            value: Box::new(value.into()),
            ordering,
            single_thread,
        }
    }
}

impl<'a> InstructionBuilder for AtomicRMW<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildAtomicRMW(
                builder.as_raw(),
                self.op,
                self.ptr.emit_to(builder).into_raw(),
                self.value.emit_to(builder).into_raw(),
                self.ordering,
                self.single_thread.as_bool(),
            )
        }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicCmpXchg<'a> {
    ptr: Box<AstNode<'a>>,
    cmp: Box<AstNode<'a>>,
    new: Box<AstNode<'a>>,
    success_ordering: LLVMAtomicOrdering,
    failure_ordering: LLVMAtomicOrdering,
    single_thread: bool,
}

impl<'a> AtomicCmpXchg<'a> {
    pub fn new<P, C, N>(
        ptr: P,
        cmp: C,
        new: N,
        success_ordering: LLVMAtomicOrdering,
        failure_ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Self
    where
        P: Into<AstNode<'a>>,
        C: Into<AstNode<'a>>,
        N: Into<AstNode<'a>>,
    {
        AtomicCmpXchg {
            ptr: Box::new(ptr.into()),
            cmp: Box::new(cmp.into()),
            new: Box::new(new.into()),
            success_ordering,
            failure_ordering,
            single_thread,
        }
    }
}

impl<'a> InstructionBuilder for AtomicCmpXchg<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildAtomicCmpXchg(
                builder.as_raw(),
                self.ptr.emit_to(builder).into_raw(),
                self.cmp.emit_to(builder).into_raw(),
                self.new.emit_to(builder).into_raw(),
                self.success_ordering,
                self.failure_ordering,
                self.single_thread.as_bool(),
            )
        }.into()
    }
}

/// `fence` instructions take an ordering argument which defines what synchronizes-with edges they add.
/// They can only be given `acquire`, `release`, `acq_rel`, and `seq_cst` orderings.
#[macro_export]
macro_rules! fence_ordering {
    (acquire) => {
        $crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire
    };
    (release) => {
        $crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingRelease
    };
    (acq_rel) => {
        $crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease
    };
    (seq_cst) => {
        $crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent
    };
    ($ordering: ident) => {
        "`fence` instruction can only be given acquire, release, acq_rel, and seq_cst orderings."
    };
}

/// The `fence` instruction is used to introduce happens-before edges between operations.
#[macro_export]
macro_rules! fence {
    (singlethread $ordering: ident) => {
        $crate::insts::Fence::new(fence_ordering!($ordering), true)
    };
    ($ordering: ident) => {
        $crate::insts::Fence::new(fence_ordering!($ordering), false)
    };
}

#[macro_export]
macro_rules! atomic_ordering {
    (not_atomic) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic);
    (unordered) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingUnordered);
    (monotonic) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic);
    (acquire) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire);
    (release) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingRelease);
    (acq_rel) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease);
    (seq_cst) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent);
    ($ordering:ident) => ("unknown atomic ordering" $ordering);
}

#[macro_export]
macro_rules! atomic_operation {
    (xchg) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXchg);
    (add) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd);
    (sub) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub);
    (and) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAnd);
    (nand) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpNand);
    (or) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpOr);
    (xor) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXor);
    (max) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMax);
    (min) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMin);
    (umax) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMax);
    (umin) => ($crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMin);
    ($op:ident) => ("unknown atomic operation" $op);
}

#[macro_export]
macro_rules! atomic {
    ($operation: ident $ptr: expr, $value: expr; $ordering: ident) => {
        $crate::insts::AtomicRMW::new(
            atomic_operation!($operation),
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false,
        )
    };
    (cmpxchg $ptr: expr, $cmp: expr, $new: expr; $ordering: ident) => {
        $crate::insts::AtomicCmpXchg::new(
            $ptr,
            $cmp,
            $new,
            atomic_ordering!($ordering),
            atomic_ordering!($ordering),
            false,
        )
    };
    (cmpxchg $ptr: expr, $cmp: expr, $new: expr; $success_ordering: ident $failure_ordering: ident) => {
        $crate::insts::AtomicCmpXchg::new(
            $ptr,
            $cmp,
            $new,
            atomic_ordering!($success_ordering),
            atomic_ordering!($failure_ordering),
            false,
        )
    };
}

impl IRBuilder {
    /// The `fence` instruction is used to introduce happens-before edges between operations.
    pub fn fence(&self, ordering: LLVMAtomicOrdering, single_thread: bool) -> Instruction {
        Fence::new(ordering, single_thread).emit_to(self)
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    pub fn atomic_rmw<'a, P, V>(
        &self,
        op: LLVMAtomicRMWBinOp,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        AtomicRMW::new(op, ptr, value, ordering, single_thread).emit_to(self)
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    ///
    /// xchg: *ptr = val
    pub fn atomic_xchg<'a, P, V>(
        &self,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        self.atomic_rmw(
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXchg,
            ptr,
            value,
            ordering,
            single_thread,
        )
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    ///
    /// add: *ptr = *ptr + val
    pub fn atomic_add<'a, P, V>(
        &self,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        self.atomic_rmw(
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
            ptr,
            value,
            ordering,
            single_thread,
        )
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    ///
    /// sub: *ptr = *ptr - val
    pub fn atomic_sub<'a, P, V>(
        &self,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        self.atomic_rmw(
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
            ptr,
            value,
            ordering,
            single_thread,
        )
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    ///
    /// and: *ptr = *ptr & val
    pub fn atomic_and<'a, P, V>(
        &self,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        self.atomic_rmw(
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAnd,
            ptr,
            value,
            ordering,
            single_thread,
        )
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    ///
    /// nand: *ptr = ~(*ptr & val)
    pub fn atomic_nand<'a, P, V>(
        &self,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        self.atomic_rmw(
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpNand,
            ptr,
            value,
            ordering,
            single_thread,
        )
    }

    /// The `atomicrmw` instruction is used to atomically modify memory.
    ///
    /// or: *ptr = *ptr | val
    pub fn atomic_or<'a, P, V>(
        &self,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        V: Into<AstNode<'a>>,
    {
        self.atomic_rmw(
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpOr,
            ptr,
            value,
            ordering,
            single_thread,
        )
    }

    /// The `cmpxchg` instruction is used to atomically modify memory.
    ///
    /// It loads a value in memory and compares it to a given value.
    /// If they are equal, it tries to store a new value into the memory.
    pub fn cmpxchg<'a, P, C, N>(
        &self,
        ptr: P,
        cmp: C,
        new: N,
        success_ordering: LLVMAtomicOrdering,
        failure_ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Instruction
    where
        P: Into<AstNode<'a>>,
        C: Into<AstNode<'a>>,
        N: Into<AstNode<'a>>,
    {
        AtomicCmpXchg::new(ptr, cmp, new, success_ordering, failure_ordering, single_thread).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

    macro_rules! test_atomic {
        ($builder: expr,atomic !($op: ident $ptr: expr, $value: expr; $ordering: ident), $display: expr) => {
            assert_eq!(
                atomic!($op $ptr, $value ; $ordering)
                    .emit_to(&$builder)
                    .to_string()
                    .trim(),
                $display
            )
        };
    }

    #[test]
    fn atomic() {
        let context = Context::new();
        let module = context.create_module("atomic");
        let builder = context.create_builder();

        let i64_t = context.int64_t();
        let p_i64_t = i64_t.ptr_t();

        let function_type = FunctionType::new(context.void_t(), types![p_i64_t], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let arg0_p_i64 = function.get_param(0).unwrap();

        test_atomic!(
            builder,
            atomic!(add arg0_p_i64, i64_t.int(123); unordered),
            "%1 = atomicrmw add i64* %0, i64 123 unordered"
        );
        test_atomic!(
            builder,
            atomic!(sub arg0_p_i64, i64_t.int(123); monotonic),
            "%2 = atomicrmw sub i64* %0, i64 123 monotonic"
        );
        test_atomic!(
            builder,
            atomic!(and arg0_p_i64, i64_t.int(123); acquire),
            "%3 = atomicrmw and i64* %0, i64 123 acquire"
        );
        test_atomic!(
            builder,
            atomic!(nand arg0_p_i64, i64_t.int(123); release),
            "%4 = atomicrmw nand i64* %0, i64 123 release"
        );
        test_atomic!(
            builder,
            atomic!(or arg0_p_i64, i64_t.int(123); acq_rel),
            "%5 = atomicrmw or i64* %0, i64 123 acq_rel"
        );
        test_atomic!(
            builder,
            atomic!(xor arg0_p_i64, i64_t.int(123); seq_cst),
            "%6 = atomicrmw xor i64* %0, i64 123 seq_cst"
        );
        test_atomic!(
            builder,
            atomic!(max arg0_p_i64, i64_t.int(123); unordered),
            "%7 = atomicrmw max i64* %0, i64 123 unordered"
        );
        test_atomic!(
            builder,
            atomic!(min arg0_p_i64, i64_t.int(123); unordered),
            "%8 = atomicrmw min i64* %0, i64 123 unordered"
        );
        test_atomic!(
            builder,
            atomic!(umax arg0_p_i64, i64_t.int(123); unordered),
            "%9 = atomicrmw umax i64* %0, i64 123 unordered"
        );
        test_atomic!(
            builder,
            atomic!(umin arg0_p_i64, i64_t.int(123); unordered),
            "%10 = atomicrmw umin i64* %0, i64 123 unordered"
        );

        assert_eq!(fence!(acquire).emit_to(&builder).to_string().trim(), "fence acquire");

        assert_eq!(
            fence!(singlethread acq_rel).emit_to(&builder).to_string().trim(),
            "fence syncscope(\"singlethread\") acq_rel"
        );

        assert_eq!(
            atomic!(cmpxchg arg0_p_i64, i64_t.int(123), i64_t.int(456); unordered unordered)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%11 = cmpxchg i64* %0, i64 123, i64 456 unordered unordered"
        );
    }
}
