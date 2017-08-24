use std::borrow::Cow;
use std::fmt;
use std::mem;

use llvm::{LLVMAtomicOrdering, LLVMAtomicRMWBinOp};
use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::{AsLLVMBool, AsRaw};
use value::Instruction;

#[derive(Clone, Debug, PartialEq)]
pub struct Fence<'a> {
    ordering: u32, // TODO: LLVMAtomicOrdering
    single_thread: bool,
    name: Cow<'a, str>,
}

impl<'a> Fence<'a> {
    pub fn new(ordering: LLVMAtomicOrdering, single_thread: bool, name: Cow<'a, str>) -> Self {
        Fence {
            ordering: ordering as u32,
            single_thread,
            name,
        }
    }
}

impl<'a> InstructionBuilder for Fence<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildFence(
                builder.as_raw(),
                mem::transmute(self.ordering),
                self.single_thread.as_bool(),
                cstr!(self.name),
            )
        }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicRMW<P, V> {
    op: u32, // TODO: LLVMAtomicRMWBinOp
    ptr: P,
    value: V,
    ordering: u32, // TODO: LLVMAtomicOrdering
    single_thread: bool,
}

impl<P, V> AtomicRMW<P, V> {
    pub fn new(
        op: LLVMAtomicRMWBinOp,
        ptr: P,
        value: V,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Self {
        AtomicRMW {
            op: op as u32,
            ptr,
            value,
            ordering: ordering as u32,
            single_thread,
        }
    }
}

impl<P, V> InstructionBuilder for AtomicRMW<P, V>
where
    P: InstructionBuilder + fmt::Debug,
    V: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildAtomicRMW(
                builder.as_raw(),
                mem::transmute(self.op),
                self.ptr.emit_to(builder).into().as_raw(),
                self.value.emit_to(builder).into().as_raw(),
                mem::transmute(self.ordering),
                self.single_thread.as_bool(),
            )
        }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicCmpXchg<P, C, N> {
    ptr: P,
    cmp: C,
    new: N,
    success_ordering: u32, // TODO: LLVMAtomicOrdering
    failure_ordering: u32, // TODO: LLVMAtomicOrdering
    single_thread: bool,
}

impl<P, C, N> AtomicCmpXchg<P, C, N> {
    pub fn new(
        ptr: P,
        cmp: C,
        new: N,
        success_ordering: LLVMAtomicOrdering,
        failure_ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Self {
        AtomicCmpXchg {
            ptr,
            cmp,
            new,
            success_ordering: success_ordering as u32,
            failure_ordering: failure_ordering as u32,
            single_thread,
        }
    }
}

impl<P, C, N> InstructionBuilder for AtomicCmpXchg<P, C, N>
where
    P: InstructionBuilder + fmt::Debug,
    C: InstructionBuilder + fmt::Debug,
    N: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildAtomicCmpXchg(
                builder.as_raw(),
                self.ptr.emit_to(builder).into().as_raw(),
                self.cmp.emit_to(builder).into().as_raw(),
                self.new.emit_to(builder).into().as_raw(),
                mem::transmute(self.success_ordering),
                mem::transmute(self.failure_ordering),
                self.single_thread.as_bool(),
            )
        }.into()
    }
}

/// `fence` instructions take an ordering argument which defines what synchronizes-with edges they add.
/// They can only be given `acquire`, `release`, `acq_rel`, and `seq_cst` orderings.
#[macro_export]
macro_rules! fence_ordering {
    (acquire) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire);
    (release) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingRelease);
    (acq_rel) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease);
    (seq_cst) => ($crate::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent);
    ($ordering:ident) => ("`fence` instruction can only be given acquire, release, acq_rel, and seq_cst orderings.");
}

/// The `fence` instruction is used to introduce happens-before edges between operations.
#[macro_export]
macro_rules! fence {
    (singlethread $ordering:ident ; $name:expr) => (
        $crate::insts::Fence::new(
            fence_ordering!($ordering),
            true,
            $name.into(),
        )
    );
    ($ordering:ident ; $name:expr) => (
        $crate::insts::Fence::new(
            fence_ordering!($ordering),
            false,
            $name.into(),
        )
    );
    (singlethread $ordering:ident) => (
        fence!(singlethread $ordering; format!("fence_{}", stringify!($ordering)))
    );
    ($ordering:ident) => (
        fence!($ordering; format!("fence_{}", stringify!($ordering)))
    );
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
    ($ordering:ident) => ("unknown atomic ordering");
}

#[macro_export]
macro_rules! atomic {
    (xchg $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXchg,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (add $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (sub $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (and $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAnd,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (nand $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpNand,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (or $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpOr,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (xor $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXor,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (max $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMax,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (min $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMin,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (xor $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXor,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (umax $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMax,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (umin $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::insts::AtomicRMW::new(
            $crate::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMin,
            $ptr,
            $value,
            atomic_ordering!($ordering),
            false
        )
    );
    (cmpxchg $ptr:expr, $cmp:expr, $new:expr; $success_ordering:ident $failure_ordering:ident) => (
        $crate::insts::AtomicCmpXchg::new(
            $ptr,
            $cmp,
            $new,
            atomic_ordering!($success_ordering),
            atomic_ordering!($failure_ordering),
            false
        )
    )
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    macro_rules! test_atomic {
        ($builder:expr, atomic !( $op:ident $ptr:expr, $value:expr ; $ordering:ident ), $display:expr) => (
            assert_eq!(
                atomic!($op $ptr, $value ; $ordering)
                    .emit_to(& $builder)
                    .to_string()
                    .trim(),
                $display
            )
        )
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
            atomic!(xchg arg0_p_i64, i64_t.int(123); not_atomic),
            "%1 = atomicrmw xchg i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(add arg0_p_i64, i64_t.int(123); unordered),
            "%2 = atomicrmw add i64* %0, i64 123 unordered"
        );
        test_atomic!(
            builder,
            atomic!(sub arg0_p_i64, i64_t.int(123); monotonic),
            "%3 = atomicrmw sub i64* %0, i64 123 monotonic"
        );
        test_atomic!(
            builder,
            atomic!(and arg0_p_i64, i64_t.int(123); acquire),
            "%4 = atomicrmw and i64* %0, i64 123 acquire"
        );
        test_atomic!(
            builder,
            atomic!(nand arg0_p_i64, i64_t.int(123); release),
            "%5 = atomicrmw nand i64* %0, i64 123 release"
        );
        test_atomic!(
            builder,
            atomic!(or arg0_p_i64, i64_t.int(123); acq_rel),
            "%6 = atomicrmw or i64* %0, i64 123 acq_rel"
        );
        test_atomic!(
            builder,
            atomic!(xor arg0_p_i64, i64_t.int(123); seq_cst),
            "%7 = atomicrmw xor i64* %0, i64 123 seq_cst"
        );
        test_atomic!(
            builder,
            atomic!(max arg0_p_i64, i64_t.int(123); not_atomic),
            "%8 = atomicrmw max i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(min arg0_p_i64, i64_t.int(123); not_atomic),
            "%9 = atomicrmw min i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(umax arg0_p_i64, i64_t.int(123); not_atomic),
            "%10 = atomicrmw umax i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(umin arg0_p_i64, i64_t.int(123); not_atomic),
            "%11 = atomicrmw umin i64* %0, i64 123"
        );

        assert_eq!(
            fence!(acquire).emit_to(&builder).to_string().trim(),
            "%fence_acquire = fence acquire"
        );

        assert_eq!(
            fence!(singlethread acq_rel)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%fence_acq_rel = fence singlethread acq_rel"
        );

        assert_eq!(
            atomic!(cmpxchg arg0_p_i64, i64_t.int(123), i64_t.int(456); unordered unordered)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%12 = cmpxchg i64* %0, i64 123, i64 456 unordered unordered"
        );
    }
}
