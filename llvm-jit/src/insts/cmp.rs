use std::borrow::Cow;
use std::fmt;
use std::mem;

use llvm::{LLVMIntPredicate, LLVMRealPredicate};
use llvm::core::*;
use llvm::prelude::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::AsRaw;
use value::{Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct ICmp<'a, L, R> {
    op: u32, // TODO: use LLVMIntPredicate when llvm-sys update
    lhs: L,
    rhs: R,
    name: Cow<'a, str>,
}

impl<'a, L, R> ICmp<'a, L, R> {
    pub fn new(op: LLVMIntPredicate, lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        ICmp {
            op: op as u32,
            lhs,
            rhs,
            name,
        }
    }

    pub fn equals(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, name)
    }

    pub fn not_equals(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntNE, lhs, rhs, name)
    }

    pub fn unsigned_greater_than(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntUGT, lhs, rhs, name)
    }

    pub fn unsigned_greater_or_equal(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntUGE, lhs, rhs, name)
    }

    pub fn unsigned_less_than(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntULT, lhs, rhs, name)
    }

    pub fn unsigned_less_or_equal(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntULE, lhs, rhs, name)
    }

    pub fn signed_greater_than(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, name)
    }

    pub fn signed_greater_or_equal(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }

    pub fn signed_less_than(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, name)
    }

    pub fn signed_less_or_equal(lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }
}

impl<'a, L, R> InstructionBuilder for ICmp<'a, L, R>
where
    L: InstructionBuilder + fmt::Debug,
    R: InstructionBuilder + fmt::Debug,
{
    type Target = ICmpInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildICmp(
                builder.as_raw(),
                mem::transmute(self.op),
                self.lhs.emit_to(builder).into().as_raw(),
                self.rhs.emit_to(builder).into().as_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ICmpInst(Instruction);

inherit_from!(ICmpInst, Instruction, ValueRef, LLVMValueRef);

impl ICmpInst {
    /// Obtain the predicate of an instruction.
    ///
    /// This is only valid for instructions that correspond to llvm::ICmpInst
    /// or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
    pub fn predicate(&self) -> LLVMIntPredicate {
        unsafe { LLVMGetICmpPredicate(self.as_raw()) }
    }
}

/// The `icmp` instruction returns a boolean value or a vector of boolean values
/// based on comparison of its two integer, integer vector, pointer, or pointer vector operands.
///
/// The `icmp` compares `lhs` and `rhs` according to the condition code given as cond.
/// The comparison performed always yields either an `i1` or `vector` of `i1` result, as follows:
///
/// - eq: yields true if the operands are equal, false otherwise. No sign interpretation is necessary or performed.
/// - ne: yields true if the operands are unequal, false otherwise. No sign interpretation is necessary or performed.
/// - ugt: interprets the operands as unsigned values and yields true if `lhs` is greater than `rhs`.
/// - uge: interprets the operands as unsigned values and yields true if `lhs` is greater than or equal to `rhs`.
/// - ult: interprets the operands as unsigned values and yields true if `lhs` is less than `rhs`.
/// - ule: interprets the operands as unsigned values and yields true if `lhs` is less than or equal to `rhs`.
/// - sgt: interprets the operands as signed values and yields true if `lhs` is greater than `rhs`.
/// - sge: interprets the operands as signed values and yields true if `lhs` is greater than or equal to `rhs`.
/// - slt: interprets the operands as signed values and yields true if `lhs` is less than `rhs`.
/// - sle: interprets the operands as signed values and yields true if `lhs` is less than or equal to `rhs`.
#[macro_export]
macro_rules! icmp {
    (eq $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntEQ, $lhs, $rhs, $name.into())
    );
    (ne $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntNE, $lhs, $rhs, $name.into())
    );
    (ugt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntUGT, $lhs, $rhs, $name.into())
    );
    (uge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntUGE, $lhs, $rhs, $name.into())
    );
    (ult $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntULT, $lhs, $rhs, $name.into())
    );
    (ule $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntULE, $lhs, $rhs, $name.into())
    );
    (sgt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSGT, $lhs, $rhs, $name.into())
    );
    (sge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSGE, $lhs, $rhs, $name.into())
    );
    (slt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSLT, $lhs, $rhs, $name.into())
    );
    (sle $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSLE, $lhs, $rhs, $name.into())
    );
    ($op:ident $lhs:expr, $rhs:expr) => {
        icmp!($op $lhs, $rhs; stringify!($op))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FCmp<'a, L, R> {
    op: u32, // TODO: use LLVMRealPredicate when llvm-sys update
    lhs: L,
    rhs: R,
    name: Cow<'a, str>,
}

impl<'a, L, R> FCmp<'a, L, R> {
    pub fn new(op: LLVMRealPredicate, lhs: L, rhs: R, name: Cow<'a, str>) -> Self {
        FCmp {
            op: op as u32,
            lhs,
            rhs,
            name,
        }
    }
}

impl<'a, L, R> InstructionBuilder for FCmp<'a, L, R>
where
    L: InstructionBuilder + fmt::Debug,
    R: InstructionBuilder + fmt::Debug,
{
    type Target = FCmpInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildFCmp(
                builder.as_raw(),
                mem::transmute(self.op),
                self.lhs.emit_to(builder).into().as_raw(),
                self.rhs.emit_to(builder).into().as_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FCmpInst(Instruction);

inherit_from!(FCmpInst, Instruction, ValueRef, LLVMValueRef);

impl FCmpInst {
    /// Obtain the float predicate of an instruction.
    ///
    /// This is only valid for instructions that correspond to llvm::FCmpInst
    /// or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
    pub fn predicate(&self) -> LLVMRealPredicate {
        unsafe { LLVMGetFCmpPredicate(self.as_raw()) }
    }
}

/// The `fcmp` instruction returns a boolean value or vector of boolean values based on comparison of its operands.
///
/// The `fcmp` instruction compares `lhs` and `rhs` according to the condition code given as cond.
/// If the operands are vectors, then the vectors are compared element by element.
/// Each comparison performed always yields an i1 result, as follows:
///
///  - false: always yields false, regardless of operands.
///  - oeq: yields true if both operands are not a QNAN and `lhs` is equal to `rhs`.
///  - ogt: yields true if both operands are not a QNAN and `lhs` is greater than `rhs`.
///  - oge: yields true if both operands are not a QNAN and `lhs` is greater than or equal to `rhs`.
///  - olt: yields true if both operands are not a QNAN and `lhs` is less than `rhs`.
///  - ole: yields true if both operands are not a QNAN and `lhs` is less than or equal to `rhs`.
///  - one: yields true if both operands are not a QNAN and `lhs` is not equal to `rhs`.
///  - ord: yields true if both operands are not a QNAN.
///  - ueq: yields true if either operand is a QNAN or `lhs` is equal to `rhs`.
///  - ugt: yields true if either operand is a QNAN or `lhs` is greater than `rhs`.
///  - uge: yields true if either operand is a QNAN or `lhs` is greater than or equal to `rhs`.
///  - ult: yields true if either operand is a QNAN or `lhs` is less than `rhs`.
///  - ule: yields true if either operand is a QNAN or `lhs` is less than or equal to `rhs`.
///  - une: yields true if either operand is a QNAN or `lhs` is not equal to `rhs`.
///  - uno: yields true if either operand is a QNAN.
///  - true: always yields true, regardless of operands.
#[macro_export]
macro_rules! fcmp {
    (false $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealPredicateFalse, $lhs, $rhs, $name.into())
    );
    (oeq $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOEQ, $lhs, $rhs, $name.into())
    );
    (ogt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOGT, $lhs, $rhs, $name.into())
    );
    (oge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOGE, $lhs, $rhs, $name.into())
    );
    (olt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOLT, $lhs, $rhs, $name.into())
    );
    (ole $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOLE, $lhs, $rhs, $name.into())
    );
    (one $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealONE, $lhs, $rhs, $name.into())
    );
    (ord $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealORD, $lhs, $rhs, $name.into())
    );
    (uno $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUNO, $lhs, $rhs, $name.into())
    );
    (ueq $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUEQ, $lhs, $rhs, $name.into())
    );
    (ugt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUGT, $lhs, $rhs, $name.into())
    );
    (uge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUGE, $lhs, $rhs, $name.into())
    );
    (ult $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealULT, $lhs, $rhs, $name.into())
    );
    (ule $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealULE, $lhs, $rhs, $name.into())
    );
    (une $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUNE, $lhs, $rhs, $name.into())
    );
    (true $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealPredicateTrue, $lhs, $rhs, $name.into())
    );
    ($op:ident $lhs:expr, $rhs:expr) => {
        fcmp!($op $lhs, $rhs; stringify!($op))
    }
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    macro_rules! test_icmp {
        ($builder:expr, $pred:ident !( $lhs:expr, $rhs:expr)) => ({
            assert_eq!(
                icmp!($pred $lhs, $rhs; format!("icmp_{}", stringify!($pred)))
                    .emit_to(& $builder)
                    .to_string()
                    .trim(),
                format!("%icmp_{0} = icmp {0} i64 %0, %1", stringify!($pred))
            )
        })
    }

    macro_rules! test_fcmp {
        ($builder:expr, $pred:ident !( $lhs:expr, $rhs:expr)) => ({
            assert_eq!(
                fcmp!($pred $lhs, $rhs; format!("fcmp_{}", stringify!($pred)))
                    .emit_to(& $builder)
                    .to_string()
                    .trim(),
                format!("%fcmp_{0} = fcmp {0} double %0, %1", stringify!($pred))
            )
        })
    }

    #[test]
    fn icmp() {
        let context = Context::new();
        let module = context.create_module("icmp");
        let builder = context.create_builder();

        let i64_t = context.int64_t();

        let function_type = FunctionType::new(context.void_t(), &[i64_t, i64_t], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let lhs = function.get_param(0).unwrap();
        let rhs = function.get_param(1).unwrap();

        test_icmp!(builder, eq!(lhs, rhs));
        test_icmp!(builder, ne!(lhs, rhs));
        test_icmp!(builder, ugt!(lhs, rhs));
        test_icmp!(builder, uge!(lhs, rhs));
        test_icmp!(builder, ult!(lhs, rhs));
        test_icmp!(builder, ule!(lhs, rhs));
        test_icmp!(builder, sgt!(lhs, rhs));
        test_icmp!(builder, sge!(lhs, rhs));
        test_icmp!(builder, slt!(lhs, rhs));
        test_icmp!(builder, sle!(lhs, rhs));
    }

    #[test]
    fn fcmp() {
        let context = Context::new();
        let module = context.create_module("fcmp");
        let builder = context.create_builder();

        let f64_t = context.double_t();

        let function_type = FunctionType::new(context.void_t(), &[f64_t, f64_t], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let lhs = function.get_param(0).unwrap();
        let rhs = function.get_param(1).unwrap();

        test_fcmp!(builder, oeq!(lhs, rhs));
        test_fcmp!(builder, ogt!(lhs, rhs));
        test_fcmp!(builder, oge!(lhs, rhs));
        test_fcmp!(builder, olt!(lhs, rhs));
        test_fcmp!(builder, ole!(lhs, rhs));
        test_fcmp!(builder, one!(lhs, rhs));
        test_fcmp!(builder, ord!(lhs, rhs));
        test_fcmp!(builder, uno!(lhs, rhs));
        test_fcmp!(builder, ueq!(lhs, rhs));
        test_fcmp!(builder, ugt!(lhs, rhs));
        test_fcmp!(builder, uge!(lhs, rhs));
        test_fcmp!(builder, ult!(lhs, rhs));
        test_fcmp!(builder, ule!(lhs, rhs));
        test_fcmp!(builder, une!(lhs, rhs));
    }
}
