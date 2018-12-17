use std::borrow::Cow;

use crate::llvm::{LLVMIntPredicate, LLVMRealPredicate};
use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::utils::{AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct ICmp<'a> {
    op: LLVMIntPredicate,
    lhs: Box<AstNode<'a>>,
    rhs: Box<AstNode<'a>>,
    name: Cow<'a, str>,
}

impl<'a> ICmp<'a> {
    pub fn new<L, R, N>(op: LLVMIntPredicate, lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        ICmp {
            op,
            lhs: Box::new(lhs.into()),
            rhs: Box::new(rhs.into()),
            name: name.into(),
        }
    }

    pub fn equals<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, name)
    }

    pub fn not_equals<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntNE, lhs, rhs, name)
    }

    pub fn unsigned_greater_than<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntUGT, lhs, rhs, name)
    }

    pub fn unsigned_greater_or_equal<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntUGE, lhs, rhs, name)
    }

    pub fn unsigned_less_than<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntULT, lhs, rhs, name)
    }

    pub fn unsigned_less_or_equal<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntULE, lhs, rhs, name)
    }

    pub fn signed_greater_than<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, name)
    }

    pub fn signed_greater_or_equal<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }

    pub fn signed_less_than<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, name)
    }

    pub fn signed_less_or_equal<L, R, N>(lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Self::new(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }
}

impl<'a> InstructionBuilder for ICmp<'a> {
    type Target = ICmpInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildICmp(
                builder.as_raw(),
                self.op,
                self.lhs.emit_to(builder).into_raw(),
                self.rhs.emit_to(builder).into_raw(),
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
    (EQ $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntEQ, $lhs, $rhs, $name)
    };
    (NE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntNE, $lhs, $rhs, $name)
    };
    (UGT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntUGT, $lhs, $rhs, $name)
    };
    (UGE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntUGE, $lhs, $rhs, $name)
    };
    (ULT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntULT, $lhs, $rhs, $name)
    };
    (ULE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntULE, $lhs, $rhs, $name)
    };
    (SGT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSGT, $lhs, $rhs, $name)
    };
    (SGE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSGE, $lhs, $rhs, $name)
    };
    (SLT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSLT, $lhs, $rhs, $name)
    };
    (SLE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::ICmp::new($crate::llvm::LLVMIntPredicate::LLVMIntSLE, $lhs, $rhs, $name)
    };
    ($op: ident $lhs: expr, $rhs: expr) => {
        icmp!($op $lhs, $rhs; stringify!($op).to_lowercase())
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct FCmp<'a> {
    op: LLVMRealPredicate,
    lhs: Box<AstNode<'a>>,
    rhs: Box<AstNode<'a>>,
    name: Cow<'a, str>,
}

impl<'a> FCmp<'a> {
    pub fn new<L, R, N>(op: LLVMRealPredicate, lhs: L, rhs: R, name: N) -> Self
    where
        L: Into<AstNode<'a>>,
        R: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        FCmp {
            op,
            lhs: Box::new(lhs.into()),
            rhs: Box::new(rhs.into()),
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for FCmp<'a> {
    type Target = FCmpInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildFCmp(
                builder.as_raw(),
                self.op,
                self.lhs.emit_to(builder).into_raw(),
                self.rhs.emit_to(builder).into_raw(),
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
    (false $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new(
            $crate::llvm::LLVMRealPredicate::LLVMRealPredicateFalse,
            $lhs,
            $rhs,
            $name,
        )
    };
    (OEQ $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOEQ, $lhs, $rhs, $name)
    };
    (OGT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOGT, $lhs, $rhs, $name)
    };
    (OGE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOGE, $lhs, $rhs, $name)
    };
    (OLT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOLT, $lhs, $rhs, $name)
    };
    (OLE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealOLE, $lhs, $rhs, $name)
    };
    (ONE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealONE, $lhs, $rhs, $name)
    };
    (ORD $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealORD, $lhs, $rhs, $name)
    };
    (UNO $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUNO, $lhs, $rhs, $name)
    };
    (UEQ $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUEQ, $lhs, $rhs, $name)
    };
    (UGT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUGT, $lhs, $rhs, $name)
    };
    (UGE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUGE, $lhs, $rhs, $name)
    };
    (ULT $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealULT, $lhs, $rhs, $name)
    };
    (ULE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealULE, $lhs, $rhs, $name)
    };
    (UNE $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new($crate::llvm::LLVMRealPredicate::LLVMRealUNE, $lhs, $rhs, $name)
    };
    (true $lhs: expr, $rhs: expr; $name: expr) => {
        $crate::insts::FCmp::new(
            $crate::llvm::LLVMRealPredicate::LLVMRealPredicateTrue,
            $lhs,
            $rhs,
            $name,
        )
    };
    ($op: ident $lhs: expr, $rhs: expr) => {
        fcmp!($op $lhs, $rhs; stringify!($op).to_lowercase())
    };
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

    macro_rules! test_icmp {
        ($builder: expr, $pred: ident($lhs: expr, $rhs: expr)) => {{
            assert_eq!(
                icmp!($pred $lhs, $rhs; format!("icmp_{}", stringify!($pred).to_lowercase()))
                    .emit_to(&$builder)
                    .to_string()
                    .trim(),
                format!("%icmp_{0} = icmp {0} i64 %0, %1", stringify!($pred).to_lowercase())
            )
        }};
    }

    macro_rules! test_fcmp {
        ($builder: expr, $pred: ident($lhs: expr, $rhs: expr)) => {{
            assert_eq!(
                fcmp!($pred $lhs, $rhs; format!("fcmp_{}", stringify!($pred).to_lowercase()))
                    .emit_to(&$builder)
                    .to_string()
                    .trim(),
                format!("%fcmp_{0} = fcmp {0} double %0, %1", stringify!($pred).to_lowercase())
            )
        }};
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

        test_icmp!(builder, EQ(lhs, rhs));
        test_icmp!(builder, NE(lhs, rhs));
        test_icmp!(builder, UGT(lhs, rhs));
        test_icmp!(builder, UGE(lhs, rhs));
        test_icmp!(builder, ULT(lhs, rhs));
        test_icmp!(builder, ULE(lhs, rhs));
        test_icmp!(builder, SGT(lhs, rhs));
        test_icmp!(builder, SGE(lhs, rhs));
        test_icmp!(builder, SLT(lhs, rhs));
        test_icmp!(builder, SLE(lhs, rhs));
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

        test_fcmp!(builder, OEQ(lhs, rhs));
        test_fcmp!(builder, OGT(lhs, rhs));
        test_fcmp!(builder, OGE(lhs, rhs));
        test_fcmp!(builder, OLT(lhs, rhs));
        test_fcmp!(builder, OLE(lhs, rhs));
        test_fcmp!(builder, ONE(lhs, rhs));
        test_fcmp!(builder, ORD(lhs, rhs));
        test_fcmp!(builder, UNO(lhs, rhs));
        test_fcmp!(builder, UEQ(lhs, rhs));
        test_fcmp!(builder, UGT(lhs, rhs));
        test_fcmp!(builder, UGE(lhs, rhs));
        test_fcmp!(builder, ULT(lhs, rhs));
        test_fcmp!(builder, ULE(lhs, rhs));
        test_fcmp!(builder, UNE(lhs, rhs));
    }
}
