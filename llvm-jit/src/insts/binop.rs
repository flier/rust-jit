use std::borrow::Cow;

use crate::llvm::core::*;

use crate::insts::{AstNode, InstructionBuilder};
use crate::utils::{AsRaw, IntoRaw};

macro_rules! define_binary_operator {
    ($operator: ident, $func: path, $alias: ident, $comment: expr) => {
        #[doc=$comment]
        #[derive(Clone, Debug, PartialEq)]
        pub struct $operator<'a> {
            lhs: Box<AstNode<'a>>,
            rhs: Box<AstNode<'a>>,
            name: Cow<'a, str>,
        }

        impl<'a> $operator<'a> {
            pub fn new<LHS, RHS, N>(lhs: LHS, rhs: RHS, name: N) -> Self
            where
                LHS: Into<AstNode<'a>>,
                RHS: Into<AstNode<'a>>,
                N: Into<Cow<'a, str>>,
            {
                $operator {
                    lhs: Box::new(lhs.into()),
                    rhs: Box::new(rhs.into()),
                    name: name.into(),
                }
            }
        }

        impl<'a> $crate::insts::InstructionBuilder for $operator<'a> {
            type Target = $crate::Instruction;

            fn emit_to(self, builder: &$crate::insts::IRBuilder) -> Self::Target {
                trace!("{:?} emit instruction: {:?}", builder, self);

                unsafe {
                    $func(
                        builder.as_raw(),
                        self.lhs.emit_to(builder).into_raw(),
                        self.rhs.emit_to(builder).into_raw(),
                        cstr!(self.name),
                    )
                }
                .into()
            }
        }

        #[doc=$comment]
        pub fn $alias<'a, LHS, RHS, N>(lhs: LHS, rhs: RHS, name: N) -> $operator<'a>
        where
            LHS: Into<AstNode<'a>>,
            RHS: Into<AstNode<'a>>,
            N: Into<Cow<'a, str>>,
        {
            $crate::insts::$operator::new(lhs, rhs, name.into())
        }

        #[doc=$comment]
        #[macro_export]
        macro_rules! $alias {
            ($lhs: expr,$rhs: expr; $name: expr) => {
                $crate::insts::$alias($lhs, $rhs, $name)
            };
            ($lhs: expr,$rhs: expr) => {
                $crate::insts::$alias($lhs, $rhs, stringify!($alias))
            };
        }

        impl $crate::insts::IRBuilder {
            #[doc=$comment]
            pub fn $alias<'a, LHS, RHS, N>(&self, lhs: LHS, rhs: RHS, name: N) -> $crate::Instruction
            where
                LHS: Into<AstNode<'a>>,
                RHS: Into<AstNode<'a>>,
                N: Into<Cow<'a, str>>,
            {
                $crate::insts::$alias(lhs, rhs, name).emit_to(self)
            }
        }
    };
}

define_binary_operator!(
    Add,
    LLVMBuildAdd,
    add,
    "The `add` instruction returns the sum of its two operands."
);
define_binary_operator!(
    NSWAdd,
    LLVMBuildNSWAdd,
    add_nsw,
    "The `add` instruction returns the sum of its two operands, the result value of the `add` is a poison value if signed overflow occurs."
);
define_binary_operator!(
    NUWAdd,
    LLVMBuildNUWAdd,
    add_nuw,
    "The `add` instruction returns the sum of its two operands, the result value of the `add` is a poison value if unsigned overflow occurs."
);
define_binary_operator!(
    FAdd,
    LLVMBuildFAdd,
    fadd,
    "The `fadd` instruction returns the sum of its two operands."
);
define_binary_operator!(
    Sub,
    LLVMBuildSub,
    sub,
    "The `sub` instruction returns the difference of its two operands."
);
define_binary_operator!(
    NSWSub,
    LLVMBuildNSWSub,
    sub_nsw,
    "The `sub` instruction returns the difference of its two operands, the result value of the `sub` is a poison value if signed overflow occurs."
);
define_binary_operator!(
    NUWSub,
    LLVMBuildNUWSub,
    sub_nuw,
    "The `sub` instruction returns the difference of its two operands, the result value of the `sub` is a poison value if unsigned overflow occurs."
);
define_binary_operator!(
    FSub,
    LLVMBuildFSub,
    fsub,
    "The `fsub` instruction returns the difference of its two operands."
);
define_binary_operator!(
    Mul,
    LLVMBuildMul,
    mul,
    "The `mul` instruction returns the product of its two operands."
);
define_binary_operator!(
    NSWMul,
    LLVMBuildNSWMul,
    mul_nsw,
    "The `mul` instruction returns the product of its two operands, the result value of the `mul` is a poison value if signed overflow occurs."
);
define_binary_operator!(
    NUWMul,
    LLVMBuildNUWMul,
    mul_nuw,
    "The `mul` instruction returns the product of its two operands, the result value of the `mul` is a poison value if unsigned overflow occurs."
);
define_binary_operator!(
    FMul,
    LLVMBuildFMul,
    fmul,
    "The `fmul` instruction returns the product of its two operands."
);
define_binary_operator!(
    UDiv,
    LLVMBuildUDiv,
    udiv,
    "The `udiv` instruction returns the quotient of its two operands."
);
define_binary_operator!(
    ExactUDiv,
    LLVMBuildExactUDiv,
    udiv_exact,
    "The `udiv` instruction returns the quotient of its two operands, the result value of the `udiv` is a poison value if %`lhs` is not a multiple of %`rhs` (as such, “((a udiv exact b) mul b) == a”)."
);
define_binary_operator!(
    SDiv,
    LLVMBuildSDiv,
    sdiv,
    "The `sdiv` instruction returns the quotient of its two operands"
);
define_binary_operator!(
    ExactSDiv,
    LLVMBuildExactSDiv,
    sdiv_exact,
    "The `sdiv` instruction returns the quotient of its two operands, the result value of the `sdiv` is a poison value if the result would be rounded."
);
define_binary_operator!(
    FDiv,
    LLVMBuildFDiv,
    fdiv,
    "The `fdiv` instruction returns the quotient of its two operands."
);
define_binary_operator!(
    URem,
    LLVMBuildURem,
    urem,
    "The `urem` instruction returns the remainder from the unsigned division of its two arguments."
);
define_binary_operator!(
    SRem,
    LLVMBuildSRem,
    srem,
    "The `srem` instruction returns the remainder from the signed division of its two operands. "
);
define_binary_operator!(
    FRem,
    LLVMBuildFRem,
    frem,
    "The `frem` instruction returns the remainder from the division of its two operands.
"
);
define_binary_operator!(
    Shl,
    LLVMBuildShl,
    shl,
    "The `shl` instruction returns the first operand shifted to the left a specified number of bits."
);
define_binary_operator!(
    LShr,
    LLVMBuildLShr,
    lshr,
    "The `lshr` instruction (logical shift right) returns the first operand shifted to the right a specified number of bits with zero fill."
);
define_binary_operator!(
    AShr,
    LLVMBuildAShr,
    ashr,
    "The `ashr` instruction (arithmetic shift right) returns the first operand shifted to the right a specified number of bits with sign extension.
"
);
define_binary_operator!(
    And,
    LLVMBuildAnd,
    and,
    "The `and` instruction returns the bitwise logical and of its two operands."
);
define_binary_operator!(
    Or,
    LLVMBuildOr,
    or,
    "The `or` instruction returns the bitwise logical inclusive or of its two operands."
);
define_binary_operator!(
    Xor,
    LLVMBuildXor,
    xor,
    "The `xor` instruction returns the bitwise logical exclusive or of its two operands."
);
define_binary_operator!(
    PtrDiff,
    LLVMBuildPtrDiff,
    ptrdiff,
    "Return the i64 difference between two pointer values, dividing out the size of the pointed-to objects."
);

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

    macro_rules! test_bin_op {
        ($builder: ident, $name: ident($arg0_i64: ident, $arg1_i64: ident), $display: expr) => {
            assert_eq!(
                $name($arg0_i64, $arg1_i64, stringify!($name))
                    .emit_to(&$builder)
                    .to_string()
                    .trim(),
                $display
            )
        };
    }

    #[test]
    fn binops() {
        let c = Context::new();
        let m = c.create_module("binops");
        let b = c.create_builder();

        let i64_t = c.int64_t();
        let f64_t = c.double_t();

        let f_ty = FunctionType::new(c.void_t(), &[i64_t, i64_t, f64_t, f64_t], false);
        let f = m.add_function("test", f_ty);

        let bb = f.append_basic_block_in_context("entry", &c);
        b.position(Position::AtEnd(bb));

        let arg0_i64 = f.get_param(0).unwrap();
        let arg1_i64 = f.get_param(1).unwrap();
        let arg2_f64 = f.get_param(2).unwrap();
        let arg3_f64 = f.get_param(3).unwrap();

        test_bin_op!(b, add(arg0_i64, arg1_i64), "%add = add i64 %0, %1");
        test_bin_op!(b, add_nsw(arg0_i64, arg1_i64), "%add_nsw = add nsw i64 %0, %1");
        test_bin_op!(b, add_nuw(arg0_i64, arg1_i64), "%add_nuw = add nuw i64 %0, %1");
        test_bin_op!(b, fadd(arg2_f64, arg3_f64), "%fadd = fadd double %2, %3");

        test_bin_op!(b, sub(arg0_i64, arg1_i64), "%sub = sub i64 %0, %1");
        test_bin_op!(b, sub_nsw(arg0_i64, arg1_i64), "%sub_nsw = sub nsw i64 %0, %1");
        test_bin_op!(b, sub_nuw(arg0_i64, arg1_i64), "%sub_nuw = sub nuw i64 %0, %1");
        test_bin_op!(b, fsub(arg2_f64, arg3_f64), "%fsub = fsub double %2, %3");

        test_bin_op!(b, mul(arg0_i64, arg1_i64), "%mul = mul i64 %0, %1");
        test_bin_op!(b, mul_nsw(arg0_i64, arg1_i64), "%mul_nsw = mul nsw i64 %0, %1");
        test_bin_op!(b, mul_nuw(arg0_i64, arg1_i64), "%mul_nuw = mul nuw i64 %0, %1");
        test_bin_op!(b, fmul(arg2_f64, arg3_f64), "%fmul = fmul double %2, %3");

        test_bin_op!(b, udiv(arg0_i64, arg1_i64), "%udiv = udiv i64 %0, %1");
        test_bin_op!(b, udiv_exact(arg0_i64, arg1_i64), "%udiv_exact = udiv exact i64 %0, %1");
        test_bin_op!(b, sdiv(arg0_i64, arg1_i64), "%sdiv = sdiv i64 %0, %1");
        test_bin_op!(b, sdiv_exact(arg0_i64, arg1_i64), "%sdiv_exact = sdiv exact i64 %0, %1");
        test_bin_op!(b, fdiv(arg2_f64, arg3_f64), "%fdiv = fdiv double %2, %3");

        test_bin_op!(b, urem(arg0_i64, arg1_i64), "%urem = urem i64 %0, %1");
        test_bin_op!(b, srem(arg0_i64, arg1_i64), "%srem = srem i64 %0, %1");
        test_bin_op!(b, frem(arg2_f64, arg3_f64), "%frem = frem double %2, %3");

        test_bin_op!(b, shl(arg0_i64, arg1_i64), "%shl = shl i64 %0, %1");
        test_bin_op!(b, ashr(arg0_i64, arg1_i64), "%ashr = ashr i64 %0, %1");
        test_bin_op!(b, lshr(arg0_i64, arg1_i64), "%lshr = lshr i64 %0, %1");

        test_bin_op!(b, and(arg0_i64, arg1_i64), "%and = and i64 %0, %1");
        test_bin_op!(b, or(arg0_i64, arg1_i64), "%or = or i64 %0, %1");
        test_bin_op!(b, xor(arg0_i64, arg1_i64), "%xor = xor i64 %0, %1");
    }

    #[test]
    fn ptr_diff() {
        let c = Context::new();
        let m = c.create_module("ptr_diff");
        let b = c.create_builder();

        let i64_t = c.int64_t();
        let p_i64_t = i64_t.ptr_t();

        let f_ty = FunctionType::new(c.void_t(), types![p_i64_t, p_i64_t], false);
        let f = m.add_function("test", f_ty);

        let bb = f.append_basic_block_in_context("entry", &c);
        b.position(Position::AtEnd(bb));

        let arg0_p_i64 = f.get_param(0).unwrap();
        let arg1_p_i64 = f.get_param(1).unwrap();

        ptrdiff(arg0_p_i64, arg1_p_i64, "ptrdiff").emit_to(&b);

        assert_eq!(
            bb.last_instructions(4),
            vec![
                "%2 = ptrtoint i64* %0 to i64",
                "%3 = ptrtoint i64* %1 to i64",
                "%4 = sub i64 %2, %3",
                "%ptrdiff = sdiv exact i64 %4, ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)",
            ]
        );
    }
}
