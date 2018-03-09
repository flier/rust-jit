use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Sub};
use std::cmp::{Ordering, PartialEq, PartialOrd};

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use constant::{AsConstant, Constant, ConstantFP, ConstantFPs, ConstantInt, ConstantInts, ConstantVector, InlineAsm};
use types::TypeRef;
use utils::{AsLLVMBool, AsRaw};
use module::Module;
use function::FunctionType;

pub trait ConstantExpr {
    fn neg(&self) -> Constant;

    fn neg_nsw(&self) -> Constant;

    fn neg_nuw(&self) -> Constant;

    fn fneg(&self) -> Constant;

    fn not(&self) -> Constant;

    fn add(&self, other: Constant) -> Constant;

    fn add_nsw(&self, other: Constant) -> Constant;

    fn add_nuw(&self, other: Constant) -> Constant;

    fn fadd(&self, other: Constant) -> Constant;

    fn sub(&self, other: Constant) -> Constant;

    fn sub_nsw(&self, other: Constant) -> Constant;

    fn sub_nuw(&self, other: Constant) -> Constant;

    fn fsub(&self, other: Constant) -> Constant;

    fn mul(&self, other: Constant) -> Constant;

    fn mul_nsw(&self, other: Constant) -> Constant;

    fn mul_nuw(&self, other: Constant) -> Constant;

    fn fmul(&self, other: Constant) -> Constant;

    fn udiv(&self, other: Constant) -> Constant;

    fn udiv_exact(&self, other: Constant) -> Constant;

    fn sdiv(&self, other: Constant) -> Constant;

    fn sdiv_exact(&self, other: Constant) -> Constant;

    fn fdiv(&self, other: Constant) -> Constant;

    fn urem(&self, other: Constant) -> Constant;

    fn srem(&self, other: Constant) -> Constant;

    fn frem(&self, other: Constant) -> Constant;

    fn icmp(&self, predicate: LLVMIntPredicate, other: Constant) -> bool;

    fn fcmp(&self, predicate: LLVMRealPredicate, other: Constant) -> bool;

    fn and(&self, other: Constant) -> Constant;

    fn or(&self, other: Constant) -> Constant;

    fn xor(&self, other: Constant) -> Constant;

    fn shl(&self, other: Constant) -> Constant;

    fn lshr(&self, other: Constant) -> Constant;

    fn ashr(&self, other: Constant) -> Constant;

    fn gep(&self, indices: &[Constant]) -> Constant;

    fn inbounds_gep(&self, indices: &[Constant]) -> Constant;

    fn trunc(&self, ty: TypeRef) -> Constant;

    fn sext(&self, ty: TypeRef) -> Constant;

    fn zext(&self, ty: TypeRef) -> Constant;

    fn fptrunc(&self, ty: TypeRef) -> Constant;

    fn fpext(&self, ty: TypeRef) -> Constant;

    fn uitofp(&self, ty: TypeRef) -> Constant;

    fn sitofp(&self, ty: TypeRef) -> Constant;

    fn fptoui(&self, ty: TypeRef) -> Constant;

    fn fptosi(&self, ty: TypeRef) -> Constant;

    fn ptr_to_int(&self, ty: TypeRef) -> Constant;

    fn int_to_ptr(&self, ty: TypeRef) -> Constant;

    fn bit_cast(&self, ty: TypeRef) -> Constant;

    fn addrspace_cast(&self, ty: TypeRef) -> Constant;

    fn zext_or_bit_cast(&self, ty: TypeRef) -> Constant;

    fn sext_or_bit_cast(&self, ty: TypeRef) -> Constant;

    fn trunc_or_bit_cast(&self, ty: TypeRef) -> Constant;

    fn ptr_cast(&self, ty: TypeRef) -> Constant;

    fn int_cast(&self, ty: TypeRef, signed: bool) -> Constant;

    fn fp_cast(&self, ty: TypeRef) -> Constant;

    fn select(&self, then: Constant, or_else: Constant) -> Constant;

    fn extract_value(&self, indices: &mut [u32]) -> Constant;

    fn insert_value(&self, element: Constant, indices: &mut [u32]) -> Constant;
}

impl<T: AsConstant> ConstantExpr for T {
    fn neg(&self) -> Constant {
        unsafe { LLVMConstNeg(self.as_raw()) }.into()
    }

    fn neg_nsw(&self) -> Constant {
        unsafe { LLVMConstNSWNeg(self.as_raw()) }.into()
    }

    fn neg_nuw(&self) -> Constant {
        unsafe { LLVMConstNUWNeg(self.as_raw()) }.into()
    }

    fn fneg(&self) -> Constant {
        unsafe { LLVMConstFNeg(self.as_raw()) }.into()
    }

    fn not(&self) -> Constant {
        unsafe { LLVMConstNot(self.as_raw()) }.into()
    }

    fn add(&self, other: Constant) -> Constant {
        unsafe { LLVMConstAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn add_nsw(&self, other: Constant) -> Constant {
        unsafe { LLVMConstNSWAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn add_nuw(&self, other: Constant) -> Constant {
        unsafe { LLVMConstNUWAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn fadd(&self, other: Constant) -> Constant {
        unsafe { LLVMConstFAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn sub(&self, other: Constant) -> Constant {
        unsafe { LLVMConstSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn sub_nsw(&self, other: Constant) -> Constant {
        unsafe { LLVMConstNSWSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn sub_nuw(&self, other: Constant) -> Constant {
        unsafe { LLVMConstNUWSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn fsub(&self, other: Constant) -> Constant {
        unsafe { LLVMConstFSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn mul(&self, other: Constant) -> Constant {
        unsafe { LLVMConstMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn mul_nsw(&self, other: Constant) -> Constant {
        unsafe { LLVMConstNSWMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn mul_nuw(&self, other: Constant) -> Constant {
        unsafe { LLVMConstNUWMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn fmul(&self, other: Constant) -> Constant {
        unsafe { LLVMConstFMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn udiv(&self, other: Constant) -> Constant {
        unsafe { LLVMConstUDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn udiv_exact(&self, other: Constant) -> Constant {
        unsafe { LLVMConstExactUDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn sdiv(&self, other: Constant) -> Constant {
        unsafe { LLVMConstSDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn sdiv_exact(&self, other: Constant) -> Constant {
        unsafe { LLVMConstExactSDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn fdiv(&self, other: Constant) -> Constant {
        unsafe { LLVMConstFDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn urem(&self, other: Constant) -> Constant {
        unsafe { LLVMConstURem(self.as_raw(), other.as_raw()) }.into()
    }

    fn srem(&self, other: Constant) -> Constant {
        unsafe { LLVMConstSRem(self.as_raw(), other.as_raw()) }.into()
    }

    fn frem(&self, other: Constant) -> Constant {
        unsafe { LLVMConstFRem(self.as_raw(), other.as_raw()) }.into()
    }

    fn icmp(&self, predicate: LLVMIntPredicate, other: Constant) -> bool {
        ConstantInt::from(unsafe { LLVMConstICmp(predicate, self.as_raw(), other.as_raw()) }).bool()
    }

    fn fcmp(&self, predicate: LLVMRealPredicate, other: Constant) -> bool {
        ConstantInt::from(unsafe { LLVMConstFCmp(predicate, self.as_raw(), other.as_raw()) }).bool()
    }

    fn and(&self, other: Constant) -> Constant {
        unsafe { LLVMConstAnd(self.as_raw(), other.as_raw()) }.into()
    }

    fn or(&self, other: Constant) -> Constant {
        unsafe { LLVMConstOr(self.as_raw(), other.as_raw()) }.into()
    }

    fn xor(&self, other: Constant) -> Constant {
        unsafe { LLVMConstXor(self.as_raw(), other.as_raw()) }.into()
    }

    fn shl(&self, other: Constant) -> Constant {
        unsafe { LLVMConstShl(self.as_raw(), other.as_raw()) }.into()
    }

    fn ashr(&self, other: Constant) -> Constant {
        unsafe { LLVMConstAShr(self.as_raw(), other.as_raw()) }.into()
    }

    fn lshr(&self, other: Constant) -> Constant {
        unsafe { LLVMConstLShr(self.as_raw(), other.as_raw()) }.into()
    }

    fn gep(&self, indices: &[Constant]) -> Constant {
        let mut indices = indices
            .iter()
            .map(|c| c.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMConstGEP(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }.into()
    }

    fn inbounds_gep(&self, indices: &[Constant]) -> Constant {
        let mut indices = indices
            .iter()
            .map(|c| c.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMConstInBoundsGEP(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }.into()
    }

    fn trunc(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstTrunc(self.as_raw(), ty.as_raw()) }.into()
    }

    fn sext(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstSExt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn zext(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstZExt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fptrunc(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstFPTrunc(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fpext(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstFPExt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn uitofp(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstUIToFP(self.as_raw(), ty.as_raw()) }.into()
    }

    fn sitofp(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstSIToFP(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fptoui(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstFPToUI(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fptosi(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstFPToSI(self.as_raw(), ty.as_raw()) }.into()
    }

    fn ptr_to_int(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstPtrToInt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn int_to_ptr(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstIntToPtr(self.as_raw(), ty.as_raw()) }.into()
    }

    fn bit_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn addrspace_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstAddrSpaceCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn zext_or_bit_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstZExtOrBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn sext_or_bit_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstSExtOrBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn trunc_or_bit_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstTruncOrBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn ptr_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstPointerCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn int_cast(&self, ty: TypeRef, signed: bool) -> Constant {
        unsafe { LLVMConstIntCast(self.as_raw(), ty.as_raw(), signed.as_bool()) }.into()
    }

    fn fp_cast(&self, ty: TypeRef) -> Constant {
        unsafe { LLVMConstFPCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn select(&self, then: Constant, or_else: Constant) -> Constant {
        unsafe { LLVMConstSelect(self.as_raw(), then.as_raw(), or_else.as_raw()) }.into()
    }

    fn extract_value(&self, indices: &mut [u32]) -> Constant {
        unsafe { LLVMConstExtractValue(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }.into()
    }

    fn insert_value(&self, element: Constant, indices: &mut [u32]) -> Constant {
        unsafe {
            LLVMConstInsertValue(
                self.as_raw(),
                element.as_raw(),
                indices.as_mut_ptr(),
                indices.len() as u32,
            )
        }.into()
    }
}

impl Neg for ConstantInt {
    type Output = Self;

    fn neg(self) -> Self::Output {
        ConstantExpr::neg(&self).into()
    }
}

impl Not for ConstantInt {
    type Output = Self;

    fn not(self) -> Self::Output {
        ConstantExpr::not(&self).into()
    }
}

impl Add for ConstantInt {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        ConstantExpr::add(&self, rhs.into()).into()
    }
}

impl Sub for ConstantInt {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        ConstantExpr::sub(&self, rhs.into()).into()
    }
}

impl Mul for ConstantInt {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        ConstantExpr::mul(&self, rhs.into()).into()
    }
}

impl BitAnd for ConstantInt {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        ConstantExpr::and(&self, rhs.into()).into()
    }
}

impl BitOr for ConstantInt {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        ConstantExpr::or(&self, rhs.into()).into()
    }
}

impl BitXor for ConstantInt {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        ConstantExpr::xor(&self, rhs.into()).into()
    }
}

macro_rules! impl_const_int_operators {
    ($type:ty, $signed:expr) => {
        impl Add<$type> for ConstantInt {
            type Output = Self;

            fn add(self, rhs: $type) -> Self::Output {
                ConstantExpr::add(&self, self.type_of().int_value(rhs as u64, $signed).into()).into()
            }
        }

        impl Sub<$type> for ConstantInt {
            type Output = Self;

            fn sub(self, rhs: $type) -> Self::Output {
                ConstantExpr::sub(&self, self.type_of().int_value(rhs as u64, $signed).into()).into()
            }
        }

        impl Mul<$type> for ConstantInt {
            type Output = Self;

            fn mul(self, rhs: $type) -> Self::Output {
                ConstantExpr::mul(&self, self.type_of().int_value(rhs as u64, $signed).into()).into()
            }
        }

        impl Div<$type> for ConstantInt {
            type Output = Self;

            fn div(self, rhs: $type) -> Self::Output {
                let rhs = self.type_of().int_value(rhs as u64, $signed).into();

                if $signed {
                    ConstantExpr::sdiv(&self, rhs).into()
                } else {
                    ConstantExpr::udiv(&self, rhs).into()
                }
            }
        }

        impl Rem<$type> for ConstantInt {
            type Output = Self;

            fn rem(self, rhs: $type) -> Self::Output {
                let rhs = self.type_of().int_value(rhs as u64, $signed).into();

                if $signed {
                    ConstantExpr::srem(&self, rhs).into()
                } else {
                    ConstantExpr::urem(&self, rhs).into()
                }
            }
        }

        impl BitAnd<$type> for ConstantInt {
            type Output = Self;

            fn bitand(self, rhs: $type) -> Self::Output {
                ConstantExpr::and(&self, self.type_of().int_value(rhs as u64, $signed).into()).into()
            }
        }

        impl BitOr<$type> for ConstantInt {
            type Output = Self;

            fn bitor(self, rhs: $type) -> Self::Output {
                ConstantExpr::or(&self, self.type_of().int_value(rhs as u64, $signed).into()).into()
            }
        }

        impl BitXor<$type> for ConstantInt {
            type Output = Self;

            fn bitxor(self, rhs: $type) -> Self::Output {
                ConstantExpr::xor(&self, self.type_of().int_value(rhs as u64, $signed).into()).into()
            }
        }

        impl PartialEq<$type> for ConstantInt {
            fn eq(&self, other: & $type) -> bool {
                let rhs = self.type_of().int_value(*other as u64, $signed).into();

                ConstantExpr::icmp(self, LLVMIntPredicate::LLVMIntEQ, rhs)
            }
        }

        impl PartialOrd<$type> for ConstantInt {
            fn partial_cmp(&self, other: & $type) -> Option<Ordering> {
                let rhs = self.type_of().int_value(*other as u64, $signed).into();
                let lt = if $signed {
                    LLVMIntPredicate::LLVMIntSLT
                } else {
                    LLVMIntPredicate::LLVMIntULT
                };

                Some(if ConstantExpr::icmp(self, lt, rhs) {
                    Ordering::Less
                } else if ConstantExpr::icmp(self, LLVMIntPredicate::LLVMIntEQ, rhs) {
                    Ordering::Equal
                } else {
                    Ordering::Greater
                })
            }
        }
    };
}

impl_const_int_operators!(u8, false);
impl_const_int_operators!(u16, false);
impl_const_int_operators!(u32, false);
impl_const_int_operators!(u64, false);
impl_const_int_operators!(usize, false);
impl_const_int_operators!(i8, true);
impl_const_int_operators!(i16, true);
impl_const_int_operators!(i32, true);
impl_const_int_operators!(i64, true);
impl_const_int_operators!(isize, true);

impl Neg for ConstantFP {
    type Output = Self;

    fn neg(self) -> Self::Output {
        ConstantExpr::fneg(&self).into()
    }
}

impl Add for ConstantFP {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        ConstantExpr::fadd(&self, rhs.into()).into()
    }
}

impl Sub for ConstantFP {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        ConstantExpr::fsub(&self, rhs.into()).into()
    }
}

impl Mul for ConstantFP {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        ConstantExpr::fmul(&self, rhs.into()).into()
    }
}

impl Div for ConstantFP {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        ConstantExpr::fdiv(&self, rhs.into()).into()
    }
}

impl Rem for ConstantFP {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        ConstantExpr::frem(&self, rhs.into()).into()
    }
}

macro_rules! impl_const_floating_point_operators {
    ($type:ty) => {
        impl Add<$type> for ConstantFP {
            type Output = Self;

            fn add(self, rhs: $type) -> Self::Output {
                ConstantExpr::fadd(&self, self.type_of().real(rhs as f64).into()).into()
            }
        }

        impl Sub<$type> for ConstantFP {
            type Output = Self;

            fn sub(self, rhs: $type) -> Self::Output {
                ConstantExpr::fsub(&self, self.type_of().real(rhs as f64).into()).into()
            }
        }

        impl Mul<$type> for ConstantFP {
            type Output = Self;

            fn mul(self, rhs: $type) -> Self::Output {
                ConstantExpr::fmul(&self, self.type_of().real(rhs as f64).into()).into()
            }
        }

        impl Div<$type> for ConstantFP {
            type Output = Self;

            fn div(self, rhs: $type) -> Self::Output {
                ConstantExpr::fdiv(&self, self.type_of().real(rhs as f64).into()).into()
            }
        }

        impl Rem<$type> for ConstantFP {
            type Output = Self;

            fn rem(self, rhs: $type) -> Self::Output {
                ConstantExpr::frem(&self, self.type_of().real(rhs as f64).into()).into()
            }
        }

        impl PartialEq<$type> for ConstantFP {
            fn eq(&self, other: & $type) -> bool {
                let rhs = self.type_of().real(*other as f64).into();

                ConstantExpr::fcmp(self, LLVMRealPredicate::LLVMRealOEQ, rhs)
            }
        }

        impl PartialOrd<$type> for ConstantFP {
            fn partial_cmp(&self, other: & $type) -> Option<Ordering> {
                let rhs = self.type_of().real(*other as f64).into();

                Some(if ConstantExpr::fcmp(self, LLVMRealPredicate::LLVMRealOLT, rhs) {
                    Ordering::Less
                } else if ConstantExpr::fcmp(self, LLVMRealPredicate::LLVMRealOEQ, rhs) {
                    Ordering::Equal
                } else {
                    Ordering::Greater
                })
            }
        }
    };
}

impl_const_floating_point_operators!(f32);
impl_const_floating_point_operators!(f64);

impl ConstantVector {
    pub fn extract_element(&self, index: ConstantInt) -> Constant {
        unsafe { LLVMConstExtractElement(self.as_raw(), index.as_raw()) }.into()
    }

    pub fn insert_element(&self, element: Constant, index: ConstantInt) -> Constant {
        unsafe { LLVMConstInsertElement(self.as_raw(), element.as_raw(), index.as_raw()) }.into()
    }

    pub fn shuffle_vector(&self, other: ConstantVector, mask: ConstantVector) -> ConstantVector {
        unsafe { LLVMConstInsertElement(self.as_raw(), other.as_raw(), mask.as_raw()) }.into()
    }
}

impl Module {
    pub fn inline_asm<S: AsRef<str>>(&self, code: S) {
        unsafe { LLVMSetModuleInlineAsm(self.as_raw(), cstr!(code)) }
    }
}

impl FunctionType {
    pub fn inline_asm<S: AsRef<str>>(
        &self,
        code: S,
        constraints: S,
        side_effects: bool,
        align_stack: bool,
    ) -> InlineAsm {
        unsafe {
            LLVMConstInlineAsm(
                self.as_raw(),
                cstr!(code),
                cstr!(constraints),
                side_effects.as_bool(),
                align_stack.as_bool(),
            )
        }.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prelude::*;

    #[test]
    fn const_expr() {
        let c = Context::new();

        let bool_t = c.int1_t();
        let i64_t = c.int64_t();
        let f64_t = c.double_t();

        let b = bool_t.int(1);
        let i = i64_t.int(123);
        let f = f64_t.real(123.0);

        let bv = bool_t.vector_t(4).vector_of(values![
            bool_t.int(1),
            bool_t.int(0),
            bool_t.int(1),
            bool_t.int(0),
        ]);
        let iv = i64_t.vector_t(4).vector_of(values![
            i64_t.int(1),
            i64_t.int(2),
            i64_t.int(3),
            i64_t.int(4),
        ]);
        let iv2 = i64_t.vector_t(4).vector_of(values![
            i64_t.int(6),
            i64_t.int(7),
            i64_t.int(8),
            i64_t.int(9),
        ]);
        let fv = f64_t.vector_t(4).vector_of(values![
            f64_t.real(1.0),
            f64_t.real(2.0),
            f64_t.real(3.0),
            f64_t.real(4.0),
        ]);

        // neg
        assert_eq!(-i, i64_t.int(-123));
        assert_eq!(
            iv.neg().to_string(),
            "<4 x i64> <i64 -1, i64 -2, i64 -3, i64 -4>"
        );

        // fneg
        assert_eq!(-f, f64_t.real(-123.0));
        assert_eq!(
            fv.fneg().to_string(),
            "<4 x double> <double -1.000000e+00, double -2.000000e+00, double -3.000000e+00, double -4.000000e+00>"
        );

        // not
        assert_eq!(!b, bool_t.int(0));
        assert_eq!(
            bv.not().to_string(),
            "<4 x i1> <i1 false, i1 true, i1 false, i1 true>"
        );

        // add
        assert_eq!(i + 456, i64_t.int(123 + 456));
        assert_eq!(
            iv.add(iv.into()).to_string(),
            "<4 x i64> <i64 2, i64 4, i64 6, i64 8>"
        );

        // sub
        assert_eq!(i - 456, i64_t.int(123 - 456));
        assert_eq!(iv.sub(iv.into()).to_string(), "<4 x i64> zeroinitializer");

        // mul
        assert_eq!(i * 2, i64_t.int(123 * 2));
        assert_eq!(
            iv.mul(iv.into()).to_string(),
            "<4 x i64> <i64 1, i64 4, i64 9, i64 16>"
        );

        // div
        assert_eq!(i / 2, i64_t.int(123 / 2));
        assert_eq!(i / -2, i64_t.int(123 / -2));
        assert_eq!(
            iv.udiv(iv.into()).to_string(),
            "<4 x i64> <i64 1, i64 1, i64 1, i64 1>"
        );
        assert_eq!(
            iv.sdiv(iv.neg().into()).to_string(),
            "<4 x i64> <i64 -1, i64 -1, i64 -1, i64 -1>"
        );

        // rem
        assert_eq!(i % 2, i64_t.int(1));
        assert_eq!(i % -2, i64_t.int(1));
        assert_eq!(iv.urem(iv.into()).to_string(), "<4 x i64> zeroinitializer");
        assert_eq!(
            iv.srem(iv.neg().into()).to_string(),
            "<4 x i64> zeroinitializer"
        );

        // and
        assert_eq!(i & 456, i64_t.int(123 & 456));
        assert_eq!(
            iv.and(iv2.into()).to_string(),
            "<4 x i64> <i64 0, i64 2, i64 0, i64 0>"
        );

        // or
        assert_eq!(i | 456, i64_t.int(123 | 456));
        assert_eq!(
            iv.or(iv2.into()).to_string(),
            "<4 x i64> <i64 7, i64 7, i64 11, i64 13>"
        );

        // xor
        assert_eq!(i ^ 456, i64_t.int(123 ^ 456));
        assert_eq!(
            iv.xor(iv2.into()).to_string(),
            "<4 x i64> <i64 7, i64 5, i64 11, i64 13>"
        );

        // fadd
        assert_eq!(f + 456.0, f64_t.real(123.0 + 456.0));
        assert_eq!(
            fv.fadd(fv.into()).to_string(),
            "<4 x double> <double 2.000000e+00, double 4.000000e+00, double 6.000000e+00, double 8.000000e+00>"
        );

        // fsub
        assert_eq!(f - 456.0, f64_t.real(123.0 - 456.0));
        assert_eq!(
            fv.fsub(fv.into()).to_string(),
            "<4 x double> zeroinitializer"
        );

        // fmul
        assert_eq!(f * 2.0, f64_t.real(123.0 * 2.0));
        assert_eq!(
            fv.fmul(fv.into()).to_string(),
            "<4 x double> <double 1.000000e+00, double 4.000000e+00, double 9.000000e+00, double 1.600000e+01>"
        );

        // fdiv
        assert_eq!(f / 2.0, f64_t.real(123.0 / 2.0));
        assert_eq!(
            fv.fdiv(fv.into()).to_string(),
            "<4 x double> <double 1.000000e+00, double 1.000000e+00, double 1.000000e+00, double 1.000000e+00>"
        );

        // frem
        assert_eq!(f % 2.0, f64_t.real(123.0 % 2.0));
        assert_eq!(
            fv.frem(fv.into()).to_string(),
            "<4 x double> zeroinitializer"
        );

        // eq
        assert_eq!(i, 123);
        assert!(iv.eq(&iv.into()));

        assert_eq!(f, 123.0);
        assert!(fv.eq(&fv.into()));

        // ord
        assert!(i < 456);
        assert!(f < 456.0);
    }
}
