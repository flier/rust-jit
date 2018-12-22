#![cfg_attr(feature = "cargo-clippy", allow(clippy::cast_lossless))]

use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Sub};

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::*;

use crate::constant::{AsConstant, Constant, ConstantFP, ConstantFPs, ConstantInt, ConstantInts, ConstantVector};
use crate::types::TypeRef;
use crate::utils::{AsLLVMBool, AsRaw};

pub trait ConstantExpr {
    fn neg(&self) -> Constant;

    fn neg_nsw(&self) -> Constant;

    fn neg_nuw(&self) -> Constant;

    fn fneg(&self) -> Constant;

    fn not(&self) -> Constant;

    fn add<T: AsConstant>(&self, other: T) -> Constant;

    fn add_nsw<T: AsConstant>(&self, other: T) -> Constant;

    fn add_nuw<T: AsConstant>(&self, other: T) -> Constant;

    fn fadd<T: AsConstant>(&self, other: T) -> Constant;

    fn sub<T: AsConstant>(&self, other: T) -> Constant;

    fn sub_nsw<T: AsConstant>(&self, other: T) -> Constant;

    fn sub_nuw<T: AsConstant>(&self, other: T) -> Constant;

    fn fsub<T: AsConstant>(&self, other: T) -> Constant;

    fn mul<T: AsConstant>(&self, other: T) -> Constant;

    fn mul_nsw<T: AsConstant>(&self, other: T) -> Constant;

    fn mul_nuw<T: AsConstant>(&self, other: T) -> Constant;

    fn fmul<T: AsConstant>(&self, other: T) -> Constant;

    fn udiv<T: AsConstant>(&self, other: T) -> Constant;

    fn udiv_exact<T: AsConstant>(&self, other: T) -> Constant;

    fn sdiv<T: AsConstant>(&self, other: T) -> Constant;

    fn sdiv_exact<T: AsConstant>(&self, other: T) -> Constant;

    fn fdiv<T: AsConstant>(&self, other: T) -> Constant;

    fn urem<T: AsConstant>(&self, other: T) -> Constant;

    fn srem<T: AsConstant>(&self, other: T) -> Constant;

    fn frem<T: AsConstant>(&self, other: T) -> Constant;

    fn icmp<T: AsConstant>(&self, predicate: LLVMIntPredicate, other: T) -> bool;

    fn fcmp<T: AsConstant>(&self, predicate: LLVMRealPredicate, other: T) -> bool;

    fn and<T: AsConstant>(&self, other: T) -> Constant;

    fn or<T: AsConstant>(&self, other: T) -> Constant;

    fn xor<T: AsConstant>(&self, other: T) -> Constant;

    fn shl<T: AsConstant>(&self, other: T) -> Constant;

    fn lshr<T: AsConstant>(&self, other: T) -> Constant;

    fn ashr<T: AsConstant>(&self, other: T) -> Constant;

    fn gep(&self, indices: &[Constant]) -> Constant;

    fn inbounds_gep(&self, indices: &[Constant]) -> Constant;

    fn trunc<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn sext<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn zext<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn fptrunc<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn fpext<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn uitofp<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn sitofp<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn fptoui<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn fptosi<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn ptr_to_int<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn int_to_ptr<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn addrspace_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn zext_or_bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn sext_or_bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn trunc_or_bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn ptr_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn int_cast(&self, ty: TypeRef, signed: bool) -> Constant;

    fn fp_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant;

    fn select(&self, then: Constant, or_else: Constant) -> Constant;

    fn extract_value(&self, indices: &mut [u32]) -> Constant;

    fn insert_value(&self, element: Constant, indices: &mut [u32]) -> Constant;
}

impl<C: AsConstant> ConstantExpr for C {
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

    fn add<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn add_nsw<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstNSWAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn add_nuw<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstNUWAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn fadd<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstFAdd(self.as_raw(), other.as_raw()) }.into()
    }

    fn sub<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn sub_nsw<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstNSWSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn sub_nuw<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstNUWSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn fsub<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstFSub(self.as_raw(), other.as_raw()) }.into()
    }

    fn mul<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn mul_nsw<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstNSWMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn mul_nuw<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstNUWMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn fmul<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstFMul(self.as_raw(), other.as_raw()) }.into()
    }

    fn udiv<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstUDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn udiv_exact<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstExactUDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn sdiv<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstSDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn sdiv_exact<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstExactSDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn fdiv<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstFDiv(self.as_raw(), other.as_raw()) }.into()
    }

    fn urem<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstURem(self.as_raw(), other.as_raw()) }.into()
    }

    fn srem<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstSRem(self.as_raw(), other.as_raw()) }.into()
    }

    fn frem<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstFRem(self.as_raw(), other.as_raw()) }.into()
    }

    fn icmp<T: AsConstant>(&self, predicate: LLVMIntPredicate, other: T) -> bool {
        ConstantInt::from(unsafe { LLVMConstICmp(predicate, self.as_raw(), other.as_raw()) }).bool()
    }

    fn fcmp<T: AsConstant>(&self, predicate: LLVMRealPredicate, other: T) -> bool {
        ConstantInt::from(unsafe { LLVMConstFCmp(predicate, self.as_raw(), other.as_raw()) }).bool()
    }

    fn and<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstAnd(self.as_raw(), other.as_raw()) }.into()
    }

    fn or<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstOr(self.as_raw(), other.as_raw()) }.into()
    }

    fn xor<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstXor(self.as_raw(), other.as_raw()) }.into()
    }

    fn shl<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstShl(self.as_raw(), other.as_raw()) }.into()
    }

    fn ashr<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstAShr(self.as_raw(), other.as_raw()) }.into()
    }

    fn lshr<T: AsConstant>(&self, other: T) -> Constant {
        unsafe { LLVMConstLShr(self.as_raw(), other.as_raw()) }.into()
    }

    fn gep(&self, indices: &[Constant]) -> Constant {
        let mut indices = indices.iter().map(|c| c.as_raw()).collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMConstGEP(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }.into()
    }

    fn inbounds_gep(&self, indices: &[Constant]) -> Constant {
        let mut indices = indices.iter().map(|c| c.as_raw()).collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMConstInBoundsGEP(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }.into()
    }

    fn trunc<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstTrunc(self.as_raw(), ty.as_raw()) }.into()
    }

    fn sext<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstSExt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn zext<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstZExt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fptrunc<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstFPTrunc(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fpext<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstFPExt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn uitofp<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstUIToFP(self.as_raw(), ty.as_raw()) }.into()
    }

    fn sitofp<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstSIToFP(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fptoui<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstFPToUI(self.as_raw(), ty.as_raw()) }.into()
    }

    fn fptosi<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstFPToSI(self.as_raw(), ty.as_raw()) }.into()
    }

    fn ptr_to_int<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstPtrToInt(self.as_raw(), ty.as_raw()) }.into()
    }

    fn int_to_ptr<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstIntToPtr(self.as_raw(), ty.as_raw()) }.into()
    }

    fn bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn addrspace_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstAddrSpaceCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn zext_or_bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstZExtOrBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn sext_or_bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstSExtOrBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn trunc_or_bit_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstTruncOrBitCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn ptr_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
        unsafe { LLVMConstPointerCast(self.as_raw(), ty.as_raw()) }.into()
    }

    fn int_cast(&self, ty: TypeRef, signed: bool) -> Constant {
        unsafe { LLVMConstIntCast(self.as_raw(), ty.as_raw(), signed.as_bool()) }.into()
    }

    fn fp_cast<T: AsRaw<RawType = LLVMTypeRef>>(&self, ty: T) -> Constant {
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
        }
        .into()
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
        ConstantExpr::add(&self, rhs).into()
    }
}

impl Sub for ConstantInt {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        ConstantExpr::sub(&self, rhs).into()
    }
}

impl Mul for ConstantInt {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        ConstantExpr::mul(&self, rhs).into()
    }
}

impl BitAnd for ConstantInt {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        ConstantExpr::and(&self, rhs).into()
    }
}

impl BitOr for ConstantInt {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        ConstantExpr::or(&self, rhs).into()
    }
}

impl BitXor for ConstantInt {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        ConstantExpr::xor(&self, rhs).into()
    }
}

macro_rules! impl_const_int_operators {
    ($type: ty, $signed: expr) => {
        impl Add<$type> for ConstantInt {
            type Output = Self;

            fn add(self, rhs: $type) -> Self::Output {
                ConstantExpr::add(&self, self.type_of().int_value(rhs as u64, $signed)).into()
            }
        }

        impl Sub<$type> for ConstantInt {
            type Output = Self;

            fn sub(self, rhs: $type) -> Self::Output {
                ConstantExpr::sub(&self, self.type_of().int_value(rhs as u64, $signed)).into()
            }
        }

        impl Mul<$type> for ConstantInt {
            type Output = Self;

            fn mul(self, rhs: $type) -> Self::Output {
                ConstantExpr::mul(&self, self.type_of().int_value(rhs as u64, $signed)).into()
            }
        }

        impl Div<$type> for ConstantInt {
            type Output = Self;

            fn div(self, rhs: $type) -> Self::Output {
                let rhs = self.type_of().int_value(rhs as u64, $signed);

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
                let rhs = self.type_of().int_value(rhs as u64, $signed);

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
                ConstantExpr::and(&self, self.type_of().int_value(rhs as u64, $signed)).into()
            }
        }

        impl BitOr<$type> for ConstantInt {
            type Output = Self;

            fn bitor(self, rhs: $type) -> Self::Output {
                ConstantExpr::or(&self, self.type_of().int_value(rhs as u64, $signed)).into()
            }
        }

        impl BitXor<$type> for ConstantInt {
            type Output = Self;

            fn bitxor(self, rhs: $type) -> Self::Output {
                ConstantExpr::xor(&self, self.type_of().int_value(rhs as u64, $signed)).into()
            }
        }

        impl PartialEq<$type> for ConstantInt {
            fn eq(&self, other: &$type) -> bool {
                let rhs = self.type_of().int_value(*other as u64, $signed);

                ConstantExpr::icmp(self, LLVMIntPredicate::LLVMIntEQ, rhs)
            }
        }

        impl PartialOrd<$type> for ConstantInt {
            fn partial_cmp(&self, other: &$type) -> Option<Ordering> {
                let rhs = self.type_of().int_value(*other as u64, $signed);
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
        ConstantExpr::fadd(&self, rhs).into()
    }
}

impl Sub for ConstantFP {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        ConstantExpr::fsub(&self, rhs).into()
    }
}

impl Mul for ConstantFP {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        ConstantExpr::fmul(&self, rhs).into()
    }
}

impl Div for ConstantFP {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        ConstantExpr::fdiv(&self, rhs).into()
    }
}

impl Rem for ConstantFP {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        ConstantExpr::frem(&self, rhs).into()
    }
}

macro_rules! impl_const_floating_point_operators {
    ($type: ty) => {
        impl Add<$type> for ConstantFP {
            type Output = Self;

            fn add(self, rhs: $type) -> Self::Output {
                ConstantExpr::fadd(&self, self.type_of().real(f64::from(rhs))).into()
            }
        }

        impl Sub<$type> for ConstantFP {
            type Output = Self;

            fn sub(self, rhs: $type) -> Self::Output {
                ConstantExpr::fsub(&self, self.type_of().real(f64::from(rhs))).into()
            }
        }

        impl Mul<$type> for ConstantFP {
            type Output = Self;

            fn mul(self, rhs: $type) -> Self::Output {
                ConstantExpr::fmul(&self, self.type_of().real(f64::from(rhs))).into()
            }
        }

        impl Div<$type> for ConstantFP {
            type Output = Self;

            fn div(self, rhs: $type) -> Self::Output {
                ConstantExpr::fdiv(&self, self.type_of().real(f64::from(rhs))).into()
            }
        }

        impl Rem<$type> for ConstantFP {
            type Output = Self;

            fn rem(self, rhs: $type) -> Self::Output {
                ConstantExpr::frem(&self, self.type_of().real(f64::from(rhs))).into()
            }
        }

        impl PartialEq<$type> for ConstantFP {
            fn eq(&self, other: &$type) -> bool {
                let rhs = self.type_of().real(f64::from(*other));

                ConstantExpr::fcmp(self, LLVMRealPredicate::LLVMRealOEQ, rhs)
            }
        }

        impl PartialOrd<$type> for ConstantFP {
            fn partial_cmp(&self, other: &$type) -> Option<Ordering> {
                let rhs = self.type_of().real(f64::from(*other));

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;

    #[test]
    fn const_expr() {
        let c = Context::new();

        let bool_t = c.int1_t();
        let i64_t = c.int64_t();
        let f64_t = c.double_t();

        let b = bool_t.int(1);
        let i = i64_t.int(123);
        let f = f64_t.real(123.0);

        let bv = bool_t
            .vector_t(4)
            .vector_of(values![bool_t.int(1), bool_t.int(0), bool_t.int(1), bool_t.int(0),]);
        let iv = i64_t
            .vector_t(4)
            .vector_of(values![i64_t.int(1), i64_t.int(2), i64_t.int(3), i64_t.int(4),]);
        let iv2 = i64_t
            .vector_t(4)
            .vector_of(values![i64_t.int(6), i64_t.int(7), i64_t.int(8), i64_t.int(9),]);
        let fv = f64_t.vector_t(4).vector_of(values![
            f64_t.real(1.0),
            f64_t.real(2.0),
            f64_t.real(3.0),
            f64_t.real(4.0),
        ]);

        // neg
        assert_eq!(-i, i64_t.int(-123));
        assert_eq!(iv.neg().to_string(), "<4 x i64> <i64 -1, i64 -2, i64 -3, i64 -4>");

        // fneg
        assert_eq!(-f, f64_t.real(-123.0));
        assert_eq!(
            fv.fneg().to_string(),
            "<4 x double> <double -1.000000e+00, double -2.000000e+00, double -3.000000e+00, double -4.000000e+00>"
        );

        // not
        assert_eq!(!b, bool_t.int(0));
        assert_eq!(bv.not().to_string(), "<4 x i1> <i1 false, i1 true, i1 false, i1 true>");

        // add
        assert_eq!(i + 456, i64_t.int(123 + 456));
        assert_eq!(iv.add(iv).to_string(), "<4 x i64> <i64 2, i64 4, i64 6, i64 8>");

        // sub
        assert_eq!(i - 456, i64_t.int(123 - 456));
        assert_eq!(iv.sub(iv).to_string(), "<4 x i64> zeroinitializer");

        // mul
        assert_eq!(i * 2, i64_t.int(123 * 2));
        assert_eq!(iv.mul(iv).to_string(), "<4 x i64> <i64 1, i64 4, i64 9, i64 16>");

        // div
        assert_eq!(i / 2, i64_t.int(123 / 2));
        assert_eq!(i / -2, i64_t.int(123 / -2));
        assert_eq!(iv.udiv(iv).to_string(), "<4 x i64> <i64 1, i64 1, i64 1, i64 1>");
        assert_eq!(
            iv.sdiv(iv.neg()).to_string(),
            "<4 x i64> <i64 -1, i64 -1, i64 -1, i64 -1>"
        );

        // rem
        assert_eq!(i % 2, i64_t.int(1));
        assert_eq!(i % -2, i64_t.int(1));
        assert_eq!(iv.urem(iv).to_string(), "<4 x i64> zeroinitializer");
        assert_eq!(iv.srem(iv.neg()).to_string(), "<4 x i64> zeroinitializer");

        // and
        assert_eq!(i & 456, i64_t.int(123 & 456));
        assert_eq!(iv.and(iv2).to_string(), "<4 x i64> <i64 0, i64 2, i64 0, i64 0>");

        // or
        assert_eq!(i | 456, i64_t.int(123 | 456));
        assert_eq!(iv.or(iv2).to_string(), "<4 x i64> <i64 7, i64 7, i64 11, i64 13>");

        // xor
        assert_eq!(i ^ 456, i64_t.int(123 ^ 456));
        assert_eq!(iv.xor(iv2).to_string(), "<4 x i64> <i64 7, i64 5, i64 11, i64 13>");

        // fadd
        assert_eq!(f + 456.0, f64_t.real(123.0 + 456.0));
        assert_eq!(
            fv.fadd(fv).to_string(),
            "<4 x double> <double 2.000000e+00, double 4.000000e+00, double 6.000000e+00, double 8.000000e+00>"
        );

        // fsub
        assert_eq!(f - 456.0, f64_t.real(123.0 - 456.0));
        assert_eq!(fv.fsub(fv).to_string(), "<4 x double> zeroinitializer");

        // fmul
        assert_eq!(f * 2.0, f64_t.real(123.0 * 2.0));
        assert_eq!(
            fv.fmul(fv).to_string(),
            "<4 x double> <double 1.000000e+00, double 4.000000e+00, double 9.000000e+00, double 1.600000e+01>"
        );

        // fdiv
        assert_eq!(f / 2.0, f64_t.real(123.0 / 2.0));
        assert_eq!(
            fv.fdiv(fv).to_string(),
            "<4 x double> <double 1.000000e+00, double 1.000000e+00, double 1.000000e+00, double 1.000000e+00>"
        );

        // frem
        assert_eq!(f % 2.0, f64_t.real(123.0 % 2.0));
        assert_eq!(fv.frem(fv).to_string(), "<4 x double> zeroinitializer");

        // eq
        assert_eq!(i, 123);
        assert!(iv.eq(&iv));

        assert_eq!(f, 123.0);
        assert!(fv.eq(&fv));

        // ord
        assert!(i < 456);
        assert!(f < 456.0);
    }
}
