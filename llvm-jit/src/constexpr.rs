use llvm::*;
use llvm::core::*;
use llvm::prelude::*;

use constant::{Constant, ConstantInt, ConstantVector};
use types::TypeRef;
use utils::{AsLLVMBool, unchecked_cstring};
use value::{AsValueRef, ValueRef};

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

    fn icmp(&self, predicate: LLVMIntPredicate, other: Constant) -> Constant;

    fn fcmp(&self, predicate: LLVMRealPredicate, other: Constant) -> Constant;

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

impl ConstantExpr for Constant {
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

    fn icmp(&self, predicate: LLVMIntPredicate, other: Constant) -> Constant {
        unsafe { LLVMConstICmp(predicate, self.as_raw(), other.as_raw()) }.into()
    }

    fn fcmp(&self, predicate: LLVMRealPredicate, other: Constant) -> Constant {
        unsafe { LLVMConstFCmp(predicate, self.as_raw(), other.as_raw()) }.into()
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

        unsafe { LLVMConstInBoundsGEP(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }
            .into()
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
        unsafe { LLVMConstExtractValue(self.as_raw(), indices.as_mut_ptr(), indices.len() as u32) }
            .into()
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

impl TypeRef {
    pub fn inline_asm(
        &self,
        asm: &str,
        constraints: &str,
        has_side_effects: bool,
        is_align_stack: bool,
    ) -> ValueRef {
        unsafe {
            LLVMConstInlineAsm(
                self.as_raw(),
                unchecked_cstring(asm).as_ptr(),
                unchecked_cstring(constraints).as_ptr(),
                has_side_effects.as_bool(),
                is_align_stack.as_bool(),
            )
        }.into()
    }
}
