use llvm::LLVMTypeKind;

use function::FunctionType;
use intrinsics::IntrinsicId;
use intrinsics::gen::INTRINSIC_NAMES;
use types::{ArrayType, PointerType, SequentialType, StructType, TypeRef, VectorType};

impl IntrinsicId {
    /// Return the LLVM name for an intrinsic, such as "llvm.ppc.altivec.lvx".
    pub fn name(&self) -> &str {
        INTRINSIC_NAMES[*self as usize - 1]
    }

    pub fn name_of(&self, param_types: &[TypeRef]) -> String {
        let mut s = self.name().to_owned();

        for ty in param_types {
            s.push_str(&format!(".{}", ty.mangled_name()))
        }

        s
    }
}

impl TypeRef {
    /// Returns a stable mangling for the type specified for use in the name
    /// mangling scheme used by 'any' types in intrinsic signatures.  The mangling
    /// of named types is simply their name.  Manglings for unnamed types consist
    /// of a prefix ('p' for pointers, 'a' for arrays, 'f_' for functions)
    /// combined with the mangling of their component types.  A vararg function
    /// type will have a suffix of 'vararg'.  Since function types can contain
    /// other function types, we close a function type mangling with suffix 'f'
    /// which can't be confused with it's prefix.  This ensures we don't have
    /// collisions between two unrelated function types. Otherwise, you might
    /// parse ffXX as f(fXX) or f(fX)X.  (X is a placeholder for any other type.)
    /// Manglings of integers, floats, and vectors ('i', 'f', and 'v' prefix in most
    /// cases) fall back to the MVT codepath, where they could be mangled to
    /// 'x86mmx', for example; matching on derived types is not sufficient to mangle
    /// everything.
    pub fn mangled_name(&self) -> String {
        match self.kind() {
            LLVMTypeKind::LLVMVoidTypeKind => "isVoid".to_owned(),
            LLVMTypeKind::LLVMHalfTypeKind => "f16".to_owned(),
            LLVMTypeKind::LLVMFloatTypeKind => "f32".to_owned(),
            LLVMTypeKind::LLVMDoubleTypeKind => "f64".to_owned(),
            LLVMTypeKind::LLVMX86_FP80TypeKind => "f80".to_owned(),
            LLVMTypeKind::LLVMFP128TypeKind => "f128".to_owned(),
            LLVMTypeKind::LLVMPPC_FP128TypeKind => "ppcf128".to_owned(),
            LLVMTypeKind::LLVMLabelTypeKind => "label".to_owned(),
            LLVMTypeKind::LLVMTokenTypeKind => "token".to_owned(),
            LLVMTypeKind::LLVMMetadataTypeKind => "Metadata".to_owned(),
            LLVMTypeKind::LLVMX86_MMXTypeKind => "x86mmx".to_owned(),
            LLVMTypeKind::LLVMIntegerTypeKind => format!("i{}", self.bit_width()),
            LLVMTypeKind::LLVMPointerTypeKind => {
                let ty = PointerType::from(*self);

                format!("p{}{}", ty.address_space(), ty.element_type().mangled_name())
            }
            LLVMTypeKind::LLVMArrayTypeKind => {
                let ty = ArrayType::from(*self);

                format!("a{}{}", ty.len(), ty.element_type().mangled_name())
            }
            LLVMTypeKind::LLVMVectorTypeKind => {
                let ty = VectorType::from(*self);

                format!("v{}{}", ty.len(), ty.element_type().mangled_name())
            }
            LLVMTypeKind::LLVMStructTypeKind => {
                let ty = StructType::from(*self);

                let mut s = String::new();

                if ty.element_count() == 0 {
                    s.push_str(&format!("s_{}", ty.name().unwrap_or_default()))
                } else {
                    s.push_str("sl");

                    ty.elements().for_each(|element| s.push_str(&element.mangled_name()))
                };

                // Ensure nested structs are distinguishable.
                s.push_str("s");
                s
            }
            LLVMTypeKind::LLVMFunctionTypeKind => {
                let ty = FunctionType::from(*self);

                let mut s = format!("f_{}", ty.return_type().mangled_name());

                for param in ty.param_types() {
                    s.push_str(&param.mangled_name());
                }

                if ty.is_var_arg() {
                    s.push_str("vararg")
                }

                // Ensure nested function types are distinguishable.
                s.push_str("f");
                s
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use context::Context;
    use function::FunctionType;
    use intrinsics::IntrinsicId;
    use types::*;

    #[test]
    fn mangled_type() {
        let ctxt = Context::new();
        let i8_t = ctxt.int8_t();

        assert_eq!(ctxt.int8_t().mangled_name(), "i8");
        assert_eq!(ctxt.int16_t().mangled_name(), "i16");
        assert_eq!(ctxt.int32_t().mangled_name(), "i32");
        assert_eq!(ctxt.int64_t().mangled_name(), "i64");
        assert_eq!(ctxt.int128_t().mangled_name(), "i128");
        assert_eq!(ctxt.int_type(256).mangled_name(), "i256");

        assert_eq!(ctxt.half_t().mangled_name(), "f16");
        assert_eq!(ctxt.float_t().mangled_name(), "f32");
        assert_eq!(ctxt.double_t().mangled_name(), "f64");
        assert_eq!(ctxt.x86_fp80_t().mangled_name(), "f80");
        assert_eq!(ctxt.fp128_t().mangled_name(), "f128");
        assert_eq!(ctxt.ppc_fp128_t().mangled_name(), "ppcf128");

        assert_eq!(ctxt.void_t().mangled_name(), "isVoid");
        assert_eq!(ctxt.label_t().mangled_name(), "label");
        assert_eq!(ctxt.x86_mmx_t().mangled_name(), "x86mmx");

        assert_eq!(ctxt.int8_t().array_t(4).mangled_name(), "a4i8");
        assert_eq!(ctxt.int8_t().ptr_t().mangled_name(), "p0i8");
        assert_eq!(ctxt.int8_t().vector_t(4).mangled_name(), "v4i8");

        assert_eq!(
            StructType::new(types![i8_t, i8_t.ptr_t()], true).mangled_name(),
            "sli8p0i8s"
        );
        assert_eq!(ctxt.empty_struct_t("test").mangled_name(), "s_tests");

        assert_eq!(
            FunctionType::new(i8_t, types![i8_t.ptr_t()], true).mangled_name(),
            "f_i8p0i8varargf"
        );
    }

    #[test]
    fn intrinsic_name() {
        assert_eq!(IntrinsicId::memset.name(), "llvm.memset");
    }

    #[test]
    fn mangled_func() {
        let ctxt = Context::new();
        let i8_t = ctxt.int8_t();
        let i32_t = ctxt.int32_t();

        assert_eq!(
            IntrinsicId::memset.name_of(types![i8_t.ptr_t(), i32_t]),
            "llvm.memset.p0i8.i32"
        );
    }
}
