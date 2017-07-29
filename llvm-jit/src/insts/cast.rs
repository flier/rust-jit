use llvm::core::*;

macro_rules! define_cast_instruction {
    ($operator:ident, $func:path) => (
        #[derive(Clone, Debug, PartialEq)]
        pub struct $operator<'a> {
            value: $crate::ValueRef,
            dest_ty: $crate::TypeRef,
            name: ::std::borrow::Cow<'a, str>,
        }

        impl<'a> $operator<'a> {
            pub fn new(value: $crate::ValueRef, dest_ty: $crate::TypeRef, name: ::std::borrow::Cow<'a, str>) -> Self {
                $operator { value, dest_ty, name }
            }
        }

        impl<'a> $crate::insts::InstructionBuilder for $operator<'a> {
            type Target = $crate::Instruction;

            fn emit_to(&self, builder: & $crate::insts::IRBuilder) -> Self::Target {
                unsafe {
                    $func(
                        builder.as_raw(),
                        self.value.as_raw(),
                        self.dest_ty.as_raw(),
                        $crate::utils::unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }.into()
            }
        }
    );
    ($operator:ident, $func:path, $alias:ident, $comment:expr) => (
        define_cast_instruction!($operator, $func);

        #[doc=$comment]
        #[macro_export]
        macro_rules! $alias {
            ($value:expr, $dest_ty:expr; $name:expr) => {
                $crate::insts::$operator::new($value.into(), $dest_ty.into(), $name.into())
            }
        }
    );
    ($operator:ident, $func:path, $alias:ident) => (
        define_cast_instruction!($operator, $func);

        #[macro_export]
        macro_rules! $alias {
            ($value:expr, $dest_ty:expr; $name:expr) => {
                $crate::insts::$operator::new($value.into(), $dest_ty.into(), $name.into())
            }
        }
    )
}

define_cast_instruction!(
    Trunc,
    LLVMBuildTrunc,
    trunc,
    "The `trunc` instruction truncates its operand to the type."
);
define_cast_instruction!(
    ZExt,
    LLVMBuildZExt,
    zext,
    "The `zext` instruction zero extends its operand to type."
);
define_cast_instruction!(
    SExt,
    LLVMBuildSExt,
    sext,
    "The `sext` instruction takes a value to cast, and a type to cast it to. Both types must be of integer types, or vectors of the same number of integers. The bit size of the value must be smaller than the bit size of the destination type."
);
define_cast_instruction!(
    FPTrunc,
    LLVMBuildFPTrunc,
    fptrunc,
    "The `fptrunc` instruction truncates value to type."
);
define_cast_instruction!(
    FPExt,
    LLVMBuildFPExt,
    fpext,
    "The `fpext` extends a floating point value to a larger floating point value."
);
define_cast_instruction!(
    FPToUI,
    LLVMBuildFPToUI,
    fptoui,
    "The `fptoui` converts a floating point value to its unsigned integer equivalent of type."
);
define_cast_instruction!(
    FPToSI,
    LLVMBuildFPToSI,
    fptosi,
    "The `fptosi` instruction converts floating point value to type."
);
define_cast_instruction!(
    UIToFP,
    LLVMBuildUIToFP,
    uitofp,
    "The `uitofp` instruction regards value as an unsigned integer and converts that value to the type."
);
define_cast_instruction!(
    SIToFP,
    LLVMBuildSIToFP,
    sitofp,
    "The `sitofp` instruction regards value as a signed integer and converts that value to the type."
);
define_cast_instruction!(
    PtrToInt,
    LLVMBuildPtrToInt,
    ptr_to_int,
    "The `ptrtoint` instruction converts the pointer or a vector of pointers value to the integer (or vector of integers) type."
);
define_cast_instruction!(
    IntToPtr,
    LLVMBuildIntToPtr,
    int_to_ptr,
    "The `inttoptr` instruction converts an integer value to a pointer type."
);
define_cast_instruction!(
    BitCast,
    LLVMBuildBitCast,
    bit_cast,
    "The `bitcast` instruction converts value to type ty2 without changing any bits."
);
define_cast_instruction!(
    AddrSpaceCast,
    LLVMBuildAddrSpaceCast,
    addrspace_cast,
    "The `addrspacecast` instruction converts ptrval from pty in address space n to type pty2 in address space m."
);
define_cast_instruction!(
    ZExtOrBitCast,
    LLVMBuildZExtOrBitCast,
    zext_or_bit_cast,
    "Create a `zext` or `bitcast` cast instruction."
);
define_cast_instruction!(
    SExtOrBitCast,
    LLVMBuildSExtOrBitCast,
    sext_or_bit_cast,
    "Create a `sext` or `bitcast` cast instruction."
);
define_cast_instruction!(
    TruncOrBitCast,
    LLVMBuildTruncOrBitCast,
    trunc_or_bit_cast,
    "Create a `trunc` or `bitcast` cast instruction."
);
define_cast_instruction!(
    PointerCast,
    LLVMBuildPointerCast,
    ptr_cast,
    "Create a `bitcast`, `addrspacecast`, or `ptrtoint` cast instruction."
);
define_cast_instruction!(
    IntCast,
    LLVMBuildIntCast,
    int_cast,
    "Create a `zext`, `bitcast`, or `trunc` instruction for int -> int casts."
);
define_cast_instruction!(
    FPCast,
    LLVMBuildFPCast,
    fp_cast,
    "Create an `fpext`, `bitcast`, or `fptrunc` instruction for fp -> fp casts."
);

#[cfg(test)]
mod tests {
    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;

    macro_rules! test_instruction {
        ($builder:ident, $name:ident !( $arg0_i64:ident, $arg1_i64:ident ), $display:expr) => (
            assert_eq!( $name !( $arg0_i64, $arg1_i64; stringify!($name) ).emit_to(& $builder).to_string().trim(), $display )
        );
    }

    #[test]
    fn cast() {
        let c = Context::new();
        let m = Module::with_name_in_context("instructions", &c);
        let b = IRBuilder::within_context(&c);

        let i32t = c.int32_t();
        let i64t = c.int64_t();
        let f32t = c.float_t();
        let f64t = c.double_t();

        let p_i64t = i64t.ptr();
        let p_f64t = f64t.ptr();
        let p_f64t_1 = f64t.ptr_in_address_space(1);

        let f_ty = FunctionType::new(
            c.void_t(),
            &[
                i64t,
                i64t,
                f64t,
                f64t,
                i32t,
                f32t,
                p_i64t.into(),
                p_f64t.into(),
            ],
            false,
        );
        let f = m.add_function("test", f_ty);

        let bb = f.append_basic_block_in_context("entry", &c);
        b.position(Position::AtEnd(bb));

        let arg0_i64 = f.get_param(0).unwrap();
        let _arg1_i64 = f.get_param(1).unwrap();
        let arg2_f64 = f.get_param(2).unwrap();
        let _arg3_f64 = f.get_param(3).unwrap();
        let arg4_i32 = f.get_param(4).unwrap();
        let arg5_f32 = f.get_param(5).unwrap();
        let arg6_p_i64 = f.get_param(6).unwrap();
        let arg7_p_f64 = f.get_param(7).unwrap();

        test_instruction!(b, trunc!(arg0_i64, i32t), "%trunc = trunc i64 %0 to i32");
        test_instruction!(b, zext!(arg4_i32, i64t), "%zext = zext i32 %4 to i64");
        test_instruction!(b, sext!(arg4_i32, i64t), "%sext = sext i32 %4 to i64");

        test_instruction!(
            b,
            fptrunc!(arg2_f64, f32t),
            "%fptrunc = fptrunc double %2 to float"
        );
        test_instruction!(
            b,
            fpext!(arg5_f32, f64t),
            "%fpext = fpext float %5 to double"
        );

        test_instruction!(
            b,
            fptoui!(arg2_f64, i64t),
            "%fptoui = fptoui double %2 to i64"
        );
        test_instruction!(
            b,
            fptosi!(arg2_f64, i64t),
            "%fptosi = fptosi double %2 to i64"
        );
        test_instruction!(
            b,
            uitofp!(arg0_i64, f64t),
            "%uitofp = uitofp i64 %0 to double"
        );
        test_instruction!(
            b,
            sitofp!(arg0_i64, f64t),
            "%sitofp = sitofp i64 %0 to double"
        );

        test_instruction!(
            b,
            ptr_to_int!(arg6_p_i64, i64t),
            "%ptr_to_int = ptrtoint i64* %6 to i64"
        );
        test_instruction!(
            b,
            int_to_ptr!(arg0_i64, p_i64t),
            "%int_to_ptr = inttoptr i64 %0 to i64*"
        );

        test_instruction!(
            b,
            bit_cast!(arg6_p_i64, p_f64t),
            "%bit_cast = bitcast i64* %6 to double*"
        );
        test_instruction!(
            b,
            addrspace_cast!(arg7_p_f64, p_f64t_1),
            "%addrspace_cast = addrspacecast double* %7 to double addrspace(1)*"
        );

        test_instruction!(
            b,
            trunc_or_bit_cast!(arg0_i64, i32t),
            "%trunc_or_bit_cast = trunc i64 %0 to i32"
        );
        test_instruction!(
            b,
            trunc_or_bit_cast!(arg0_i64, f64t),
            "%trunc_or_bit_cast1 = bitcast i64 %0 to double"
        );
        test_instruction!(
            b,
            zext_or_bit_cast!(arg4_i32, i64t),
            "%zext_or_bit_cast = zext i32 %4 to i64"
        );
        test_instruction!(
            b,
            zext_or_bit_cast!(arg4_i32, f32t),
            "%zext_or_bit_cast2 = bitcast i32 %4 to float"
        );
        test_instruction!(
            b,
            sext_or_bit_cast!(arg4_i32, i64t),
            "%sext_or_bit_cast = sext i32 %4 to i64"
        );
        test_instruction!(
            b,
            sext_or_bit_cast!(arg4_i32, f32t),
            "%sext_or_bit_cast3 = bitcast i32 %4 to float"
        );

        test_instruction!(
            b,
            ptr_cast!(arg6_p_i64, p_f64t),
            "%ptr_cast = bitcast i64* %6 to double*"
        );
        test_instruction!(
            b,
            int_cast!(arg0_i64, i32t),
            "%int_cast = trunc i64 %0 to i32"
        );
        test_instruction!(
            b,
            int_cast!(arg4_i32, i64t),
            "%int_cast4 = sext i32 %4 to i64"
        );
        test_instruction!(
            b,
            fp_cast!(arg2_f64, f32t),
            "%fp_cast = fptrunc double %2 to float"
        );
        test_instruction!(
            b,
            fp_cast!(arg5_f32, f64t),
            "%fp_cast5 = fpext float %5 to double"
        );
    }
}
