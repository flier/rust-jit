use llvm::core::*;

macro_rules! define_unary_instruction {
    ($operator:ident, $func:path) => (
        #[derive(Clone, Debug, PartialEq)]
        pub struct $operator<'a> {
            value: $crate::ValueRef,
            name: ::std::borrow::Cow<'a, str>,
        }

        impl<'a> $operator<'a> {
            pub fn new(value: $crate::ValueRef, name: ::std::borrow::Cow<'a, str>) -> Self {
                $operator { value, name }
            }
        }

        impl<'a> $crate::insts::InstructionBuilder for $operator<'a> {
            type Target = $crate::Instruction;

            fn emit_to(&self, builder: & $crate::insts::IRBuilder) -> Self::Target {
                unsafe {
                    $func(
                        builder.as_raw(),
                        self.value.as_raw(),
                        $crate::utils::unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }.into()
            }
        }
    );

    ($operator:ident, $func:path, $alias:ident, $comment:expr) => (
        define_unary_instruction!($operator, $func);

        #[doc=$comment]
        #[macro_export]
        macro_rules! $alias {
            ($value:expr; $name:expr) => { $crate::insts::$operator::new($value.into(), $name.into()) }
        }
    );

    ($operator:ident, $func:path, $alias:ident) => (
        define_unary_instruction!($operator, $func);

        #[macro_export]
        macro_rules! $alias {
            ($value:expr; $name:expr) => { $crate::insts::$operator::new($value.into(), $name.into()) }
        }
    );
}

define_unary_instruction!(
    Neg,
    LLVMBuildNeg,
    neg,
    "The unary negation operator precedes its operand and negates it."
);
define_unary_instruction!(
    NSWNeg,
    LLVMBuildNSWNeg,
    neg_nsw,
    "The unary negation operator precedes its operand and negates it, the result value of the `neg` is a poison value if signed overflow occurs."
);
define_unary_instruction!(
    NUWNeg,
    LLVMBuildNUWNeg,
    neg_nuw,
    "The unary negation operator precedes its operand and negates it, the result value of the `neg` is a poison value if unsigned overflow occurs."
);
define_unary_instruction!(
    FNeg,
    LLVMBuildFNeg,
    fneg,
    "The unary negation operator precedes its operand and negates it."
);
define_unary_instruction!(
    Not,
    LLVMBuildNot,
    not,
    "The logical negation operator reverses the meaning of its operand."
);
define_unary_instruction!(
    IsNull,
    LLVMBuildIsNull,
    is_null,
    "Determine whether a value instance is null."
);
define_unary_instruction!(
    IsNotNull,
    LLVMBuildIsNotNull,
    is_not_null,
    "Determine whether a value instance is not null."
);

#[cfg(test)]
mod tests {
    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;

    macro_rules! test_unary_inst {
        ($builder:ident, $name:ident !( $arg0_i64:ident ), $display:expr) => (
            assert_eq!( $name !( $arg0_i64; stringify!($name) ).emit_to(& $builder).to_string().trim(), $display )
        )
    }

    #[test]
    fn unary() {
        let c = Context::new();
        let m = Module::with_name_in_context("instructions", &c);
        let b = IRBuilder::within_context(&c);

        let i64t = c.int64();
        let p_i64t = i64t.ptr();
        let f64t = c.double();

        let f_ty = FunctionType::new(c.void(), &[i64t, p_i64t.into(), f64t, f64t], false);
        let f = m.add_function("test", f_ty);

        let bb = f.append_basic_block_in_context("entry", &c);
        b.position(Position::AtEnd(bb));

        let arg0_i64 = f.get_param(0).unwrap();
        let arg1_p_i64 = f.get_param(1).unwrap();
        let arg2_f64 = f.get_param(2).unwrap();
        let arg3_f64 = f.get_param(3).unwrap();

        test_unary_inst!(b, neg!(arg0_i64), "%neg = sub i64 0, %0");
        test_unary_inst!(b, neg_nsw!(arg0_i64), "%neg_nsw = sub nsw i64 0, %0");
        test_unary_inst!(b, neg_nuw!(arg0_i64), "%neg_nuw = sub nuw i64 0, %0");
        test_unary_inst!(b, fneg!(arg2_f64), "%fneg = fsub double -0.000000e+00, %2");

        test_unary_inst!(b, not!(arg0_i64), "%not = xor i64 %0, -1");
        test_unary_inst!(
            b,
            not!(arg3_f64),
            "%not1 = xor double %3, 0xFFFFFFFFFFFFFFFF"
        );

        test_unary_inst!(b, is_null!(arg1_p_i64), "%is_null = icmp eq i64* %1, null");
        test_unary_inst!(
            b,
            is_not_null!(arg1_p_i64),
            "%is_not_null = icmp ne i64* %1, null"
        );
    }
}
