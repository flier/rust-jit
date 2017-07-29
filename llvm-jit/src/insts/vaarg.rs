use std::borrow::Cow;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct VaArg<'a> {
    args: ValueRef,
    ty: TypeRef,
    name: Cow<'a, str>,
}

impl<'a> VaArg<'a> {
    pub fn new(args: ValueRef, ty: TypeRef, name: Cow<'a, str>) -> Self {
        VaArg { args, ty, name }
    }
}

impl<'a> InstructionBuilder for VaArg<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe {
            LLVMBuildVAArg(
                builder.as_raw(),
                self.args.as_raw(),
                self.ty.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

#[macro_export]
macro_rules! va_arg {
    ($args:expr, $ty:expr; $name:expr) => ({
        $crate::insts::VaArg::new($args.into(), $ty.into(), $name.into())
    });
}

#[cfg(test)]
mod tests {
    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;

    #[test]
    fn va_arg() {
        let context = Context::new();
        let module = Module::with_name_in_context("invoke", &context);
        let builder = IRBuilder::within_context(&context);

        let fn_test = module.add_function("test", FunctionType::new(context.void_t(), &[], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i8_t = context.int8_t();
        let p_i8_t = i8_t.ptr_t();
        let va_list = context.anonymous_struct_t(&[p_i8_t.into()], false);

        let ap = alloca!(va_list; "ap").emit_to(&builder);
        va_arg!(ap, context.int32_t(); "va_arg").emit_to(&builder);

        assert_eq!(
            bb.last_instructions(4),
            vec!["%ap = alloca { i8* }", "%va_arg = va_arg { i8* }* %ap, i32"]
        );
    }
}
