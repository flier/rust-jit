use std::borrow::Cow;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

/// The `va_arg` instruction is used to access arguments passed through the “variable argument” area of a function call. It is used to implement the `va_arg` macro in C.
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
        trace!("{:?} emit instruction: {:?}", builder, self);

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

/// The `va_arg` instruction is used to access arguments passed through the “variable argument” area of a function call. It is used to implement the `va_arg` macro in C.
pub fn va_arg<'a, A, T, N>(args: A, ty: T, name: N) -> VaArg<'a>
where
    A: Into<ValueRef>,
    T: Into<TypeRef>,
    N: Into<Cow<'a, str>>,
{
    VaArg::new(args.into(), ty.into(), name.into())
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn va_arg_inst() {
        let context = Context::new();
        let module = context.create_module("va_arg");
        let builder = context.create_builder();

        let fn_test = module.add_function("test", FunctionType::new(context.void_t(), &[], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i8_t = context.int8_t();
        let p_i8_t = i8_t.ptr_t();
        let va_list = context.anonymous_struct_t(&[p_i8_t.into()], false);

        let ap = alloca!(va_list; "ap").emit_to(&builder);
        va_arg(ap, context.int32_t(), "va_arg").emit_to(&builder);

        assert_eq!(
            bb.last_instructions(4),
            vec!["%ap = alloca { i8* }", "%va_arg = va_arg { i8* }* %ap, i32"]
        );
    }
}
