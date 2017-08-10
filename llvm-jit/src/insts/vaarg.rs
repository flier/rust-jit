use std::borrow::Cow;
use std::fmt;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::unchecked_cstring;
use value::Instruction;

/// The `va_arg` instruction is used to access arguments passed through the “variable argument” area of a function call. It is used to implement the `va_arg` macro in C.
#[derive(Clone, Debug, PartialEq)]
pub struct VaArg<'a, V> {
    args: V,
    ty: TypeRef,
    name: Cow<'a, str>,
}

impl<'a, V> VaArg<'a, V> {
    pub fn new(args: V, ty: TypeRef, name: Cow<'a, str>) -> Self {
        VaArg { args, ty, name }
    }
}

impl<'a, V> InstructionBuilder for VaArg<'a, V>
where
    V: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildVAArg(
                builder.as_raw(),
                self.args.emit_to(builder).into().as_raw(),
                self.ty.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `va_arg` instruction is used to access arguments passed through the “variable argument” area
/// of a function call. It is used to implement the `va_arg` macro in C.
pub fn va_arg<'a, V, T, N>(args: V, ty: T, name: N) -> VaArg<'a, V>
where
    T: Into<TypeRef>,
    N: Into<Cow<'a, str>>,
{
    VaArg::new(args.into(), ty.into(), name.into())
}

/// The `va_arg` instruction is used to access arguments passed through the “variable argument” area
/// of a function call. It is used to implement the `va_arg` macro in C.
#[macro_export]
macro_rules! va_arg {
    ($args:expr, $ty:expr ; $name:expr) => (
        $crate::insts::va_arg($args, $ty, $name)
    );
    ($args:expr, $ty:expr) => (
        va_arg!($args, $ty ; "va_arg")
    )
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
        builder.position_at_end(bb);

        let i8_t = context.int8_t();
        let p_i8_t = i8_t.ptr_t();
        let va_list = context.struct_t(&[p_i8_t.into()], false);

        let ap = alloca!(va_list; "ap");
        va_arg!(ap, context.int32_t()).emit_to(&builder);

        assert_eq!(
            bb.last_instructions(4),
            vec!["%ap = alloca { i8* }", "%va_arg = va_arg { i8* }* %ap, i32"]
        );
    }
}
