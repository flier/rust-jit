use std::borrow::Cow;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Select<'a> {
    cond: ValueRef,
    then: ValueRef,
    or_else: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> Select<'a> {
    pub fn new(cond: ValueRef, then: ValueRef, or_else: ValueRef, name: Cow<'a, str>) -> Self {
        Select {
            cond,
            then,
            or_else,
            name,
        }
    }
}

impl<'a> InstructionBuilder for Select<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe {
            LLVMBuildSelect(
                builder.as_raw(),
                self.cond.as_raw(),
                self.then.as_raw(),
                self.or_else.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

#[macro_export]
macro_rules! select {
    ($cond:expr => $then:expr, _ => $or_else:expr; $name:expr) => (
        $crate::insts::Select::new($cond.into(), $then.into(), $or_else.into(), $name.into())
    );
}

#[cfg(test)]
mod tests {
    use context::Context;
    use function::FunctionType;
    use insts::*;
    use module::Module;
    use types::*;
    use value::*;

    #[test]
    fn select() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let bool_t = context.int1_t();

        let function_type = FunctionType::new(context.void_t(), &[bool_t], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_bool = function.get_param(0).unwrap();

        let i64_t = context.int64_t();

        assert_eq!(
            select!(arg0_bool => i64_t.int(123), _ => i64_t.int(456); "select")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%select = select i1 %0, i64 123, i64 456"
        );
    }
}
