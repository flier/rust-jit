use std::borrow::Cow;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

/// The `select` instruction is used to choose one value based on a condition, without IR-level branching.
///
/// The `select` instruction requires an `i1` value or a vector of `i1` values indicating the condition, and two values of the same first class type.
///
/// - If the condition is an i1 and it evaluates to 1, the instruction returns the first value argument; otherwise, it returns the second value argument.
/// - If the condition is a vector of i1, then the value arguments must be vectors of the same size, and the selection is done element by element.
/// - If the condition is an i1 and the value arguments are vectors of the same size, then an entire vector is selected.
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
        trace!("{:?} emit instruction: {:?}", builder, self);

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

/// The `select` instruction is used to choose one value based on a condition, without IR-level branching.
///
/// The `select` instruction requires an `i1` value or a vector of `i1` values indicating the condition, and two values of the same first class type.
///
/// - If the condition is an i1 and it evaluates to 1, the instruction returns the first value argument; otherwise, it returns the second value argument.
/// - If the condition is a vector of i1, then the value arguments must be vectors of the same size, and the selection is done element by element.
/// - If the condition is an i1 and the value arguments are vectors of the same size, then an entire vector is selected.
pub fn select<'a, C, T, O, N>(cond: C, then: T, or_else: O, name: N) -> Select<'a>
where
    C: Into<ValueRef>,
    T: Into<ValueRef>,
    O: Into<ValueRef>,
    N: Into<Cow<'a, str>>,
{
    Select::new(cond.into(), then.into(), or_else.into(), name.into())
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn select_inst() {
        let context = Context::new();
        let module = context.create_module("select");
        let builder = context.create_builder();

        let bool_t = context.int1_t();

        let function_type = FunctionType::new(context.void_t(), &[bool_t], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let arg0_bool = function.get_param(0).unwrap();

        let i64_t = context.int64_t();

        assert_eq!(
            select(arg0_bool, i64_t.int(123), i64_t.int(456), "select")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%select = select i1 %0, i64 123, i64 456"
        );
    }
}
