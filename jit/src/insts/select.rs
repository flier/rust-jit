use std::borrow::Cow;

use crate::llvm::core::*;

use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::utils::{AsRaw, IntoRaw};
use crate::value::Instruction;

/// The `select` instruction is used to choose one value based on a condition, without IR-level branching.
///
/// The `select` instruction requires an `i1` value or a vector of `i1` values indicating the condition, and two values of the same first class type.
///
/// - If the condition is an i1 and it evaluates to 1, the instruction returns the first value argument; otherwise, it returns the second value argument.
/// - If the condition is a vector of i1, then the value arguments must be vectors of the same size, and the selection is done element by element.
/// - If the condition is an i1 and the value arguments are vectors of the same size, then an entire vector is selected.
#[derive(Clone, Debug, PartialEq)]
pub struct Select<'a> {
    cond: Box<AstNode<'a>>,
    then: Box<AstNode<'a>>,
    or_else: Box<AstNode<'a>>,
    name: Cow<'a, str>,
}

impl<'a> Select<'a> {
    pub fn new<C, T, E, N>(cond: C, then: T, or_else: E, name: N) -> Self
    where
        C: Into<AstNode<'a>>,
        T: Into<AstNode<'a>>,
        E: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        Select {
            cond: Box::new(cond.into()),
            then: Box::new(then.into()),
            or_else: Box::new(or_else.into()),
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for Select<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildSelect(
                builder.as_raw(),
                self.cond.emit_to(builder).into_raw(),
                self.then.emit_to(builder).into_raw(),
                self.or_else.emit_to(builder).into_raw(),
                cstr!(self.name),
            )
        }
        .into()
    }
}

/// The `select` instruction is used to choose one value based on a condition, without IR-level branching.
///
/// The `select` instruction requires an `i1` value or a vector of `i1` values indicating the condition,
/// and two values of the same first class type.
///
/// - If the condition is an i1 and it evaluates to 1, the instruction returns the first value argument;
/// otherwise, it returns the second value argument.
/// - If the condition is a vector of i1, then the value arguments must be vectors of the same size,
/// and the selection is done element by element.
/// - If the condition is an i1 and the value arguments are vectors of the same size, then an entire vector is selected.
pub fn select<'a, C, T, E, N>(cond: C, then: T, or_else: E, name: N) -> Select<'a>
where
    C: Into<AstNode<'a>>,
    T: Into<AstNode<'a>>,
    E: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    Select::new(cond, then, or_else, name)
}

/// The `select` instruction is used to choose one value based on a condition, without IR-level branching.
///
/// The `select` instruction requires an `i1` value or a vector of `i1` values indicating the condition,
/// and two values of the same first class type.
///
/// - If the condition is an i1 and it evaluates to 1, the instruction returns the first value argument;
/// otherwise, it returns the second value argument.
/// - If the condition is a vector of i1, then the value arguments must be vectors of the same size,
/// and the selection is done element by element.
/// - If the condition is an i1 and the value arguments are vectors of the same size,
/// then an entire vector is selected.
#[macro_export]
macro_rules! select {
    ($cond: expr => $then: expr, _ => $or_else: expr; $name: expr) => {
        $crate::insts::select($cond, $then, $or_else, $name)
    };
    ($cond: expr => $then: expr, _ => $or_else: expr) => {
        select!($cond, $then, $or_else; "select")
    };
}

impl IRBuilder {
    /// The `select` instruction is used to choose one value based on a condition, without IR-level branching.
    ///
    /// The `select` instruction requires an `i1` value or a vector of `i1` values indicating the condition,
    /// and two values of the same first class type.
    ///
    /// - If the condition is an i1 and it evaluates to 1, the instruction returns the first value argument;
    /// otherwise, it returns the second value argument.
    /// - If the condition is a vector of i1, then the value arguments must be vectors of the same size,
    /// and the selection is done element by element.
    /// - If the condition is an i1 and the value arguments are vectors of the same size,
    /// then an entire vector is selected.
    pub fn select<'a, C, T, E, N>(&self, cond: C, then: T, or_else: E, name: N) -> Instruction
    where
        C: Into<AstNode<'a>>,
        T: Into<AstNode<'a>>,
        E: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        select(cond, then, or_else, name).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

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

        let arg0_bool = function.param(0).unwrap();

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
