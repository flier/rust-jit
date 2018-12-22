use std::borrow::Cow;

use crate::llvm::core::*;

use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::utils::{AsRaw, IntoRaw};
use crate::value::Instruction;

/// This instruction extracts a struct member or array element value from an aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractValue<'a> {
    aggregate: Box<AstNode<'a>>,
    index: u32,
    name: Cow<'a, str>,
}

impl<'a> ExtractValue<'a> {
    pub fn new<T, N>(aggregate: T, index: u32, name: N) -> Self
    where
        T: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        ExtractValue {
            aggregate: Box::new(aggregate.into()),
            index,
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for ExtractValue<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildExtractValue(
                builder.as_raw(),
                self.aggregate.emit_to(builder).into_raw(),
                self.index,
                cstr!(self.name),
            )
        }
        .into()
    }
}

/// The `extractvalue` instruction extracts the value of a member field from an aggregate value.
pub fn extract_value<'a, T, N>(aggregate: T, index: u32, name: N) -> ExtractValue<'a>
where
    T: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    ExtractValue::new(aggregate, index, name)
}

#[macro_export]
macro_rules! extract_value {
    ($aggregate: expr, $index: expr; $name: expr) => {
        $crate::insts::extract_value($aggregate, $index as u32, $name)
    };
    ($aggregate: expr, $index: expr) => {
        extract_value!($aggregate, $index; "extract_value")
    };
}

/// This instruction inserts a struct field of array element value into an aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct InsertValue<'a> {
    aggregate: Box<AstNode<'a>>,
    element: Box<AstNode<'a>>,
    index: u32,
    name: Cow<'a, str>,
}

impl<'a> InsertValue<'a> {
    pub fn new<T, E, N>(aggregate: T, element: E, index: u32, name: N) -> Self
    where
        T: Into<AstNode<'a>>,
        E: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        InsertValue {
            aggregate: Box::new(aggregate.into()),
            element: Box::new(element.into()),
            index,
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for InsertValue<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildInsertValue(
                builder.as_raw(),
                self.aggregate.emit_to(builder).into_raw(),
                self.element.emit_to(builder).into_raw(),
                self.index,
                cstr!(self.name),
            )
        }
        .into()
    }
}

/// The `insertvalue` instruction inserts a value into a member field in an aggregate value.
pub fn insert_value<'a, T, E, N>(aggregate: T, element: E, index: u32, name: N) -> InsertValue<'a>
where
    T: Into<AstNode<'a>>,
    E: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    InsertValue::new(aggregate, element, index, name)
}

#[macro_export]
macro_rules! insert_value {
    ($aggregate: expr, $element: expr, $index: expr; $name: expr) => {
        $crate::insts::insert_value($aggregate, $element, $index as u32, $name)
    };
    ($aggregate: expr, $element: expr, $index: expr) => {
        insert_value!($aggregate, $element, $index; "insert_value")
    };
}

impl IRBuilder {
    /// The `extractvalue` instruction extracts the value of a member field from an aggregate value.
    pub fn extract_value<'a, T, N>(&self, aggregate: T, index: u32, name: N) -> Instruction
    where
        T: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        extract_value(aggregate, index, name).emit_to(self)
    }

    /// The `insertvalue` instruction inserts a value into a member field in an aggregate value.
    pub fn insert_value<'a, T, E, N>(&self, aggregate: T, element: E, index: u32, name: N) -> Instruction
    where
        T: Into<AstNode<'a>>,
        E: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        insert_value(aggregate, element, index, name).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

    #[test]
    fn aggregate() {
        let context = Context::new();
        let module = context.create_module("aggregate");
        let builder = context.create_builder();

        let i32_t = context.int32_t();
        let i64_t = context.int64_t();
        let array_t = i64_t.array_t(4).into();
        let struct_t = context.struct_t(&[i32_t, i64_t], false);
        let function_type = FunctionType::new(context.void_t(), &[array_t, struct_t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let arg0_array = function.get_param(0).unwrap();
        let arg1_struct = function.get_param(1).unwrap();

        assert_eq!(
            extract_value!(arg0_array, 1).emit_to(&builder).to_string().trim(),
            "%extract_value = extractvalue [4 x i64] %0, 1"
        );

        assert_eq!(
            extract_value!(arg1_struct, 1).emit_to(&builder).to_string().trim(),
            "%extract_value1 = extractvalue { i32, i64 } %1, 1"
        );

        assert_eq!(
            insert_value!(arg0_array, i64_t.int(123), 1)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_value = insertvalue [4 x i64] %0, i64 123, 1"
        );

        assert_eq!(
            insert_value!(arg1_struct, i64_t.int(123), 1)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_value2 = insertvalue { i32, i64 } %1, i64 123, 1"
        );
    }
}
