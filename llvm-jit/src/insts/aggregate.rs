use std::borrow::Cow;
use std::fmt;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::unchecked_cstring;
use value::Instruction;

/// This instruction extracts a struct member or array element value from an aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractValue<'a, T> {
    aggregate: T,
    index: u32,
    name: Cow<'a, str>,
}

impl<'a, T> ExtractValue<'a, T> {
    pub fn new(aggregate: T, index: u32, name: Cow<'a, str>) -> Self {
        ExtractValue {
            aggregate,
            index,
            name,
        }
    }
}

impl<'a, T> InstructionBuilder for ExtractValue<'a, T>
where
    T: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildExtractValue(
                builder.as_raw(),
                self.aggregate.emit_to(builder).into().as_raw(),
                self.index,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `extractvalue` instruction extracts the value of a member field from an aggregate value.
pub fn extract_value<'a, T, N: Into<Cow<'a, str>>>(
    aggregate: T,
    index: u32,
    name: N,
) -> ExtractValue<'a, T> {
    ExtractValue::new(aggregate, index, name.into())
}

#[macro_export]
macro_rules! extract_value {
    ($aggregate:expr, $index:expr; $name:expr) => (
        $crate::insts::ExtractValue::new($aggregate, $index as u32, $name.into())
    );
    ($aggregate:expr, $index:expr) => {
        extract_value!($aggregate, $index; "extract_value")
    }
}

/// This instruction inserts a struct field of array element value into an aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct InsertValue<'a, T, E> {
    aggregate: T,
    element: E,
    index: u32,
    name: Cow<'a, str>,
}

impl<'a, T, E> InsertValue<'a, T, E> {
    pub fn new(aggregate: T, element: E, index: u32, name: Cow<'a, str>) -> Self {
        InsertValue {
            aggregate,
            element,
            index,
            name,
        }
    }
}

impl<'a, T, E> InstructionBuilder for InsertValue<'a, T, E>
where
    T: InstructionBuilder + fmt::Debug,
    E: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildInsertValue(
                builder.as_raw(),
                self.aggregate.emit_to(builder).into().as_raw(),
                self.element.emit_to(builder).into().as_raw(),
                self.index,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `insertvalue` instruction inserts a value into a member field in an aggregate value.
pub fn insert_value<'a, T, E, N: Into<Cow<'a, str>>>(
    aggregate: T,
    element: E,
    index: u32,
    name: N,
) -> InsertValue<'a, T, E> {
    InsertValue::new(aggregate, element, index, name.into())
}

#[macro_export]
macro_rules! insert_value {
    ($aggregate:expr, $element:expr, $index:expr; $name:expr) => (
        $crate::insts::InsertValue::new($aggregate, $element, $index as u32, $name.into())
    );
    ($aggregate:expr, $element:expr, $index:expr) => {
        insert_value!($aggregate, $element, $index; "extract_value")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insts::*;
    use prelude::*;

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
            extract_value(arg0_array, 1, "extract_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_value = extractvalue [4 x i64] %0, 1"
        );

        assert_eq!(
            extract_value(arg1_struct, 1, "extract_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_value1 = extractvalue { i32, i64 } %1, 1"
        );

        assert_eq!(
            insert_value(arg0_array, i64_t.int(123), 1, "insert_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_value = insertvalue [4 x i64] %0, i64 123, 1"
        );

        assert_eq!(
            insert_value(arg1_struct, i64_t.int(123), 1, "insert_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_value2 = insertvalue { i32, i64 } %1, i64 123, 1"
        );
    }
}
