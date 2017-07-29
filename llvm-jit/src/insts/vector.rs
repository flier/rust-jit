use std::borrow::Cow;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

/// This instruction extracts a single (scalar) element from a `VectorType` value
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractElement<'a> {
    vector: ValueRef,
    index: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> ExtractElement<'a> {
    pub fn new(vector: ValueRef, index: ValueRef, name: Cow<'a, str>) -> Self {
        ExtractElement {
            vector,
            index,
            name,
        }
    }
}

impl<'a> InstructionBuilder for ExtractElement<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe {
            LLVMBuildExtractElement(
                builder.as_raw(),
                self.vector.as_raw(),
                self.index.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
#[macro_export]
macro_rules! extract_element {
    ($vector:expr, $index:expr; $name:expr) => ({
        $crate::insts::ExtractElement::new($vector.into(), $index.into(), $name.into())
    })
}

/// This instruction inserts a single (scalar) element into a `VectorType` value
#[derive(Clone, Debug, PartialEq)]
pub struct InsertElement<'a> {
    vector: ValueRef,
    element: ValueRef,
    index: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> InsertElement<'a> {
    pub fn new(vector: ValueRef, element: ValueRef, index: ValueRef, name: Cow<'a, str>) -> Self {
        InsertElement {
            vector,
            element,
            index,
            name,
        }
    }
}

impl<'a> InstructionBuilder for InsertElement<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe {
            LLVMBuildInsertElement(
                builder.as_raw(),
                self.vector.as_raw(),
                self.element.as_raw(),
                self.index.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
#[macro_export]
macro_rules! insert_element {
    ($vector:expr, $element:expr, $index:expr; $name:expr) => ({
        $crate::insts::InsertElement::new($vector.into(), $element.into(), $index.into(), $name.into())
    })
}

/// This instruction constructs a fixed permutation of two input vectors.
#[derive(Clone, Debug, PartialEq)]
pub struct ShuffleVector<'a> {
    v1: ValueRef,
    v2: ValueRef,
    mask: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> ShuffleVector<'a> {
    pub fn new(v1: ValueRef, v2: ValueRef, mask: ValueRef, name: Cow<'a, str>) -> Self {
        ShuffleVector { v1, v2, mask, name }
    }
}

impl<'a> InstructionBuilder for ShuffleVector<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe {
            LLVMBuildShuffleVector(
                builder.as_raw(),
                self.v1.as_raw(),
                self.v2.as_raw(),
                self.mask.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
/// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
#[macro_export]
macro_rules! shuffle_vector {
    ($v1:expr, $v2:expr, $mask:expr; $name:expr) => ({
        $crate::insts::ShuffleVector::new($v1.into(), $v2.into(), $mask.into(), $name.into())
    })
}

#[cfg(test)]
mod tests {
    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;
    use value::*;

    #[test]
    fn vector() {
        let context = Context::new();
        let module = Module::with_name_in_context("vector", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64_t();
        let function_type = FunctionType::new(
            context.void_t(),
            &[i64t.vector_t(3).into(), i64t.vector_t(3).into()],
            false,
        );
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_vector = function.get_param(0).unwrap();
        let arg1_vector = function.get_param(1).unwrap();

        let idx = i64t.int(1);

        assert_eq!(
            extract_element!(arg0_vector, idx; "extract_element")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_element = extractelement <3 x i64> %0, i64 1"
        );

        assert_eq!(
            insert_element!(arg0_vector, i64t.int(10), idx; "insert_element")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_element = insertelement <3 x i64> %0, i64 10, i64 1"
        );

        let i32t = context.int32_t();
        let mask = vector![i32t.int(1), i32t.int(0), i32t.int(2)];

        assert_eq!(
            shuffle_vector!(arg0_vector, arg1_vector, mask; "shuffle_vector")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%shuffle_vector = shufflevector <3 x i64> %0, <3 x i64> %1, <3 x i32> <i32 1, i32 0, i32 2>"
        );
    }
}
