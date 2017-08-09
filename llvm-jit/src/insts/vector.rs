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

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

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
pub fn extract_element<'a, V, I, N>(vector: V, index: I, name: N) -> ExtractElement<'a>
where
    V: Into<ValueRef>,
    I: Into<ValueRef>,
    N: Into<Cow<'a, str>>,
{
    ExtractElement::new(vector.into(), index.into(), name.into())
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

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

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
pub fn insert_element<'a, V, T, I, N>(vector: V, element: T, index: I, name: N) -> InsertElement<'a>
where
    V: Into<ValueRef>,
    T: Into<ValueRef>,
    I: Into<ValueRef>,
    N: Into<Cow<'a, str>>,
{
    InsertElement::new(vector.into(), element.into(), index.into(), name.into())
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

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

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
pub fn shuffle_vector<'a, V1, V2, M, N>(v1: V1, v2: V2, mask: M, name: N) -> ShuffleVector<'a>
where
    V1: Into<ValueRef>,
    V2: Into<ValueRef>,
    M: Into<ValueRef>,
    N: Into<Cow<'a, str>>,
{
    ShuffleVector::new(v1.into(), v2.into(), mask.into(), name.into())
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn vector() {
        let context = Context::new();
        let module = context.create_module("vector");
        let builder = context.create_builder();

        let i64_t = context.int64_t();
        let function_type = FunctionType::new(
            context.void_t(),
            types![i64_t.vector_t(3), i64_t.vector_t(3)],
            false,
        );
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let arg0_vector = function.get_param(0).unwrap();
        let arg1_vector = function.get_param(1).unwrap();

        let idx = i64_t.int(1);

        assert_eq!(
            extract_element(arg0_vector, idx, "extract_element")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_element = extractelement <3 x i64> %0, i64 1"
        );

        assert_eq!(
            insert_element(arg0_vector, i64_t.int(10), idx, "insert_element")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_element = insertelement <3 x i64> %0, i64 10, i64 1"
        );

        let i32_t = context.int32_t();
        let mask = vector![i32_t.int(1), i32_t.int(0), i32_t.int(2)];

        assert_eq!(
            shuffle_vector(arg0_vector, arg1_vector, mask, "shuffle_vector")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%shuffle_vector = shufflevector <3 x i64> %0, <3 x i64> %1, <3 x i32> <i32 1, i32 0, i32 2>"
        );
    }
}
