use std::borrow::Cow;
use std::fmt;

use llvm::core::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::AsRaw;
use value::Instruction;

/// This instruction extracts a single (scalar) element from a `VectorType` value
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractElement<'a, V, I> {
    vector: V,
    index: I,
    name: Cow<'a, str>,
}

impl<'a, V, I> ExtractElement<'a, V, I> {
    pub fn new(vector: V, index: I, name: Cow<'a, str>) -> Self {
        ExtractElement {
            vector,
            index,
            name,
        }
    }
}

impl<'a, V, I> InstructionBuilder for ExtractElement<'a, V, I>
where
    V: InstructionBuilder + fmt::Debug,
    I: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildExtractElement(
                builder.as_raw(),
                self.vector.emit_to(builder).into().as_raw(),
                self.index.emit_to(builder).into().as_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
pub fn extract_element<'a, V, I, N>(vector: V, index: I, name: N) -> ExtractElement<'a, V, I>
where
    N: Into<Cow<'a, str>>,
{
    ExtractElement::new(vector, index, name.into())
}

/// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
#[macro_export]
macro_rules! extract_element {
    ($vector:expr, $index:expr ; $name:expr) => (
        $crate::insts::extract_element($vector, $index, $name)
    );
    ($vector:expr, $index:expr) => (
        extract_element!($vector, $index ; "extract_element")
    )
}

/// This instruction inserts a single (scalar) element into a `VectorType` value
#[derive(Clone, Debug, PartialEq)]
pub struct InsertElement<'a, V, E, I> {
    vector: V,
    element: E,
    index: I,
    name: Cow<'a, str>,
}

impl<'a, V, E, I> InsertElement<'a, V, E, I> {
    pub fn new(vector: V, element: E, index: I, name: Cow<'a, str>) -> Self {
        InsertElement {
            vector,
            element,
            index,
            name,
        }
    }
}

impl<'a, V, E, I> InstructionBuilder for InsertElement<'a, V, E, I>
where
    V: InstructionBuilder + fmt::Debug,
    E: InstructionBuilder + fmt::Debug,
    I: InstructionBuilder + fmt::Debug,{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildInsertElement(
                builder.as_raw(),
                self.vector.emit_to(builder).into().as_raw(),
                self.element.emit_to(builder).into().as_raw(),
                self.index.emit_to(builder).into().as_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
pub fn insert_element<'a, V, E, I, N>(
    vector: V,
    element: E,
    index: I,
    name: N,
) -> InsertElement<'a, V, E, I>
where
    N: Into<Cow<'a, str>>,
{
    InsertElement::new(vector, element, index, name.into())
}

/// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
#[macro_export]
macro_rules! insert_element {
    ($vector:expr, $element:expr, $index:expr ; $name:expr) => (
        $crate::insts::insert_element($vector, $element, $index, $name)
    );
    ($vector:expr, $element:expr, $index:expr) => (
        insert_element!($vector, $element, $index ; "insert_element")
    )
}

/// This instruction constructs a fixed permutation of two input vectors.
#[derive(Clone, Debug, PartialEq)]
pub struct ShuffleVector<'a, V1, V2, M> {
    v1: V1,
    v2: V2,
    mask: M,
    name: Cow<'a, str>,
}

impl<'a, V1, V2, M> ShuffleVector<'a, V1, V2, M> {
    pub fn new(v1: V1, v2: V2, mask: M, name: Cow<'a, str>) -> Self {
        ShuffleVector { v1, v2, mask, name }
    }
}

impl<'a, V1, V2, M> InstructionBuilder for ShuffleVector<'a, V1, V2, M>
where
    V1: InstructionBuilder
        + fmt::Debug,
    V2: InstructionBuilder
        + fmt::Debug,
    M: InstructionBuilder
        + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildShuffleVector(
                builder.as_raw(),
                self.v1.emit_to(builder).into().as_raw(),
                self.v2.emit_to(builder).into().as_raw(),
                self.mask.emit_to(builder).into().as_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
/// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
pub fn shuffle_vector<'a, V1, V2, M, N>(
    v1: V1,
    v2: V2,
    mask: M,
    name: N,
) -> ShuffleVector<'a, V1, V2, M>
where
    N: Into<Cow<'a, str>>,
{
    ShuffleVector::new(v1, v2, mask, name.into())
}

/// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
/// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
#[macro_export]
macro_rules! shuffle_vector {
    ($v1:expr, $v2:expr, $mask:expr ; $name:expr) => (
        $crate::insts::shuffle_vector($v1, $v2, $mask, $name)
    );
    ($v1:expr, $v2:expr, $mask:expr) => (
        shuffle_vector!($v1, $v2, $mask ; "shuffle_vector")
    )
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
            extract_element!(arg0_vector, idx)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_element = extractelement <3 x i64> %0, i64 1"
        );

        assert_eq!(
            insert_element!(arg0_vector, i64_t.int(10), idx)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_element = insertelement <3 x i64> %0, i64 10, i64 1"
        );

        let i32_t = context.int32_t();
        let mask = vector![i32_t.int(1), i32_t.int(0), i32_t.int(2)];

        assert_eq!(
            shuffle_vector!(arg0_vector, arg1_vector, mask)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%shuffle_vector = shufflevector <3 x i64> %0, <3 x i64> %1, <3 x i32> <i32 1, i32 0, i32 2>"
        );
    }
}
