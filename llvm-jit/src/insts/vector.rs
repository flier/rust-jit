use std::borrow::Cow;

use llvm::core::*;

use insts::{AstNode, IRBuilder, InstructionBuilder};
use utils::{AsRaw, IntoRaw};
use value::Instruction;

/// This instruction extracts a single (scalar) element from a `VectorType` value
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractElement<'a> {
    vector: Box<AstNode<'a>>,
    index: Box<AstNode<'a>>,
    name: Cow<'a, str>,
}

impl<'a> ExtractElement<'a> {
    pub fn new<V, I, N>(vector: V, index: I, name: N) -> Self
    where
        V: Into<AstNode<'a>>,
        I: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        ExtractElement {
            vector: Box::new(vector.into()),
            index: Box::new(index.into()),
            name: name.into(),
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
                self.vector.emit_to(builder).into_raw(),
                self.index.emit_to(builder).into_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
pub fn extract_element<'a, V, I, N>(vector: V, index: I, name: N) -> ExtractElement<'a>
where
    V: Into<AstNode<'a>>,
    I: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    ExtractElement::new(vector, index, name)
}

/// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
#[macro_export]
macro_rules! extract_element {
    ($vector: expr, $index: expr; $name: expr) => {
        $crate::insts::extract_element($vector, $index, $name)
    };
    ($vector: expr, $index: expr) => {
        extract_element!($vector, $index ; "extract_element")
    };
}

/// This instruction inserts a single (scalar) element into a `VectorType` value
#[derive(Clone, Debug, PartialEq)]
pub struct InsertElement<'a> {
    vector: Box<AstNode<'a>>,
    element: Box<AstNode<'a>>,
    index: Box<AstNode<'a>>,
    name: Cow<'a, str>,
}

impl<'a> InsertElement<'a> {
    pub fn new<V, E, I, N>(vector: V, element: E, index: I, name: N) -> Self
    where
        V: Into<AstNode<'a>>,
        E: Into<AstNode<'a>>,
        I: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        InsertElement {
            vector: Box::new(vector.into()),
            element: Box::new(element.into()),
            index: Box::new(index.into()),
            name: name.into(),
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
                self.vector.emit_to(builder).into_raw(),
                self.element.emit_to(builder).into_raw(),
                self.index.emit_to(builder).into_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
pub fn insert_element<'a, V, E, I, N>(vector: V, element: E, index: I, name: N) -> InsertElement<'a>
where
    V: Into<AstNode<'a>>,
    E: Into<AstNode<'a>>,
    I: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    InsertElement::new(vector, element, index, name)
}

/// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
#[macro_export]
macro_rules! insert_element {
    ($vector: expr, $element: expr, $index: expr; $name: expr) => {
        $crate::insts::insert_element($vector, $element, $index, $name)
    };
    ($vector: expr, $element: expr, $index: expr) => {
        insert_element!($vector, $element, $index ; "insert_element")
    };
}

/// This instruction constructs a fixed permutation of two input vectors.
#[derive(Clone, Debug, PartialEq)]
pub struct ShuffleVector<'a> {
    v1: Box<AstNode<'a>>,
    v2: Box<AstNode<'a>>,
    mask: Box<AstNode<'a>>,
    name: Cow<'a, str>,
}

impl<'a> ShuffleVector<'a> {
    pub fn new<V1, V2, M, N>(v1: V1, v2: V2, mask: M, name: N) -> Self
    where
        V1: Into<AstNode<'a>>,
        V2: Into<AstNode<'a>>,
        M: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        ShuffleVector {
            v1: Box::new(v1.into()),
            v2: Box::new(v2.into()),
            mask: Box::new(mask.into()),
            name: name.into(),
        }
    }
}

impl<'a> InstructionBuilder for ShuffleVector<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildShuffleVector(
                builder.as_raw(),
                self.v1.emit_to(builder).into_raw(),
                self.v2.emit_to(builder).into_raw(),
                self.mask.emit_to(builder).into_raw(),
                cstr!(self.name),
            )
        }.into()
    }
}

/// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
/// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
pub fn shuffle_vector<'a, V1, V2, M, N>(v1: V1, v2: V2, mask: M, name: N) -> ShuffleVector<'a>
where
    V1: Into<AstNode<'a>>,
    V2: Into<AstNode<'a>>,
    M: Into<AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    ShuffleVector::new(v1, v2, mask, name)
}

/// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
/// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
#[macro_export]
macro_rules! shuffle_vector {
    ($v1: expr, $v2: expr, $mask: expr; $name: expr) => {
        $crate::insts::shuffle_vector($v1, $v2, $mask, $name)
    };
    ($v1: expr, $v2: expr, $mask: expr) => {
        shuffle_vector!($v1, $v2, $mask ; "shuffle_vector")
    };
}

impl IRBuilder {
    /// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
    pub fn extract_element<'a, V, I, N>(&self, vector: V, index: I, name: N) -> Instruction
    where
        V: Into<AstNode<'a>>,
        I: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        extract_element(vector, index, name).emit_to(self)
    }

    /// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
    pub fn insert_element<'a, V, E, I, N>(&self, vector: V, element: E, index: I, name: N) -> Instruction
    where
        V: Into<AstNode<'a>>,
        E: Into<AstNode<'a>>,
        I: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        insert_element(vector, element, index, name).emit_to(self)
    }

    /// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
    /// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
    pub fn shuffle_vector<'a, V1, V2, M, N>(&self, v1: V1, v2: V2, mask: M, name: N) -> Instruction
    where
        V1: Into<AstNode<'a>>,
        V2: Into<AstNode<'a>>,
        M: Into<AstNode<'a>>,
        N: Into<Cow<'a, str>>,
    {
        shuffle_vector(v1, v2, mask, name).emit_to(self)
    }
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
        let function_type = FunctionType::new(context.void_t(), types![i64_t.vector_t(3), i64_t.vector_t(3)], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let arg0_vector = function.get_param(0).unwrap();
        let arg1_vector = function.get_param(1).unwrap();

        let idx = i64_t.int(1);

        assert_eq!(
            extract_element!(arg0_vector, idx).emit_to(&builder).to_string().trim(),
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
