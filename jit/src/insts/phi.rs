use std::borrow::Cow;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::block::BasicBlock;
use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::types::TypeRef;
use crate::utils::{AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

/// The `phi` instruction is used to implement the φ node in the SSA graph representing the function.
#[derive(Clone, Debug, PartialEq)]
pub struct Phi<'a> {
    ty: TypeRef,
    incomings: Vec<(AstNode<'a>, BasicBlock)>,
    name: Cow<'a, str>,
}

impl<'a> Phi<'a> {
    pub fn new<T, N>(ty: T, name: N) -> Self
    where
        T: Into<TypeRef>,
        N: Into<Cow<'a, str>>,
    {
        Phi {
            ty: ty.into(),
            incomings: vec![],
            name: name.into(),
        }
    }

    pub fn add_incoming<V, B>(mut self, value: V, block: B) -> Self
    where
        V: Into<AstNode<'a>>,
        B: Into<BasicBlock>,
    {
        self.incomings.push((value.into(), block.into()));
        self
    }
}

impl<'a> InstructionBuilder for Phi<'a> {
    type Target = PhiNode;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let phi: PhiNode = unsafe { LLVMBuildPhi(builder.as_raw(), self.ty.into_raw(), cstr!(self.name)) }.into();
        let count = self.incomings.len();

        let (mut values, mut blocks) =
            self.incomings
                .into_iter()
                .fold((Vec::new(), Vec::new()), |(mut values, mut blocks), (value, block)| {
                    values.push(value.emit_to(builder).into_raw());
                    blocks.push(block.into_raw());
                    (values, blocks)
                });

        unsafe { LLVMAddIncoming(phi.as_raw(), values.as_mut_ptr(), blocks.as_mut_ptr(), count as u32) }

        phi
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PhiNode(Instruction);

inherit_from!(PhiNode, Instruction, ValueRef; LLVMValueRef);

impl PhiNode {
    pub fn add_incoming<V: Into<ValueRef>, B: Into<BasicBlock>>(&self, value: V, block: B) -> &Self {
        self.add_incomings(&[(value.into(), block.into())])
    }

    /// Add an incoming values to the end of a PHI list.
    pub fn add_incomings(&self, incomings: &[(ValueRef, BasicBlock)]) -> &Self {
        let (mut values, mut blocks) =
            incomings
                .iter()
                .fold((Vec::new(), Vec::new()), |(mut values, mut blocks), &(value, block)| {
                    values.push(value.as_raw());
                    blocks.push(block.as_raw());
                    (values, blocks)
                });

        unsafe {
            LLVMAddIncoming(
                self.as_raw(),
                values.as_mut_ptr(),
                blocks.as_mut_ptr(),
                incomings.len() as u32,
            )
        }
        self
    }

    /// Obtain incoming values to a PHI node.
    pub fn incomings(&self) -> Vec<(ValueRef, BasicBlock)> {
        let count = unsafe { LLVMCountIncoming(self.as_raw()) };

        (0..count)
            .map(|idx| unsafe {
                (
                    LLVMGetIncomingValue(self.as_raw(), idx).into(),
                    LLVMGetIncomingBlock(self.as_raw(), idx).into(),
                )
            })
            .collect()
    }
}

/// The `phi` instruction is used to implement the φ node in the SSA graph representing the function.
pub fn phi<'a, T, N>(ty: T, name: N) -> Phi<'a>
where
    T: Into<TypeRef>,
    N: Into<Cow<'a, str>>,
{
    Phi::new(ty, name)
}

#[macro_export]
macro_rules! phi {
    ( $ty:expr, $( $value:expr => $block:expr ),* ; $name:expr ) => ({
        $crate::insts::phi($ty, $name) $( .add_incoming( $value, $block ) )*
    });
    ( $ty:expr, $( $value:expr => $block:expr ),* ) => ({
        phi!( $ty, $( $value => $block ),* ; "phi" )
    });
    ( $ty:expr; $name:expr ) => ({
        $crate::insts::phi($ty, $name)
    });
    ( $ty:expr ) => ({
        phi!( $ty ; "phi" )
    })
}

impl IRBuilder {
    /// The `phi` instruction is used to implement the φ node in the SSA graph representing the function.
    pub fn phi<'a, T, N>(&self, ty: T, name: N) -> PhiNode
    where
        T: Into<TypeRef>,
        N: Into<Cow<'a, str>>,
    {
        phi(ty, name).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

    #[test]
    fn phi() {
        let context = Context::new();
        let module = context.create_module("phi");
        let builder = context.create_builder();

        let i64_t = context.int64_t();
        let p_i64_t = i64_t.ptr_t();

        let function_type = FunctionType::new(context.void_t(), &[p_i64_t.into()], false);
        let function = module.add_function("test", function_type);

        let bb_loop_header = function.append_basic_block_in_context("LoopHeader", &context);

        let bb_loop = function.append_basic_block_in_context("Loop", &context);
        builder.position(Position::AtEnd(bb_loop));

        let indvar = phi!(i64_t, i64_t.int(0) => bb_loop_header; "indvar").emit_to(&builder);
        let nextindvar = add(indvar, i64_t.int(1), "nextindvar").emit_to(&builder);

        br!(bb_loop).emit_to(&builder);

        indvar.add_incomings(&[(nextindvar.into(), bb_loop)]);

        assert_eq!(
            bb_loop.last_instructions(4),
            vec![
                "%indvar = phi i64 [ 0, %LoopHeader ], [ %nextindvar, %Loop ]",
                "%nextindvar = add i64 %indvar, 1",
                "br label %Loop",
            ]
        );

        assert_eq!(
            indvar.incomings(),
            vec![(i64_t.int(0).into(), bb_loop_header), (nextindvar.into(), bb_loop)]
        );
    }
}
