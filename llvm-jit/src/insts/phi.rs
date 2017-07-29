use std::borrow::Cow;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::unchecked_cstring;
use value::{AsValueRef, Instruction, ValueRef};

/// The ‘phi‘ instruction is used to implement the φ node in the SSA graph representing the function.
#[derive(Clone, Debug, PartialEq)]
pub struct Phi<'a> {
    ty: TypeRef,
    incomings: Vec<(ValueRef, BasicBlock)>,
    name: Cow<'a, str>,
}

impl<'a> Phi<'a> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Phi {
            ty,
            incomings: Vec::new(),
            name,
        }
    }

    pub fn add_incoming(mut self, value: ValueRef, block: BasicBlock) -> Self {
        self.incomings.push((value, block));
        self
    }
}

impl<'a> InstructionBuilder for Phi<'a> {
    type Target = PhiNode;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        let phi: PhiNode = unsafe {
            LLVMBuildPhi(
                builder.as_raw(),
                self.ty.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into();

        let (mut values, mut blocks) = self.incomings.iter().fold(
            (Vec::new(), Vec::new()),
            |(mut values, mut blocks),
             &(value, block)| {
                values.push(value.as_raw());
                blocks.push(block.as_raw());
                (values, blocks)
            },
        );

        unsafe {
            LLVMAddIncoming(
                phi.as_raw(),
                values.as_mut_ptr(),
                blocks.as_mut_ptr(),
                self.incomings.len() as u32,
            )
        }

        phi
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PhiNode(Instruction);

inherit_from!(PhiNode, Instruction, ValueRef, LLVMValueRef);

impl PhiNode {
    /// Add an incoming values to the end of a PHI list.
    pub fn add_incomings(&self, incomings: &[(ValueRef, BasicBlock)]) -> &Self {
        let (mut values, mut blocks) = incomings.iter().fold(
            (Vec::new(), Vec::new()),
            |(mut values, mut blocks),
             &(value, block)| {
                values.push(value.as_raw());
                blocks.push(block.as_raw());
                (values, blocks)
            },
        );

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

#[macro_export]
macro_rules! phi {
    ($ty:expr, $([ $value:expr, $block:expr ])* ; $name:expr ) => ({
        $crate::insts::Phi::new($ty.into(), $name.into()) $( .add_incoming( $value.into(), $block.into() ) )*
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
    fn phi() {
        let context = Context::new();
        let module = Module::with_name_in_context("phi", &context);
        let builder = IRBuilder::within_context(&context);

        let i64_t = context.int64_t();
        let p_i64_t = i64_t.ptr_t();

        let function_type = FunctionType::new(context.void_t(), &[p_i64_t.into()], false);
        let function = module.add_function("test", function_type);

        let bb_loop_header = function.append_basic_block_in_context("LoopHeader", &context);

        let bb_loop = function.append_basic_block_in_context("Loop", &context);
        builder.position(Position::AtEnd(bb_loop));

        let indvar = phi!(i64_t, [i64_t.int(0), bb_loop_header]; "indvar").emit_to(&builder);
        let nextindvar = add!(indvar, i64_t.int(1); "nextindvar").emit_to(&builder);

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
            vec![(i64_t.int(0), bb_loop_header), (nextindvar.into(), bb_loop)]
        );
    }
}
