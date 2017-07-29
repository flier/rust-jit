use std::borrow::Cow;
use std::ffi::CStr;

use llvm::core::*;
use llvm::prelude::*;

use function::Function;
use insts::TerminatorInst;
use value::{BlockAddress, Instruction, ValueRef};

/// Basic Block
///
/// A basic block represents a single entry single exit section of code.
/// Basic blocks contain a list of instructions which form the body of the block.
///
/// Basic blocks belong to functions. They have the type of label.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BasicBlock(LLVMBasicBlockRef);

inherit_from!(BasicBlock, LLVMBasicBlockRef);

impl BasicBlock {
    /// Obtain the string name of a basic block.
    pub fn name(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetBasicBlockName(self.0)).to_string_lossy() }
    }

    /// Obtain the function to which a basic block belongs.
    pub fn parent(&self) -> Function {
        unsafe { LLVMGetBasicBlockParent(self.0) }.into()
    }

    /// Computes the address of the specified basic block in the specified function
    pub fn addr(&self) -> BlockAddress {
        unsafe { LLVMBlockAddress(self.parent().as_raw(), self.as_raw()) }.into()
    }

    /// Obtain the terminator instruction for a basic block.
    pub fn terminator(&self) -> Option<TerminatorInst> {
        unsafe { LLVMGetBasicBlockTerminator(self.0).as_mut() }.map(|v| TerminatorInst::from_raw(v))
    }

    /// Convert a basic block instance to a value type.
    pub fn as_value(&self) -> ValueRef {
        unsafe { LLVMBasicBlockAsValue(self.0) }.into()
    }

    /// Remove a basic block from a function and delete it.
    ///
    /// This deletes the basic block from its containing function and deletes the basic block itself.
    pub fn delete(self) {
        unsafe { LLVMDeleteBasicBlock(self.0) }
    }

    /// Remove a basic block from a function.
    ///
    /// This deletes the basic block from its containing function but keep the basic block alive.
    pub fn remove_from_parent(&self) {
        unsafe { LLVMRemoveBasicBlockFromParent(self.0) }
    }

    /// Move a basic block to before another one.
    pub fn move_before(&self, pos: BasicBlock) {
        unsafe { LLVMMoveBasicBlockBefore(self.0, pos.0) }
    }

    /// Move a basic block to after another one.
    pub fn move_after(&self, pos: BasicBlock) {
        unsafe { LLVMMoveBasicBlockAfter(self.0, pos.0) }
    }

    /// Obtain an iterator to the instructions in a basic block.
    pub fn instructions(&self) -> InstrIter {
        InstrIter::new(self.0)
    }

    #[cfg(test)]
    pub fn last_instructions(&self, n: usize) -> Vec<String> {
        let mut insts = self.instructions()
            .rev()
            .take(n)
            .map(|i| i.to_string().trim().to_owned())
            .collect::<Vec<String>>();

        insts.reverse();

        insts
    }
}

impl_iter!(
    InstrIter,
    LLVMGetFirstInstruction | LLVMGetLastInstruction[LLVMBasicBlockRef],
    LLVMGetNextInstruction | LLVMGetPreviousInstruction[LLVMValueRef],
    Instruction::from_raw
);

#[cfg(test)]
mod tests {
    use super::*;
    use context::Context;
    use function::FunctionType;
    use insts::{IRBuilder, Position, add};
    use module::Module;
    use prelude::*;

    #[test]
    fn basic_block() {
        // Set up a context, module and builder in that context.
        let context = Context::new();
        let module = Module::with_name_in_context("sum", &context);
        let builder = IRBuilder::within_context(&context);

        // get a type for sum function
        let i64_t = context.int64_t();
        let argts = [i64_t, i64_t, i64_t];
        let function_type = FunctionType::new(i64_t, &argts, false);

        // add it to our module
        let function = module.add_function("sum", function_type);

        // Create a basic block in the function and set our builder to generate code in it.
        let bb = function.append_basic_block_in_context("entry", &context);

        builder.position(Position::AtEnd(bb));

        // get the function's arguments
        let x = function.get_param(0).unwrap();
        let y = function.get_param(1).unwrap();
        let z = function.get_param(2).unwrap();

        let sum1 = builder.emit(add(x, y, "sum.1"));
        let sum2 = builder.emit(add(sum1, z, "sum.2"));

        // Emit a `ret` into the function
        let ret = builder.emit(ret!(sum2));

        assert!(!bb.as_raw().is_null());
        assert_eq!(bb.name(), "entry");
        assert_eq!(bb.parent(), function);
        assert_eq!(bb.terminator(), Some(ret));
        assert_eq!(
            bb.instructions().collect::<Vec<Instruction>>(),
            vec![sum1, sum2, ret.into()]
        );
    }
}
