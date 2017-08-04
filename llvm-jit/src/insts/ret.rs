use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use insts::{IRBuilder, InstructionBuilder};
use value::{AsValueRef, Instruction, ValueRef};

/// Create a 'ret void' instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RetVoid;

impl InstructionBuilder for RetVoid {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildRetVoid(builder.as_raw()) }.into()
    }
}

/// Create a 'ret <val>' instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ret(ValueRef);

impl Ret {
    pub fn new(ret: ValueRef) -> Self {
        Ret(ret)
    }
}

impl InstructionBuilder for Ret {
    type Target = TerminatorInst;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildRet(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// Create a sequence of N insertvalue instructions,
/// with one Value from the retVals array each, that build a aggregate
/// return value one value at a time, and a ret instruction to return
/// the resulting aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct AggregateRet(Vec<ValueRef>);

impl AggregateRet {
    pub fn new(results: Vec<ValueRef>) -> Self {
        AggregateRet(results)
    }
}

impl InstructionBuilder for AggregateRet {
    type Target = TerminatorInst;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let mut values = self.0
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMBuildAggregateRet(builder.as_raw(), values.as_mut_ptr(), values.len() as u32) }
            .into()
    }
}

/// Every basic block in a program ends with a “Terminator” instruction,
/// which indicates which block should be executed after the current block is finished.
///
/// These terminator instructions typically yield a `void` value: they produce control flow,
/// not values (the one exception being the `invoke` instruction).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TerminatorInst(Instruction);

inherit_from!(TerminatorInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst {
    /// Return the successors that this terminator has.
    pub fn successors(&self) -> Vec<BasicBlock> {
        let count = unsafe { LLVMGetNumSuccessors(self.as_raw()) };

        (0..count)
            .map(|idx| unsafe { LLVMGetSuccessor(self.as_raw(), idx).into() })
            .collect()
    }

    /// Update the specified successor to point at the provided block.
    pub fn set_successor(&self, idx: u32, block: BasicBlock) {
        unsafe { LLVMSetSuccessor(self.as_raw(), idx, block.as_raw()) }
    }
}

/// The `ret` instruction is used to return control flow (and optionally a value) from a function back to the caller.
#[macro_export]
macro_rules! ret {
    () => {
        $crate::insts::RetVoid
    };
    ($result:expr) => {
        $crate::insts::Ret::new($result.into())
    };
    ($( $result:expr ),*) => {
        $crate::insts::AggregateRet::new(vec![$( $result.into() ),*])
    }
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;
    use types::*;

    #[test]
    fn ret_void() {
        let context = Context::new();
        let module = context.create_module("ret_void");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        assert_eq!(ret!().emit_to(&builder).to_string().trim(), "ret void");
    }

    #[test]
    fn ret() {
        let context = Context::new();
        let module = context.create_module("ret");
        let builder = context.create_builder();

        let i64_t = context.int64_t();
        let function_type = FunctionType::new(i64_t, &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        assert_eq!(
            ret!(i64_t.uint(123)).emit_to(&builder).to_string().trim(),
            "ret i64 123"
        );
    }

    #[test]
    fn aggregate_ret() {
        let context = Context::new();
        let module = context.create_module("aggregate_ret");
        let builder = context.create_builder();

        let i64_t = context.int64_t();
        let f64_t = context.double_t();
        let ret = context.anonymous_struct_t(&[i64_t, f64_t], false);
        let function_type = FunctionType::new(ret.into(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        assert_eq!(
            ret!(i64_t.uint(123), f64_t.real(456f64))
                .emit_to(&builder)
                .to_string()
                .trim(),
            "ret { i64, double } { i64 123, double 4.560000e+02 }"
        );
    }
}
