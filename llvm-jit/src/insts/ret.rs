use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use insts::{AstNode, IRBuilder, InstructionBuilder, TerminatorInst};
use utils::{AsRaw, IntoRaw};
use value::{Instruction, ValueRef};

/// Create a 'ret' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct Ret<'a>(Return<'a>);

impl<'a> Ret<'a> {
    pub fn void() -> Self {
        Ret(Return::Void)
    }

    pub fn new<V>(value: V) -> Self
    where
        V: Into<AstNode<'a>>,
    {
        Ret(Return::Value(Box::new(value.into())))
    }

    pub fn aggregate<I, V>(values: I) -> Self
    where
        I: IntoIterator<Item = V>,
        V: Into<AstNode<'a>>,
    {
        Ret(Return::Aggregate(values.into_iter().map(|v| v.into()).collect()))
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Return<'a> {
    Void,
    Value(Box<AstNode<'a>>),
    Aggregate(Vec<AstNode<'a>>),
}

impl<'a> InstructionBuilder for Ret<'a> {
    type Target = ReturnInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        match self.0 {
            Return::Void => unsafe { LLVMBuildRetVoid(builder.as_raw()) }.into(),
            Return::Value(value) => unsafe { LLVMBuildRet(builder.as_raw(), value.emit_to(builder).into_raw()) }.into(),
            Return::Aggregate(values) => {
                let mut values = values
                    .into_iter()
                    .map(|v| v.emit_to(builder).into_raw())
                    .collect::<Vec<_>>();

                unsafe { LLVMBuildAggregateRet(builder.as_raw(), values.as_mut_ptr(), values.len() as u32) }.into()
            }
        }
    }
}

/// Every basic block in a program ends with a “Terminator” instruction,
/// which indicates which block should be executed after the current block is finished.
///
/// These terminator instructions typically yield a `void` value: they produce control flow,
/// not values (the one exception being the `invoke` instruction).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ReturnInst(Instruction);

inherit_from!(ReturnInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst for ReturnInst {}

impl ReturnInst {
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
        $crate::insts::Ret::void()
    };
    ($result:expr) => {
        $crate::insts::Ret::new($result)
    };
    ($( $result:expr ),*) => {
        $crate::insts::Ret::aggregate(vec![$( $crate::insts::AstNode::from($result) ),*])
    }
}

impl IRBuilder {
    /// The ‘ret‘ instruction is used to return control flow (and optionally a value)
    /// from a function back to the caller.
    pub fn ret_void(&self) -> ReturnInst {
        Ret::void().emit_to(self)
    }

    /// The ‘ret‘ instruction is used to return control flow (and optionally a value)
    /// from a function back to the caller.
    pub fn ret<'a, T>(&self, result: T) -> ReturnInst
    where
        T: Into<AstNode<'a>>,
    {
        Ret::new(result.into()).emit_to(self)
    }

    /// The ‘ret‘ instruction is used to return control flow (and optionally a value)
    /// from a function back to the caller.
    pub fn aggregate_ret<'a, I, V>(&self, values: I) -> ReturnInst
    where
        I: IntoIterator<Item = V>,
        V: Into<AstNode<'a>>,
    {
        Ret::aggregate(values).emit_to(self)
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
        let ret = context.struct_t(&[i64_t, f64_t], false);
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
