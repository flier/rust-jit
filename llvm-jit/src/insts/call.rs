use std::borrow::Cow;
use std::mem;

use llvm::LLVMCallConv;
use llvm::core::*;
use llvm::prelude::*;

use insts::{IRBuilder, InstructionBuilder};
use utils::{AsBool, AsLLVMBool, unchecked_cstring};
use value::{AsValueRef, Function, Instruction, ValueRef};

/// This instruction is designed to operate as a standard `call` instruction in most regards.
///
/// The primary difference is that it establishes an association with a label,
/// which is used by the runtime library to unwind the stack.
///
/// This instruction is used in languages with destructors to ensure
/// that proper cleanup is performed in the case of either a longjmp or a thrown exception.
/// Additionally, this is important for implementation of `catch` clauses in high-level languages that support them.
#[derive(Clone, Debug, PartialEq)]
pub struct Call<'a> {
    func: Function,
    args: Vec<ValueRef>,
    name: Cow<'a, str>,
}

impl<'a> Call<'a> {
    pub fn new(func: Function, args: Vec<ValueRef>, name: Cow<'a, str>) -> Self {
        Call {
            func: func,
            args: args,
            name: name,
        }
    }
}

impl<'a> InstructionBuilder for Call<'a> {
    type Target = CallInst;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        let mut args = self.args
            .iter()
            .map(|arg| arg.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe {
            LLVMBuildCall(
                builder.as_raw(),
                self.func.as_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

pub type CallConv = LLVMCallConv;

pub trait CallSite: AsValueRef {
    /// Obtain the argument count for a call instruction.
    fn argn(&self) -> usize {
        unsafe { LLVMGetNumArgOperands(self.as_raw()) as usize }
    }

    /// Obtain the calling convention for a call instruction.
    fn call_conv(&self) -> CallConv {
        unsafe { mem::transmute(LLVMGetInstructionCallConv(self.as_raw())) }
    }

    /// Set the calling convention for a call instruction.
    fn set_call_conv(&self, cc: CallConv) {
        unsafe { LLVMSetInstructionCallConv(self.as_raw(), cc as u32) }
    }

    /// Obtain the pointer to the function invoked by this instruction.
    fn called(&self) -> Function {
        unsafe { LLVMGetCalledValue(self.as_raw()) }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CallInst(Instruction);

inherit_from!(CallInst, Instruction, ValueRef, LLVMValueRef);

impl CallSite for CallInst {}

impl CallInst {
    /// Obtain whether a call instruction is a tail call.
    pub fn is_tail_call(&self) -> bool {
        unsafe { LLVMIsTailCall(self.as_raw()) }.as_bool()
    }

    /// Set whether a call instruction is a tail call.
    pub fn set_tail_call(&self, is_tail_call: bool) {
        unsafe { LLVMSetTailCall(self.as_raw(), is_tail_call.as_bool()) }
    }
}

#[macro_export]
macro_rules! call {
    ($func:expr, $( $arg:expr ),*; $name:expr) => ({
        $crate::insts::Call::new($func.into(), vec![ $( $arg.into() ),* ], $name.into())
    });
}

#[cfg(test)]
mod tests {
    use llvm::LLVMCallConv;

    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;
    use value::*;

    #[test]
    fn invoke() {
        let context = Context::new();
        let module = Module::with_name_in_context("invoke", &context);
        let builder = IRBuilder::within_context(&context);

        let i64_t = context.int64_t();
        let fn_test = module.add_function("test", FunctionType::new(context.void_t(), &[], false));
        let fn_hello =
            module.add_function("hello", FunctionType::new(i64_t, &[i64_t, i64_t], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let call = call!(fn_hello, i64_t.uint(123), i64_t.int(456); "call").emit_to(&builder);

        assert_eq!(
            call.to_string().trim(),
            "%call = call i64 @hello(i64 123, i64 456)"
        );
        assert_eq!(call.argn(), 2);
        assert!(matches!(call.call_conv(), LLVMCallConv::LLVMCCallConv));
        assert_eq!(call.called(), fn_hello);
        assert!(!call.is_tail_call());
    }
}
