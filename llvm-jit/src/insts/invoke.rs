use std::borrow::Cow;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use function::Function;
use insts::{CallSite, IRBuilder, InstructionBuilder};
use utils::unchecked_cstring;
use value::{AsValueRef, Instruction, ValueRef};

/// This instruction is designed to operate as a standard `call` instruction in most regards.
///
/// The primary difference is that it establishes an association with a label,
/// which is used by the runtime library to unwind the stack.
///
/// This instruction is used in languages with destructors to ensure
/// that proper cleanup is performed in the case of either a longjmp or a thrown exception.
/// Additionally, this is important for implementation of `catch` clauses in high-level languages that support them.
#[derive(Clone, Debug, PartialEq)]
pub struct Invoke<'a> {
    func: Function,
    args: Vec<ValueRef>,
    then: Option<BasicBlock>,
    unwind: Option<BasicBlock>,
    name: Cow<'a, str>,
}

impl<'a> Invoke<'a> {
    pub fn call(func: Function, args: Vec<ValueRef>, name: Cow<'a, str>) -> Self {
        Invoke {
            func: func,
            args: args,
            then: None,
            unwind: None,
            name: name,
        }
    }

    pub fn then(mut self, dest: BasicBlock) -> Self {
        self.then = Some(dest);
        self
    }

    pub fn unwind(mut self, dest: BasicBlock) -> Self {
        self.unwind = Some(dest);
        self
    }
}

impl<'a> InstructionBuilder for Invoke<'a> {
    type Target = InvokeInst;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        let mut args = self.args
            .iter()
            .map(|arg| arg.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe {
            LLVMBuildInvoke(
                builder.as_raw(),
                self.func.as_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.unwind.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InvokeInst(Instruction);

inherit_from!(InvokeInst, Instruction, ValueRef, LLVMValueRef);

impl CallSite for InvokeInst {}

impl InvokeInst {
    /// Return the normal destination basic block.
    pub fn normal_dest(&self) -> BasicBlock {
        unsafe { LLVMGetNormalDest(self.as_raw()) }.into()
    }

    /// Set the normal destination basic block.
    pub fn set_normal_dest(&self, dest: BasicBlock) {
        unsafe { LLVMSetNormalDest(self.as_raw(), dest.as_raw()) }
    }

    /// Return the unwind destination basic block.
    pub fn unwind_dest(&self) -> BasicBlock {
        unsafe { LLVMGetUnwindDest(self.as_raw()) }.into()
    }

    /// Set the unwind destination basic block.
    pub fn set_unwind_dest(&self, dest: BasicBlock) {
        unsafe { LLVMSetUnwindDest(self.as_raw(), dest.as_raw()) }
    }
}

/// The `invoke` instruction causes control to transfer to a specified function,
/// with the possibility of control flow transfer to either the `normal` label or the `exception` label.
#[macro_export]
macro_rules! invoke {
    ($func:expr, $( $arg:expr ),* ; to $then:expr ; unwind $unwind:expr ; $name:expr) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).then($then).unwind($unwind)
    });
    ($func:expr, $( $arg:expr ),* ; to $then:expr ; $name:expr) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).then($then)
    });
    ($func:expr, $( $arg:expr ),* ; unwind $unwind:expr ; $name:expr) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).unwind($unwind)
    });
    ($func:expr, $( $arg:expr ),* ; $name:expr) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into())
    });

    ($func:expr, $( $arg:expr ),* ; to $then:expr ; unwind $unwind:expr) => ({
        invoke!($func, $( $arg ),* ; to $then ; unwind $unwind ; "invoke")
    });
    ($func:expr, $( $arg:expr ),* ; to $then:expr) => ({
        invoke!($func, $( $arg ),* ; to $then ; "invoke")
    });
    ($func:expr, $( $arg:expr ),* ; unwind $unwind:expr) => ({
        invoke!($func, $( $arg ),* ; unwind $unwind ; "invoke")
    });
    ($func:expr, $( $arg:expr ),*) => ({
        invoke!($func, $( $arg ),* ; "invoke")
    });
}

#[cfg(test)]
mod tests {
    use llvm::LLVMCallConv;

    use context::Context;
    use function::FunctionType;
    use insts::*;
    use module::Module;
    use prelude::*;
    use types::*;

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

        let bb_normal = fn_test.append_basic_block_in_context("normal", &context);
        let bb_unwind = fn_test.append_basic_block_in_context("catch", &context);

        let invoke =
            invoke!(fn_hello, i64_t.uint(123), i64_t.int(456); to bb_normal; unwind bb_unwind)
                .emit_to(&builder);

        assert_eq!(
            invoke.to_string().trim(),
            r#"%invoke = invoke i64 @hello(i64 123, i64 456)
          to label %normal unwind label %catch"#
        );

        assert_eq!(invoke.argn(), 2);
        assert!(matches!(invoke.call_conv(), LLVMCallConv::LLVMCCallConv));
        assert_eq!(invoke.called(), fn_hello);
        assert_eq!(invoke.normal_dest(), bb_normal);
        assert_eq!(invoke.unwind_dest(), bb_unwind);
    }
}
