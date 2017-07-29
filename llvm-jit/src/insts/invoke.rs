use std::borrow::Cow;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use insts::{IRBuilder, InstructionBuilder};
use utils::unchecked_cstring;
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
    type Target = Instruction;

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

/// The `invoke` instruction causes control to transfer to a specified function,
/// with the possibility of control flow transfer to either the `normal` label or the `exception` label.
#[macro_export]
macro_rules! invoke {
    ($func:expr => $name:expr; to $then:expr; unwind $unwind:expr; [ $( $arg:expr ),* ]) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).then($then).unwind($unwind)
    });
    ($func:expr => $name:expr; to $then:expr; [ $( $arg:expr ),* ]) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).then($then)
    });
    ($func:expr => $name:expr; unwind $unwind:expr; [ $( $arg:expr ),* ]) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).unwind($unwind)
    });
    ($func:expr => $name:expr; [ $( $arg:expr ),* ]) => ({
        $crate::insts::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into())
    });
}

#[cfg(test)]
mod tests {
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

        let i64t = context.int64();
        let fn_test = module.add_function("test", FunctionType::new(context.void(), &[], false));
        let fn_hello = module.add_function("hello", FunctionType::new(i64t, &[i64t, i64t], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let bb_normal = fn_test.append_basic_block_in_context("normal", &context);
        let bb_catch = fn_test.append_basic_block_in_context("catch", &context);

        assert_eq!(
            invoke!(fn_hello => "ret"; to bb_normal; unwind bb_catch; [i64t.uint(123), i64t.int(456)]).emit_to(&builder).to_string().trim(),
            r#"%ret = invoke i64 @hello(i64 123, i64 456)
          to label %normal unwind label %catch"#
        );
    }
}
