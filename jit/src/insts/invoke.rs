use std::borrow::Cow;
use std::ptr;

use crate::llvm::core::*;
use crate::llvm::prelude::*;
use crate::llvm::LLVMCallConv;

use crate::block::BasicBlock;
use crate::function::Function;
use crate::insts::{AstNode, CallSite, IRBuilder, InstructionBuilder};
use crate::utils::{AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

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
    args: Vec<AstNode<'a>>,
    then: Option<BasicBlock>,
    unwind: Option<BasicBlock>,
    name: Cow<'a, str>,
    call_conv: Option<LLVMCallConv>,
}

/// The ‘invoke‘ instruction causes control to transfer to a specified function,
/// with the possibility of control flow transfer to either the ‘normal‘ label or the ‘exception‘ label.
pub fn invoke<'a, F, I, N>(func: F, args: I, name: N) -> Invoke<'a>
where
    F: Into<Function>,
    I: IntoIterator<Item = AstNode<'a>>,
    N: Into<Cow<'a, str>>,
{
    Invoke {
        func: func.into(),
        args: args.into_iter().collect(),
        then: None,
        unwind: None,
        name: name.into(),
        call_conv: None,
    }
}

impl<'a> Invoke<'a> {
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

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let mut args = self
            .args
            .into_iter()
            .map(|arg| arg.emit_to(builder).into_raw())
            .collect::<Vec<LLVMValueRef>>();

        let invoke: Self::Target = unsafe {
            LLVMBuildInvoke(
                builder.as_raw(),
                self.func.as_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.unwind.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                cstr!(self.name),
            )
        }
        .into();

        if let Some(call_conv) = self.call_conv {
            invoke.set_call_conv(call_conv);
        }

        invoke
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InvokeInst(Instruction);

inherit_from!(InvokeInst, Instruction, ValueRef; LLVMValueRef);

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
    (__impl $invoke:ident to label $then:ident $($rest:tt)* ) => {{
        #[allow(unused_mut)]
        let mut invoke = $invoke.then($then);

        invoke!(__impl invoke $($rest)*)
    }};
    (__impl $invoke:ident unwind label $unwind:ident $($rest:tt)* ) => {{
        #[allow(unused_mut)]
        let mut invoke = $invoke.unwind($unwind);

        invoke!(__impl invoke $($rest)*)
    }};
    (__impl $invoke:expr ; $name:expr) => {{
        $invoke.name = $name.into();
        $invoke
    }};
    (__impl $invoke:expr) => {{
        $invoke
    }};
    (__call $func:ident ( $( $arg:expr ),* ) $($rest:tt)*) => ({
        #[allow(unused_mut)]
        let mut invoke = $crate::insts::invoke($func, vec![ $( $arg.into() ),* ], "invoke");

        invoke!(__impl invoke $($rest)*)
    });
    (__call_conv $call_conv:expr, $($rest:tt)*) => {{
        let mut invoke = invoke!( $($rest)* );
        invoke.call_conv = Some($call_conv);
        invoke
    }};

    // The C calling convention
    (ccc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMCCallConv, $($rest)*)
    };
    // The fast calling convention
    (fastcc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMFastCallConv, $($rest)*)
    };
    // The cold calling convention
    (coldcc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMColdCallConv, $($rest)*)
    };
    // GHC convention
    (cc10 $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMGHCCallConv, $($rest)*)
    };
    // The HiPE calling convention
    (cc11 $($rest:tt)*) => {
        invoke!(__call_conv crate::llvm::LLVMCallConv::LLVMHiPECallConv, $($rest)*)
    };
    // WebKit’s JavaScript calling convention
    (webkit_jscc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMWebKitJSCallConv, $($rest)*)
    };
    // Dynamic calling convention for code patching
    (anyregcc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMAnyRegCallConv, $($rest)*)
    };
    // The PreserveMost calling convention
    (preserve_mostcc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMPreserveMostCallConv, $($rest)*)
    };
    // The PreserveAll calling convention
    (preserve_allcc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMPreserveAllCallConv, $($rest)*)
    };
    // This calling convention is used for Swift language.
    (swiftcc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMSwiftCallConv, $($rest)*)
    };
    // The CXX_FAST_TLS calling convention for access functions
    (cxx_fast_tlscc $($rest:tt)*) => {
        invoke!(__call_conv ::llvm::LLVMCallConv::LLVMCXXFASTTLSCallConv, $($rest)*)
    };

    ($func:ident ( $( $arg:expr ),* ) $($rest:tt)*) => ({
        #[allow(unused_mut)]
        let mut invoke = $crate::insts::invoke($func, vec![ $( $arg.into() ),* ], "invoke");

        invoke!(__impl invoke $($rest)*)
    });
}

#[cfg(test)]
mod tests {
    use crate::llvm::LLVMCallConv;

    use crate::insts::*;
    use crate::prelude::*;

    #[test]
    fn invoke() {
        let context = Context::new();
        let module = context.create_module("invoke");

        let i32_t = context.int32_t();
        let i64_t = context.int64_t();

        let fn_hello = module.add_function("hello", FunctionType::new(i64_t, &[i64_t, i64_t], false));
        let entry_bb = fn_hello.append_basic_block_in_context("entry", &context);
        context.create_builder().within(entry_bb, || {
            let arg0 = fn_hello.param(0).unwrap();
            let arg1 = fn_hello.param(1).unwrap();

            ret!(add!(arg0, arg1))
        });
        fn_hello.verify().unwrap();

        let fn_frame_handler = module.add_function("frame_handler", FunctionType::new(i32_t, &[], false));
        let entry_bb = fn_frame_handler.append_basic_block_in_context("entry", &context);
        context.create_builder().within(entry_bb, || ret!(i32_t.uint(0)));
        fn_frame_handler.verify().unwrap();

        let fn_test = module.add_function("test", FunctionType::new(context.void_t(), &[], false));
        fn_test.set_personality_function(fn_frame_handler);

        let entry_bb = fn_test.append_basic_block_in_context("entry", &context);
        let normal_bb = fn_test.append_basic_block_in_context("normal", &context);
        let unwind_bb = fn_test.append_basic_block_in_context("unwind", &context);
        let catch_bb = fn_test.append_basic_block_in_context("catch", &context);

        let builder = context.create_builder();

        let invoke = builder.within(entry_bb, || {
            let arg0 = i64_t.uint(123);
            let arg1 = i64_t.int(456);

            invoke!(cc11 fn_hello(arg0, arg1) to label normal_bb unwind label unwind_bb ; "hello")
        });

        assert_eq!(
            invoke.to_string().trim(),
            r#"%hello = invoke cc11 i64 @hello(i64 123, i64 456)
          to label %normal unwind label %unwind"#
        );

        assert_eq!(invoke.argn(), 2);
        assert_eq!(invoke.call_conv(), LLVMCallConv::LLVMHiPECallConv);
        assert_eq!(invoke.called(), fn_hello);
        assert_eq!(invoke.normal_dest(), normal_bb);
        assert_eq!(invoke.unwind_dest(), unwind_bb);

        builder.within(normal_bb, || ret!());
        let catchswitch_inst = builder.within(
            unwind_bb,
            || catchswitch!(within none [label catch_bb] unwind to caller),
        );
        let catchpad_inst = builder.within(catch_bb, || catchpad!(within catchswitch_inst []));
        builder.within(catch_bb, || catchret!(from catchpad_inst to label normal_bb));

        fn_test.verify().unwrap();

        module.verify().unwrap();
    }
}
