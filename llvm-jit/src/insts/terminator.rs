use std::borrow::Cow;

use llvm::core::*;
use llvm::prelude::*;

use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::{AsLLVMBool, unchecked_cstring};
use value::{AsValueRef, Function, Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct LandingPad<'a> {
    result_ty: TypeRef,
    personality_fn: Function,
    name: Cow<'a, str>,
    clauses: Vec<ValueRef>,
    cleanup: bool,
}

impl<'a> LandingPad<'a> {
    pub fn new(result_ty: TypeRef, personality_fn: Function, name: Cow<'a, str>) -> Self {
        LandingPad {
            result_ty,
            personality_fn,
            name,
            clauses: vec![],
            cleanup: false,
        }
    }

    /// Add a catch or filter clause to the landingpad instruction
    pub fn add_clause(mut self, clause: ValueRef) -> Self {
        self.clauses.push(clause);
        self
    }

    pub fn set_cleanup(mut self, cleanup: bool) -> Self {
        self.cleanup = cleanup;
        self
    }
}

impl<'a> InstructionBuilder for LandingPad<'a> {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        let clauses = self.clauses
            .iter()
            .map(|clause| clause.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let landing_pad: Instruction = unsafe {
            LLVMBuildLandingPad(
                builder.as_raw(),
                self.result_ty.as_raw(),
                self.personality_fn.as_raw(),
                clauses.len() as u32,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into();

        for clause in &self.clauses {
            unsafe { LLVMAddClause(landing_pad.as_raw(), clause.as_raw()) }
        }

        unsafe { LLVMSetCleanup(landing_pad.as_raw(), self.cleanup.as_bool()) }

        landing_pad
    }
}

/// The `resume` instruction is a terminator instruction that has no successors.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Resume(ValueRef);

impl Resume {
    pub fn new(result: ValueRef) -> Self {
        Resume(result)
    }
}

impl InstructionBuilder for Resume {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe { LLVMBuildResume(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// The `resume` instruction is a terminator instruction that has no successors.
#[macro_export]
macro_rules! resume {
    ($result:expr) => ({
        $crate::insts::Resume::new($result.into())
    })
}

/// The `unreachable` instruction has no defined semantics.
///
/// This instruction is used to inform the optimizer that a particular portion of the code is not reachable.
/// This can be used to indicate that the code after a no-return function cannot be reached, and other facts.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe { LLVMBuildUnreachable(builder.as_raw()) }.into()
    }
}

#[cfg(test)]
mod tests {
    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;
    use value::*;

    #[test]
    fn terminator() {
        let context = Context::new();
        let module = Module::with_name_in_context("invoke", &context);
        let builder = IRBuilder::within_context(&context);

        let fn_test = module.add_function("test", FunctionType::new(context.void_t(), &[], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i64t = context.int64_t();

        assert_eq!(
            resume!(i64t.uint(123)).emit_to(&builder).to_string().trim(),
            "resume i64 123"
        );
        assert_eq!(
            Unreachable.emit_to(&builder).to_string().trim(),
            "unreachable"
        );
    }
}