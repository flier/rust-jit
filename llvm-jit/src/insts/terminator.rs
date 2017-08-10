use std::borrow::Cow;
use std::fmt;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use function::Function;
use insts::{IRBuilder, InstructionBuilder};
use types::TypeRef;
use utils::{AsLLVMBool, unchecked_cstring};
use value::{AsValueRef, Instruction, ValueRef};

#[derive(Clone, Debug, PartialEq)]
pub struct LandingPad<'a> {
    result_ty: TypeRef,
    personality_fn: Option<Function>,
    name: Cow<'a, str>,
    clauses: Vec<ValueRef>,
    cleanup: bool,
}

impl<'a> LandingPad<'a> {
    pub fn new(
        result_ty: TypeRef,
        personality_fn: Option<Function>,
        name: Cow<'a, str>,
        cleanup: bool,
    ) -> Self {
        LandingPad {
            result_ty,
            personality_fn,
            name,
            clauses: vec![],
            cleanup: cleanup,
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
    type Target = LandingPadInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let clauses = self.clauses
            .iter()
            .map(|clause| clause.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        let landingpad: LandingPadInst = unsafe {
            LLVMBuildLandingPad(
                builder.as_raw(),
                self.result_ty.as_raw(),
                self.personality_fn.map_or(ptr::null_mut(), |f| f.as_raw()),
                clauses.len() as u32,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into();

        for clause in &self.clauses {
            unsafe { LLVMAddClause(landingpad.as_raw(), clause.as_raw()) }
        }

        unsafe { LLVMSetCleanup(landingpad.as_raw(), self.cleanup.as_bool()) }

        landingpad
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LandingPadInst(Instruction);

inherit_from!(LandingPadInst, Instruction, ValueRef, LLVMValueRef);

impl LandingPadInst {
    pub fn add_clause<V: Into<ValueRef>>(&self, clause: V) -> &Self {
        unsafe { LLVMAddClause(self.as_raw(), clause.into().as_raw()) };
        self
    }

    pub fn set_cleanup(&self, cleanup: bool) -> &Self {
        unsafe { LLVMSetCleanup(self.as_raw(), cleanup.as_bool()) };
        self
    }
}

#[macro_export]
macro_rules! landing_pad {
    ($ty:expr, $personality:expr; $name:expr) => (
        $crate::insts::LandingPad::new($ty.into(), Some($personality.into()), $name.into(), false)
    );
    ($ty:expr; $name:expr) => (
        $crate::insts::LandingPad::new($ty.into(), None, $name.into(), false)
    );
    ($ty:expr, $personality:expr) => (
        landing_pad!($ty, $personality; "landingpad")
    );
    ($ty:expr) => (
        landing_pad!($ty; "landingpad")
    );
}

/// The `resume` instruction is a terminator instruction that has no successors.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Resume<V>(V);

impl<V> Resume<V> {
    pub fn new(result: V) -> Self {
        Resume(result)
    }
}

impl<V> InstructionBuilder for Resume<V>
where
    V: InstructionBuilder + fmt::Debug,
{
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildResume(builder.as_raw(), self.0.emit_to(builder).into().as_raw()) }.into()
    }
}

/// The `resume` instruction is a terminator instruction that has no successors.
pub fn resume<V>(result: V) -> Resume<V> {
    Resume::new(result)
}

#[macro_export]
macro_rules! resume {
    ($result:expr) => (
        $crate::insts::Resume::new($result)
    )
}

/// The `unreachable` instruction is used to inform the optimizer that a particular portion of the code is not reachable.
///
/// This can be used to indicate that the code after a no-return function cannot be reached, and other facts.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildUnreachable(builder.as_raw()) }.into()
    }
}

/// The `unreachable` instruction is used to inform the optimizer that a particular portion of the code is not reachable.
pub fn unreachable() -> Unreachable {
    Unreachable
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn terminator() {
        let context = Context::new();
        let module = context.create_module("terminator");
        let builder = context.create_builder();

        let fn_test = module.add_function("test", FunctionType::new(context.void_t(), &[], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        let i64_t = context.int64_t();

        assert_eq!(
            resume!(i64_t.uint(123))
                .emit_to(&builder)
                .to_string()
                .trim(),
            "resume i64 123"
        );
        assert_eq!(
            unreachable().emit_to(&builder).to_string().trim(),
            "unreachable"
        );
    }
}
