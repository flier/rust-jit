use std::borrow::Cow;
use std::ptr;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::function::Function;
use crate::insts::{AstNode, IRBuilder, InstructionBuilder};
use crate::types::TypeRef;
use crate::utils::{AsLLVMBool, AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

/// Subclasses of this class are all able to terminate a basic
/// block. Thus, these are all the flow control type of operations.
pub trait TerminatorInst {}

#[derive(Clone, Debug, PartialEq)]
pub struct LandingPad<'a> {
    result_ty: TypeRef,
    personality_fn: Option<Function>,
    name: Cow<'a, str>,
    clauses: Vec<ValueRef>,
    cleanup: bool,
}

impl<'a> LandingPad<'a> {
    pub fn new<T, N>(result_ty: T, personality_fn: Option<Function>, name: N, cleanup: bool) -> Self
    where
        T: Into<TypeRef>,
        N: Into<Cow<'a, str>>,
    {
        LandingPad {
            result_ty: result_ty.into(),
            personality_fn,
            name: name.into(),
            clauses: vec![],
            cleanup,
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

        let landingpad: LandingPadInst = unsafe {
            LLVMBuildLandingPad(
                builder.as_raw(),
                self.result_ty.into_raw(),
                self.personality_fn.map_or(ptr::null_mut(), |f| f.into_raw()),
                self.clauses.len() as u32,
                cstr!(self.name),
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

impl TerminatorInst for LandingPadInst {}

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

/// The `landingpad` instruction is used by LLVM’s exception handling system
/// to specify that a basic block is a landing pad — one where the exception lands,
/// and corresponds to the code found in the catch portion of a try/catch sequence.
/// It defines values supplied by the personality function upon re-entry to the function.
/// The resultval has the type resultty.
pub fn landing_pad<'a, T, N>(result_ty: T, personality_fn: Option<Function>, name: N, cleanup: bool) -> LandingPad<'a>
where
    T: Into<TypeRef>,
    N: Into<Cow<'a, str>>,
{
    LandingPad::new(result_ty, personality_fn, name, cleanup)
}

#[macro_export]
macro_rules! landing_pad {
    ($ty: expr, $personality: expr; $name: expr) => {
        $crate::insts::landing_pad($ty, Some($personality), $name, false)
    };
    ($ty: expr; $name: expr) => {
        $crate::insts::landing_pad($ty, None, $name, false)
    };
    ($ty: expr, $personality: expr) => {
        landing_pad!($ty, $personality; "landingpad")
    };
    ($ty: expr) => {
        landing_pad!($ty; "landingpad")
    };
}

/// The `resume` instruction is a terminator instruction that has no successors.
#[derive(Clone, Debug, PartialEq)]
pub struct Resume<'a>(Box<AstNode<'a>>);

impl<'a> Resume<'a> {
    pub fn new<V>(result: V) -> Self
    where
        V: Into<AstNode<'a>>,
    {
        Resume(Box::new(result.into()))
    }
}

impl<'a> InstructionBuilder for Resume<'a> {
    type Target = ResumeInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildResume(builder.as_raw(), self.0.emit_to(builder).into_raw()) }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ResumeInst(Instruction);

inherit_from!(ResumeInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst for ResumeInst {}

/// The `resume` instruction is a terminator instruction that has no successors.
pub fn resume<'a, V>(result: V) -> Resume<'a>
where
    V: Into<AstNode<'a>>,
{
    Resume::new(result)
}

#[macro_export]
macro_rules! resume {
    ($result: expr) => {
        $crate::insts::resume($result)
    };
}

/// The `unreachable` instruction is used to inform the optimizer that a particular portion of the code is not reachable.
///
/// This can be used to indicate that the code after a no-return function cannot be reached, and other facts.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    type Target = UnreachableInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildUnreachable(builder.as_raw()) }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UnreachableInst(Instruction);

inherit_from!(UnreachableInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst for UnreachableInst {}

/// The `unreachable` instruction is used to inform the optimizer that a particular portion of the code is not reachable.
pub fn unreachable() -> Unreachable {
    Unreachable
}

impl IRBuilder {
    /// The `landingpad` instruction is used by LLVM’s exception handling system
    /// to specify that a basic block is a landing pad — one where the exception lands,
    /// and corresponds to the code found in the catch portion of a try/catch sequence.
    /// It defines values supplied by the personality function upon re-entry to the function.
    /// The resultval has the type resultty.
    pub fn landing_pad<'a, T, N>(
        &self,
        result_ty: T,
        personality_fn: Option<Function>,
        name: N,
        cleanup: bool,
    ) -> LandingPadInst
    where
        T: Into<TypeRef>,
        N: Into<Cow<'a, str>>,
    {
        landing_pad(result_ty, personality_fn, name, cleanup).emit_to(self)
    }

    /// The `resume` instruction is a terminator instruction that has no successors.
    pub fn resume<'a, V>(&self, result: V) -> ResumeInst
    where
        V: Into<AstNode<'a>>,
    {
        resume(result).emit_to(self)
    }

    /// The `unreachable` instruction is used to inform the optimizer that a particular portion of the code is not reachable.
    pub fn unreachable(&self) -> UnreachableInst {
        unreachable().emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::insts::*;
    use crate::prelude::*;

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
            resume!(i64_t.uint(123)).emit_to(&builder).to_string().trim(),
            "resume i64 123"
        );
        assert_eq!(unreachable().emit_to(&builder).to_string().trim(), "unreachable");
    }
}
