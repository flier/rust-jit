use std::fmt;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use insts::{IRBuilder, InstructionBuilder};
use utils::{AsBool, AsRaw, FromRaw};
use value::{AsValueRef, Instruction, ValueRef};

/// Create an unconditional `br label X` instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Br(BasicBlock);

impl Br {
    pub fn new(dest: BasicBlock) -> Self {
        Br(dest)
    }
}

impl InstructionBuilder for Br {
    type Target = BranchInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildBr(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// Create a conditional `br cond, trueDest, falseDest` instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct CondBr<T> {
    cond: T,
    then: Option<BasicBlock>,
    or_else: Option<BasicBlock>,
}

impl<T> CondBr<T> {
    pub fn new(cond: T, then: Option<BasicBlock>, or_else: Option<BasicBlock>) -> CondBr<T> {
        CondBr {
            cond,
            then,
            or_else,
        }
    }

    pub fn on(cond: T) -> Self {
        CondBr {
            cond,
            then: None,
            or_else: None,
        }
    }

    pub fn then(mut self, dest: BasicBlock) -> Self {
        self.then = Some(dest);
        self
    }

    pub fn or_else(mut self, dest: BasicBlock) -> Self {
        self.or_else = Some(dest);
        self
    }
}

impl<T> InstructionBuilder for CondBr<T>
where
    T: InstructionBuilder + fmt::Debug,
{
    type Target = BranchInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildCondBr(
                builder.as_raw(),
                self.cond.emit_to(builder).into().as_raw(),
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.or_else.map_or(ptr::null_mut(), |bb| bb.as_raw()),
            )
        }.into()
    }
}

/// Create an indirect branch instruction with the specified address operand,
/// with an optional hint for the number of destinations that will be added (for efficient allocation).
#[derive(Clone, Debug, PartialEq)]
pub struct IndirectBr<T> {
    addr: T,
    dests: Vec<BasicBlock>,
}

impl<T> IndirectBr<T> {
    /// The `indirectbr` instruction implements an indirect branch to a label within the current function, whose address is specified by “address”.
    pub fn new<I>(addr: T, dests: I) -> Self
    where
        I: IntoIterator<Item = BasicBlock>,
    {
        IndirectBr {
            addr,
            dests: dests.into_iter().collect(),
        }
    }

    pub fn on(addr: T) -> Self {
        IndirectBr {
            addr,
            dests: vec![],
        }
    }

    /// Add a destination to the indirectbr instruction
    pub fn jump_to(mut self, dest: BasicBlock) -> Self {
        self.dests.push(dest);
        self
    }
}

impl<T> InstructionBuilder for IndirectBr<T>
where
    T: InstructionBuilder + fmt::Debug,
{
    type Target = BranchInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let br: BranchInst = unsafe {
            LLVMBuildIndirectBr(
                builder.as_raw(),
                self.addr.emit_to(builder).into().as_raw(),
                self.dests.len() as u32,
            )
        }.into();

        for dest in &self.dests {
            unsafe { LLVMAddDestination(br.as_raw(), dest.as_raw()) }
        }

        br
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BranchInst(Instruction);

inherit_from!(BranchInst, Instruction, ValueRef, LLVMValueRef);

impl BranchInst {
    /// Return if a branch is conditional.
    pub fn is_cond(&self) -> bool {
        unsafe { LLVMIsConditional(self.as_raw()) }.as_bool()
    }

    /// Return the condition of a branch instruction.
    pub fn get_cond(&self) -> Option<ValueRef> {
        unsafe { LLVMGetCondition(self.as_raw()) }.wrap()
    }

    /// Set the condition of a branch instruction.
    pub fn set_cond<V: AsValueRef>(&self, cond: &V) {
        unsafe { LLVMSetCondition(self.as_raw(), cond.as_raw()) }
    }
}

/// The `br` instruction is used to cause control flow to transfer to a different basic block in the current function.
#[macro_export]
macro_rules! br {
    ($dest:expr) => (
        $crate::insts::Br::new($dest.into())
    );
    ($addr:expr => [$( $dest:expr ),*]) => (
        $crate::insts::IndirectBr::on($addr) $( .jump_to($dest.into()) )*
    );
    ($cond:expr => $then:expr) => (
         $crate::insts::CondBr::new($cond, Some($then.into()), None)
    );
    ($cond:expr => $then:expr, _ => $or_else:expr) => (
        $crate::insts::CondBr::new($cond, Some($then.into()), Some($or_else.into()))
    );
}

impl IRBuilder {
    /// The `br` instruction is used to cause control flow to transfer to a different basic block in the current function.
    pub fn br(&self, dest: BasicBlock) -> BranchInst {
        Br::new(dest).emit_to(self)
    }

    /// The conditional branch form of the `br` instruction takes a single `i1` value and two `label` values.
    pub fn cond_br<T>(&self, cond: T, then: Option<BasicBlock>, or_else: Option<BasicBlock>) -> BranchInst
    where
        T: InstructionBuilder + fmt::Debug,
    {
        CondBr::new(cond, then, or_else).emit_to(self)
    }

    /// The `indirectbr` instruction implements an indirect branch to a label within the current function, whose address is specified by “address”.
    pub fn indirect_br<T, I>(&self, addr: T, dests: I) -> BranchInst
    where
        T: InstructionBuilder + fmt::Debug,
        I: IntoIterator<Item = BasicBlock>,
    {
        IndirectBr::new(addr, dests).emit_to(self)
    }
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;
    use types::*;

    #[test]
    fn br() {
        let context = Context::new();
        let module = context.create_module("br");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        // unconditional branch
        let next = function.append_basic_block_in_context("next", &context);

        assert_eq!(
            br!(next).emit_to(&builder).to_string().trim(),
            "br label %next"
        );
    }

    #[test]
    fn cond_br() {
        let context = Context::new();
        let module = context.create_module("cond_br");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        // conditional branch
        let bb_then = function.append_basic_block_in_context("then", &context);
        let bb_else = function.append_basic_block_in_context("else", &context);
        let bool_t = context.int1_t();

        assert_eq!(
            br!(bool_t.uint(1) => bb_then, _ => bb_else)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "br i1 true, label %then, label %else"
        );

        assert_eq!(
            CondBr::on(bool_t.uint(1))
                .then(bb_then)
                .or_else(bb_else)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "br i1 true, label %then, label %else"
        );
    }

    #[test]
    fn indirect_br() {
        let context = Context::new();
        let module = context.create_module("indirect_br");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position_at_end(bb);

        // indirect branch
        let bb_then = function.append_basic_block_in_context("then", &context);
        let bb_else = function.append_basic_block_in_context("else", &context);

        assert_eq!(
            br!(bb_then.addr() => [bb_then, bb_else])
                .emit_to(&builder)
                .to_string()
                .trim(),
            "indirectbr i8* blockaddress(@test, %then), [label %then, label %else]"
        );
    }
}
