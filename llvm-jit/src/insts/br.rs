use std::ptr;

use llvm::core::*;

use block::BasicBlock;
use insts::{IRBuilder, InstructionBuilder};
use value::{AsValueRef, Instruction, ValueRef};

/// Create an unconditional 'br label X' instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Br(BasicBlock);

impl Br {
    pub fn new(dest: BasicBlock) -> Self {
        Br(dest)
    }
}

impl InstructionBuilder for Br {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe { LLVMBuildBr(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// Create a conditional 'br cond, trueDest, falseDest' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct CondBr {
    cond: ValueRef,
    then: Option<BasicBlock>,
    or_else: Option<BasicBlock>,
}

impl CondBr {
    pub fn new(cond: ValueRef, then: Option<BasicBlock>, or_else: Option<BasicBlock>) -> CondBr {
        CondBr {
            cond,
            then,
            or_else,
        }
    }

    pub fn on(cond: ValueRef) -> Self {
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

impl InstructionBuilder for CondBr {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        unsafe {
            LLVMBuildCondBr(
                builder.as_raw(),
                self.cond.as_raw(),
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.or_else.map_or(ptr::null_mut(), |bb| bb.as_raw()),
            )
        }.into()
    }
}

/// Create an indirect branch instruction with the specified address operand,
/// with an optional hint for the number of destinations that will be added (for efficient allocation).
#[derive(Clone, Debug, PartialEq)]
pub struct IndirectBr {
    addr: ValueRef,
    dests: Vec<BasicBlock>,
}

impl IndirectBr {
    pub fn new(addr: ValueRef, dests: Vec<BasicBlock>) -> Self {
        IndirectBr { addr, dests }
    }

    pub fn on(addr: ValueRef) -> Self {
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

impl InstructionBuilder for IndirectBr {
    type Target = Instruction;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        let br: Instruction = unsafe {
            LLVMBuildIndirectBr(
                builder.as_raw(),
                self.addr.as_raw(),
                self.dests.len() as u32,
            )
        }.into();

        for dest in &self.dests {
            unsafe { LLVMAddDestination(br.as_raw(), dest.as_raw()) }
        }

        br
    }
}

/// The `br` instruction is used to cause control flow to transfer to a different basic block in the current function.
#[macro_export]
macro_rules! br {
    ($dest:expr) => (
        $crate::insts::Br::new($dest.into())
    );
    ($addr:expr => [$( $dest:expr ),*]) => (
        $crate::insts::IndirectBr::on($addr.into()) $( .jump_to($dest.into()) )*
    );
    ($cond:expr => $then:expr) => (
        $crate::insts::CondBr::on($cond.into()).then($then.into())
    );
    ($cond:expr => $then:expr, _ => $or_else:expr) => (
        $crate::insts::CondBr::on($cond.into()).then($then.into()).or_else($or_else.into())
    );
}

#[cfg(test)]
mod tests {
    use context::Context;
    use insts::*;
    use module::Module;
    use types::*;
    use value::*;

    #[test]
    fn br() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

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
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        // conditional branch
        let bb_then = function.append_basic_block_in_context("then", &context);
        let bb_else = function.append_basic_block_in_context("else", &context);
        let bool_t = context.int1();

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
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

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
