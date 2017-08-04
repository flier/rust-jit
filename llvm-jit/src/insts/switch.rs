use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use insts::{IRBuilder, InstructionBuilder};
use value::{AsValueRef, Instruction, ValueRef};

/// Create a switch instruction with the specified value, default dest,
/// and with a hint for the number of cases that will be added (for efficient allocation).
#[derive(Clone, Debug, PartialEq)]
pub struct Switch {
    v: ValueRef,
    dest: Option<BasicBlock>,
    cases: Vec<(ValueRef, BasicBlock)>,
}

impl Switch {
    pub fn on(v: ValueRef) -> Self {
        Switch {
            v: v,
            dest: None,
            cases: vec![],
        }
    }

    /// Add a case to the switch instruction
    pub fn case(mut self, on: ValueRef, dest: BasicBlock) -> Self {
        self.cases.push((on, dest));
        self
    }

    pub fn default(mut self, dest: BasicBlock) -> Self {
        self.dest = Some(dest);
        self
    }
}

impl InstructionBuilder for Switch {
    type Target = SwitchInst;

    fn emit_to(&self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let switch: SwitchInst = unsafe {
            LLVMBuildSwitch(
                builder.as_raw(),
                self.v.as_raw(),
                self.dest.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.cases.len() as u32,
            )
        }.into();

        for &(on, dest) in &self.cases {
            unsafe { LLVMAddCase(switch.as_raw(), on.as_raw(), dest.as_raw()) }
        }

        switch
    }
}

/// The switch instruction specifies a table of values and destinations.
///
/// When the `switch` instruction is executed, this table is searched for the given value.
/// If the value is found, control flow is transferred to the corresponding destination;
/// otherwise, control flow is transferred to the default destination.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SwitchInst(Instruction);

inherit_from!(SwitchInst, Instruction, ValueRef, LLVMValueRef);

impl SwitchInst {
    /// Obtain the default destination basic block of a switch instruction.
    pub fn default_dest(&self) -> BasicBlock {
        unsafe { LLVMGetSwitchDefaultDest(self.as_raw()) }.into()
    }
}

/// The `switch` instruction is used to transfer control flow to one of several different places.
#[macro_export]
macro_rules! switch {
    ($cond:expr; _ => $default:expr , $( $on:expr => $dest:expr ),*) => ({
        $crate::insts::Switch::on($cond.into()).default($default) $( .case($on.into(), $dest) )*
    });
    ($cond:expr; $( $on:expr => $dest:expr ),*) => ({
        $crate::insts::Switch::on($cond.into()) $( .case($on.into(), $dest) )*
    });
}

#[cfg(test)]
mod tests {
    use insts::*;
    use prelude::*;

    #[test]
    fn switch() {
        let context = Context::new();
        let module = context.create_module("switch");
        let builder = context.create_builder();

        let function_type = FunctionType::new(context.void_t(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i64_t = context.int64_t();
        let bb_default = function.append_basic_block_in_context("default", &context);

        let switch = switch!(i64_t.uint(3);
                _ => bb_default,
                i64_t.uint(1) => function.append_basic_block_in_context("one", &context),
                i64_t.uint(2) => function.append_basic_block_in_context("two", &context),
                i64_t.uint(3) => function.append_basic_block_in_context("three", &context)
            ).emit_to(&builder);

        assert_eq!(
            switch.to_string().trim(),
            r#"switch i64 3, label %default [
    i64 1, label %one
    i64 2, label %two
    i64 3, label %three
  ]"#
        );
        assert_eq!(switch.default_dest(), bb_default);
    }
}
