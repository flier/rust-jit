use std::borrow::Cow;
use std::fmt;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use utils::unchecked_cstring;
use value::{Function, Instruction, ValueRef};

/// An instruction builder represents a point within a basic block
/// and is the exclusive means of building instructions.
#[derive(Debug)]
pub struct IRBuilder(LLVMBuilderRef);

#[derive(Debug)]
pub enum Position {
    To(BasicBlock, Instruction),
    Before(Instruction),
    AtEnd(BasicBlock),
}

pub trait InstructionBuilder {
    fn build(&self, builder: &IRBuilder) -> Instruction;
}

/// Create a 'ret void' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct RetVoid;

impl InstructionBuilder for RetVoid {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe { LLVMBuildRetVoid(builder.as_raw()) })
    }
}

/// Create a 'ret <val>' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct Ret(ValueRef);

impl Ret {
    pub fn new(ret: ValueRef) -> Self {
        Ret(ret)
    }
}

impl InstructionBuilder for Ret {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe { LLVMBuildRet(builder.as_raw(), self.0.as_raw()) })
    }
}

/// Create a sequence of N insertvalue instructions,
/// with one Value from the retVals array each, that build a aggregate
/// return value one value at a time, and a ret instruction to return
/// the resulting aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct AggregateRet(Vec<ValueRef>);

impl AggregateRet {
    pub fn new(results: Vec<ValueRef>) -> Self {
        AggregateRet(results)
    }
}

impl InstructionBuilder for AggregateRet {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        let mut values = self.0
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        Instruction::from_raw(unsafe {
            LLVMBuildAggregateRet(builder.as_raw(), values.as_mut_ptr(), values.len() as u32)
        })
    }
}

#[macro_export]
macro_rules! ret {
    () => {
        $crate::ops::RetVoid
    };
    ($result:expr) => {
        $crate::ops::Ret::new($result.into())
    };
    ($( $result:expr ),*) => {
        $crate::ops::AggregateRet::new(vec![$( $result.into() ),*])
    }
}

/// Create an unconditional 'br label X' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct Br(BasicBlock);

impl Br {
    pub fn new(dest: BasicBlock) -> Self {
        Br(dest)
    }
}

impl InstructionBuilder for Br {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe { LLVMBuildBr(builder.as_raw(), self.0.as_raw()) })
    }
}

/// Create a conditional 'br Cond, TrueDest, FalseDest' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct CondBr {
    cond: ValueRef,
    then: Option<BasicBlock>,
    _else: Option<BasicBlock>,
}

impl CondBr {
    pub fn new(cond: ValueRef, then: Option<BasicBlock>, _else: Option<BasicBlock>) -> CondBr {
        CondBr {
            cond: cond,
            then: then,
            _else: _else,
        }
    }

    pub fn on(cond: ValueRef) -> Self {
        CondBr {
            cond: cond,
            then: None,
            _else: None,
        }
    }

    pub fn then(mut self, dest: BasicBlock) -> Self {
        self.then = Some(dest);
        self
    }

    pub fn _else(mut self, dest: BasicBlock) -> Self {
        self._else = Some(dest);
        self
    }
}

impl InstructionBuilder for CondBr {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe {
            LLVMBuildCondBr(
                builder.as_raw(),
                self.cond.as_raw(),
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self._else.map_or(ptr::null_mut(), |bb| bb.as_raw()),
            )
        })
    }
}

#[macro_export]
macro_rules! br {
    ($dest:expr) => (
        $crate::ops::Br::new($dest.into())
    );
    ($cond:expr => $then:expr) => (
        $crate::ops::CondBr::on($cond.into()).then($then.into())
    );
    ($cond:expr => $then:expr, _ => $else:expr) => (
        $crate::ops::CondBr::on($cond.into()).then($then.into())._else($else.into())
    );
}

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
    fn build(&self, builder: &IRBuilder) -> Instruction {
        let switch = Instruction::from_raw(unsafe {
            LLVMBuildSwitch(
                builder.as_raw(),
                self.v.as_raw(),
                self.dest.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.cases.len() as u32,
            )
        });

        for &(on, dest) in &self.cases {
            unsafe { LLVMAddCase(switch.as_raw(), on.as_raw(), dest.as_raw()) }
        }

        switch
    }
}

#[macro_export]
macro_rules! switch {
    ($cond:expr; _ => $default:expr , $( $on:expr => $dest:expr ),*) => ({
        $crate::ops::Switch::on($cond).default($default) $( .case($on, $dest) )*
    });
    ($cond:expr; $( $on:expr => $dest:expr ),*) => ({
        $crate::ops::Switch::on($cond) $( .case($on, $dest) )*
    });
}

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
    fn build(&self, builder: &IRBuilder) -> Instruction {
        let mut args = self.args
            .iter()
            .map(|arg| arg.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        Instruction::from_raw(unsafe {
            LLVMBuildInvoke(
                builder.as_raw(),
                self.func.as_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.unwind.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        })
    }
}

#[macro_export]
macro_rules! call {
    ($func:expr => $name:expr; to $then:expr; unwind $unwind:expr; [ $( $arg:expr ),* ]) => ({
        $crate::ops::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).then($then).unwind($unwind)
    });
    ($func:expr => $name:expr; to $then:expr; [ $( $arg:expr ),* ]) => ({
        $crate::ops::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).then($then)
    });
    ($func:expr => $name:expr; unwind $unwind:expr; [ $( $arg:expr ),* ]) => ({
        $crate::ops::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into()).unwind($unwind)
    });
    ($func:expr => $name:expr; [ $( $arg:expr ),* ]) => ({
        $crate::ops::Invoke::call($func.into(), vec![ $( $arg.into() ),* ], $name.into())
    });
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe { LLVMBuildUnreachable(builder.as_raw()) })
    }
}

/*
#[derive(Debug)]
pub struct BinOp<'a> {
    op: LLVMOpcode,
    lhs: ValueRef,
    rhs: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> BinOp<'a> {
    pub fn new(op: LLVMOpcode, lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        BinOp {
            op: op,
            lhs: lhs,
            rhs: rhs,
            name: name,
        }
    }
}

impl<'a> InstructionBuilder for BinOp<'a> {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe {
            LLVMBuildBinOp(
                builder.as_raw(),
                self.op,
                self.lhs.as_raw(),
                self.rhs.as_raw(),
                unchecked_cstring(self.name).as_ptr(),
            )
        })
    }
}
*/

macro_rules! define_unary_instruction {
    ($operator:ident, $func:path) => (
        #[derive(Clone, Debug, PartialEq)]
        pub struct $operator<'a> {
            value: ValueRef,
            name: Cow<'a, str>,
        }

        impl<'a> $operator<'a> {
            pub fn new(value: ValueRef, name: Cow<'a, str>) -> Self {
                $operator {
                    value: value,
                    name: name,
                }
            }
        }

        impl<'a> $crate::builder::InstructionBuilder for $operator<'a> {
            fn build(&self, builder: &IRBuilder) -> Instruction {
                Instruction::from_raw(unsafe {
                    $func(
                        builder.as_raw(),
                        self.value.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                })
            }
        }
    );

    ($operator:ident, $func:path, $alias:ident) => (
        define_unary_instruction!($operator, $func);

        #[macro_export]
        macro_rules! $alias {
            ($value:expr, $name:expr) => { $crate::ops::$operator::new($value.into(), $name.into()) }
        }
    );
}

macro_rules! define_binary_operator {
    ($operator:ident, $func:path) => (
        #[derive(Clone, Debug, PartialEq)]
        pub struct $operator<'a> {
            lhs: ValueRef,
            rhs: ValueRef,
            name: Cow<'a, str>,
        }

        impl<'a> $operator<'a> {
            pub fn new(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
                $operator {
                    lhs: lhs,
                    rhs: rhs,
                    name: name,
                }
            }
        }

        impl<'a> $crate::builder::InstructionBuilder for $operator<'a> {
            fn build(&self, builder: &IRBuilder) -> Instruction {
                Instruction::from_raw(unsafe {
                    $func(
                        builder.as_raw(),
                        self.lhs.as_raw(),
                        self.rhs.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                })
            }
        }
    );

    ($operator:ident, $func:path, $alias:ident) => (
        define_binary_operator!($operator, $func);

        #[macro_export]
        macro_rules! $alias {
            ($lhs:expr, $rhs:expr, $name:expr) => {
                $crate::ops::$operator::new($lhs.into(), $rhs.into(), $name.into())
            }
        }
    )
}

define_binary_operator!(Add, LLVMBuildAdd, add);
define_binary_operator!(NSWAdd, LLVMBuildNSWAdd);
define_binary_operator!(NUWAdd, LLVMBuildNUWAdd);
define_binary_operator!(FAdd, LLVMBuildFAdd, fadd);
define_binary_operator!(Sub, LLVMBuildSub, sub);
define_binary_operator!(NSWSub, LLVMBuildNSWSub);
define_binary_operator!(NUWSub, LLVMBuildNUWSub);
define_binary_operator!(FSub, LLVMBuildFSub, fsub);
define_binary_operator!(Mul, LLVMBuildMul, mul);
define_binary_operator!(NSWMul, LLVMBuildNSWMul);
define_binary_operator!(NUWMul, LLVMBuildNUWMul);
define_binary_operator!(FMul, LLVMBuildFMul, fmul);
define_binary_operator!(UDiv, LLVMBuildUDiv, udiv);
define_binary_operator!(ExactUDiv, LLVMBuildExactUDiv);
define_binary_operator!(SDiv, LLVMBuildSDiv, sdiv);
define_binary_operator!(ExactSDiv, LLVMBuildExactSDiv);
define_binary_operator!(FDiv, LLVMBuildFDiv, fdiv);
define_binary_operator!(URem, LLVMBuildURem, urem);
define_binary_operator!(SRem, LLVMBuildSRem, srem);
define_binary_operator!(FRem, LLVMBuildFRem, frem);
define_binary_operator!(Shl, LLVMBuildShl, shl);
define_binary_operator!(LShr, LLVMBuildLShr, lshr);
define_binary_operator!(AShr, LLVMBuildAShr, ashr);
define_binary_operator!(And, LLVMBuildAnd, and);
define_binary_operator!(Or, LLVMBuildOr, or);
define_binary_operator!(Xor, LLVMBuildXor, xor);

define_unary_instruction!(Neg, LLVMBuildNeg, neg);
define_unary_instruction!(NSWNeg, LLVMBuildNSWNeg);
define_unary_instruction!(NUWNeg, LLVMBuildNUWNeg);
define_unary_instruction!(FNeg, LLVMBuildFNeg, fneg);
define_unary_instruction!(Not, LLVMBuildNot, not);

impl IRBuilder {
    /// Create a new IR builder in the global context.
    pub fn new() -> Self {
        let builder = unsafe { LLVMCreateBuilder() };

        trace!("create builder in global context: IRBuilder({:?})", builder);

        IRBuilder(builder)
    }

    /// Create a new IR builder in a specific context.
    pub fn within_context(context: &Context) -> Self {
        let builder = unsafe { LLVMCreateBuilderInContext(context.as_raw()) };

        trace!("create builder in {:?}: IRBuilder({:?})", context, builder);

        IRBuilder(builder)
    }

    pub fn as_raw(&self) -> LLVMBuilderRef {
        self.0
    }

    pub fn insert_block(&self) -> Option<BasicBlock> {
        unsafe { LLVMGetInsertBlock(self.0).as_mut() }.map(|block| BasicBlock::from_raw(block))
    }

    pub fn clear_insertion_position(&self) {
        unsafe { LLVMClearInsertionPosition(self.0) }
    }

    /// This specifies that created instructions should be inserted at the specified point.
    pub fn position(&self, position: Position) -> &Self {
        trace!("{:?} move position {:?}", self, position);

        unsafe {
            match position {
                Position::To(block, instr) => {
                    LLVMPositionBuilder(self.0, block.as_raw(), instr.as_raw())
                }
                Position::Before(instr) => LLVMPositionBuilderBefore(self.0, instr.as_raw()),
                Position::AtEnd(block) => LLVMPositionBuilderAtEnd(self.0, block.as_raw()),
            }
        }

        &self
    }

    pub fn emit<I: InstructionBuilder + fmt::Debug>(&self, inst: I) -> Instruction {
        trace!("{:?} emit: {:?}", self, inst);

        inst.build(self)
    }
}

impl Drop for IRBuilder {
    fn drop(&mut self) {
        trace!("drop {:?}", self);

        unsafe { LLVMDisposeBuilder(self.0) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use module::Module;
    use types::*;
    use value::*;

    #[test]
    fn ret_void() {
        let context = Context::new();
        let module = Module::with_name_in_context("ret_void", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let inst = builder.emit(ret!());

        assert!(!inst.as_raw().is_null());
        assert_eq!(inst.to_string().trim(), "ret void");
    }

    #[test]
    fn ret() {
        let context = Context::new();
        let module = Module::with_name_in_context("ret", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let function_type = FunctionType::new(i64t, &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let inst = builder.emit(ret!(i64t.uint(123)));

        assert!(!inst.as_raw().is_null());
        assert_eq!(inst.to_string().trim(), "ret i64 123");
    }

    #[test]
    fn aggregate_ret() {
        let context = Context::new();
        let module = Module::with_name_in_context("aggregate_ret", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let f64t = context.double();
        let ret = context.structure(&[i64t, f64t], false);
        let function_type = FunctionType::new(ret.into(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let inst = builder.emit(ret!(i64t.uint(123), f64t.real(456f64)));

        assert!(!inst.as_raw().is_null());
        assert_eq!(
            inst.to_string().trim(),
            "ret { i64, double } { i64 123, double 4.560000e+02 }"
        );
    }

    #[test]
    fn br() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let next = function.append_basic_block_in_context("next", &context);
        let inst = br!(next);

        assert_eq!(builder.emit(inst).to_string().trim(), "br label %next");

        let bb_then = function.append_basic_block_in_context("then", &context);
        let bb_else = function.append_basic_block_in_context("else", &context);
        let bool_t = context.int1();
        let inst = br!(bool_t.uint(1) => bb_then, _ => bb_else);

        assert_eq!(
            builder.emit(inst).to_string().trim(),
            "br i1 true, label %then, label %else"
        );

        let inst = CondBr::on(bool_t.uint(1)).then(bb_then)._else(bb_else);

        assert_eq!(
            builder.emit(inst).to_string().trim(),
            "br i1 true, label %then, label %else"
        );
    }

    #[test]
    fn switch() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i64t = context.int64();

        let inst =
            switch!(i64t.uint(3);
                _ => function.append_basic_block_in_context("default", &context),
                i64t.uint(1) => function.append_basic_block_in_context("one", &context),
                i64t.uint(2) => function.append_basic_block_in_context("two", &context),
                i64t.uint(3) => function.append_basic_block_in_context("three", &context)
            );

        assert_eq!(
            builder.emit(inst).to_string().trim(),
            r#"switch i64 3, label %default [
    i64 1, label %one
    i64 2, label %two
    i64 3, label %three
  ]"#
        );
    }

    #[test]
    fn invoke() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let fn_test = module.add_function("test", FunctionType::new(context.void(), &[], false));
        let fn_hello = module.add_function("hello", FunctionType::new(i64t, &[i64t, i64t], false));

        let bb = fn_test.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let bb_normal = fn_test.append_basic_block_in_context("normal", &context);
        let bb_catch = fn_test.append_basic_block_in_context("catch", &context);

        let inst =
            call!(fn_hello => "ret"; to bb_normal; unwind bb_catch; [i64t.uint(123), i64t.int(456)]);

        assert_eq!(
            builder.emit(inst).to_string().trim(),
            r#"%ret = invoke i64 @hello(i64 123, i64 456)
          to label %normal unwind label %catch"#
        );
    }
}
