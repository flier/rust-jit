use std::borrow::Cow;
use std::fmt;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use utils::unchecked_cstring;
use value::{Instruction, ValueRef};

/// An instruction builder represents a point within a basic block and is the exclusive means of building instructions.
#[derive(Debug)]
pub struct IRBuilder(LLVMBuilderRef);

#[derive(Debug)]
pub enum Position {
    To(BasicBlock, Instruction),
    Before(Instruction),
    AtEnd(BasicBlock),
}

pub trait InstructionBuilder {
    fn build(&self, builder: &IRBuilder) -> ValueRef;
}

/// Create a 'ret void' instruction.
#[derive(Debug)]
pub struct RetVoid;

pub fn ret_void() -> RetVoid {
    RetVoid
}

impl InstructionBuilder for RetVoid {
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        ValueRef::from_raw(unsafe { LLVMBuildRetVoid(builder.as_raw()) })
    }
}

/// Create a 'ret <val>' instruction.
#[derive(Debug)]
pub struct Ret(ValueRef);

pub fn ret(ret: ValueRef) -> Ret {
    Ret(ret)
}

impl Ret {
    pub fn new(ret: ValueRef) -> Self {
        Ret(ret)
    }
}

impl InstructionBuilder for Ret {
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        ValueRef::from_raw(unsafe { LLVMBuildRet(builder.as_raw(), self.0.as_raw()) })
    }
}

/// Create a sequence of N insertvalue instructions,
/// with one Value from the retVals array each, that build a aggregate
/// return value one value at a time, and a ret instruction to return
/// the resulting aggregate value.
#[derive(Debug)]
pub struct AggregateRet(Vec<ValueRef>);

impl InstructionBuilder for AggregateRet {
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        let mut values = self.0
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        ValueRef::from_raw(unsafe {
            LLVMBuildAggregateRet(builder.as_raw(), values.as_mut_ptr(), values.len() as u32)
        })
    }
}

/// Create an unconditional 'br label X' instruction.
#[derive(Debug)]
pub struct Br(BasicBlock);

pub fn br(dest: BasicBlock) -> Br {
    Br(dest)
}

impl Br {
    pub fn new(dest: BasicBlock) -> Self {
        Br(dest)
    }
}

impl InstructionBuilder for Br {
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        ValueRef::from_raw(unsafe { LLVMBuildBr(builder.as_raw(), self.0.as_raw()) })
    }
}

/// Create a conditional 'br Cond, TrueDest, FalseDest' instruction.
#[derive(Debug)]
pub struct CondBr {
    cond: ValueRef,
    _then: Option<BasicBlock>,
    _else: Option<BasicBlock>,
}

pub fn cond_br(cond: ValueRef, _then: Option<BasicBlock>, _else: Option<BasicBlock>) -> CondBr {
    CondBr {
        cond: cond,
        _then: _then,
        _else: _else,
    }
}

impl CondBr {
    pub fn new(cond: ValueRef) -> Self {
        CondBr {
            cond: cond,
            _then: None,
            _else: None,
        }
    }

    pub fn _then(mut self, dest: BasicBlock) -> Self {
        self._then = Some(dest);
        self
    }

    pub fn _else(mut self, dest: BasicBlock) -> Self {
        self._else = Some(dest);
        self
    }
}

impl InstructionBuilder for CondBr {
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        ValueRef::from_raw(unsafe {
            LLVMBuildCondBr(
                builder.as_raw(),
                self.cond.as_raw(),
                self._then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self._else.map_or(ptr::null_mut(), |bb| bb.as_raw()),
            )
        })
    }
}

/// Create a switch instruction with the specified value, default dest,
/// and with a hint for the number of cases that will be added (for efficient allocation).
#[derive(Debug)]
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
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        let switch = ValueRef::from_raw(unsafe {
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

#[derive(Debug)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        ValueRef::from_raw(unsafe { LLVMBuildUnreachable(builder.as_raw()) })
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
    fn build(&self, builder: &IRBuilder) -> ValueRef {
        ValueRef::from_raw(unsafe {
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

macro_rules! define_binary_operator {
    ($name:ident, $func:path) => (
        #[derive(Debug)]
        pub struct $name<'a> {
            lhs: ValueRef,
            rhs: ValueRef,
            name: Cow<'a, str>,
        }

        impl<'a> $name<'a> {
            pub fn new(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
                $name {
                    lhs: lhs,
                    rhs: rhs,
                    name: name,
                }
            }
        }

        impl<'a> $crate::builder::InstructionBuilder for $name<'a> {
            fn build(&self, builder: &IRBuilder) -> ValueRef {
                ValueRef::from_raw(unsafe {
                    $func(
                        builder.as_raw(),
                        self.lhs.as_raw(),
                        self.rhs.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                })
            }
        }

        #[macro_export]
        macro_rules! $name {
            ($lhs:expr, $rhs:expr, $name:expr) => { $crate::ops::$name::new($lhs, $rhs, $name.into()) }
        }
    )
}

define_binary_operator!(Add, LLVMBuildAdd);
define_binary_operator!(NSWAdd, LLVMBuildNSWAdd);
define_binary_operator!(NUWAdd, LLVMBuildNUWAdd);
define_binary_operator!(FAdd, LLVMBuildFAdd);
define_binary_operator!(Sub, LLVMBuildSub);
define_binary_operator!(NSWSub, LLVMBuildNSWSub);
define_binary_operator!(NUWSub, LLVMBuildNUWSub);
define_binary_operator!(FSub, LLVMBuildFSub);
define_binary_operator!(Mul, LLVMBuildMul);
define_binary_operator!(NSWMul, LLVMBuildNSWMul);
define_binary_operator!(NUWMul, LLVMBuildNUWMul);
define_binary_operator!(FMul, LLVMBuildFMul);
define_binary_operator!(UDiv, LLVMBuildUDiv);
define_binary_operator!(ExactUDiv, LLVMBuildExactUDiv);
define_binary_operator!(SDiv, LLVMBuildSDiv);
define_binary_operator!(ExactSDiv, LLVMBuildExactSDiv);
define_binary_operator!(FDiv, LLVMBuildFDiv);
define_binary_operator!(URem, LLVMBuildURem);
define_binary_operator!(SRem, LLVMBuildSRem);
define_binary_operator!(FRem, LLVMBuildFRem);
define_binary_operator!(Shl, LLVMBuildShl);
define_binary_operator!(LShr, LLVMBuildLShr);
define_binary_operator!(AShr, LLVMBuildAShr);
define_binary_operator!(And, LLVMBuildAnd);
define_binary_operator!(Or, LLVMBuildOr);
define_binary_operator!(Xor, LLVMBuildXor);

#[macro_export]
macro_rules! ret {
    () => { $crate::ops::RetVoid };
    ($result:expr) => { $crate::ops::Ret::new($result) };
}

#[macro_export]
macro_rules! add {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::ops::Add::new($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! sub {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::ops::Sub::new($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! mul {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::ops::Mul::new($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! udiv {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::ops::UDiv::new($lhs, $rhs, $name.into()) }
}

#[macro_export]
macro_rules! sdiv {
    ($lhs:expr, $rhs:expr, $name:expr) => { $crate::ops::SDiv::new($lhs, $rhs, $name.into()) }
}

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

    pub fn emit<I: InstructionBuilder + fmt::Debug>(&self, inst: I) -> ValueRef {
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
