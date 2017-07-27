use std::borrow::Cow;
use std::fmt;
use std::ptr;

use llvm::core::*;
use llvm::prelude::*;

use block::BasicBlock;
use context::Context;
use types::TypeRef;
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

/// The ‘ret‘ instruction is used to return control flow (and optionally a value) from a function back to the caller.
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
    or_else: Option<BasicBlock>,
}

impl CondBr {
    pub fn new(cond: ValueRef, then: Option<BasicBlock>, or_else: Option<BasicBlock>) -> CondBr {
        CondBr {
            cond: cond,
            then: then,
            or_else: or_else,
        }
    }

    pub fn on(cond: ValueRef) -> Self {
        CondBr {
            cond: cond,
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
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe {
            LLVMBuildCondBr(
                builder.as_raw(),
                self.cond.as_raw(),
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.or_else.map_or(ptr::null_mut(), |bb| bb.as_raw()),
            )
        })
    }
}

/// The ‘br‘ instruction is used to cause control flow to transfer to a different basic block in the current function.
#[macro_export]
macro_rules! br {
    ($dest:expr) => (
        $crate::ops::Br::new($dest.into())
    );
    ($cond:expr => $then:expr) => (
        $crate::ops::CondBr::on($cond.into()).then($then.into())
    );
    ($cond:expr => $then:expr, _ => $or_else:expr) => (
        $crate::ops::CondBr::on($cond.into()).then($then.into()).or_else($or_else.into())
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

/// The ‘switch‘ instruction is used to transfer control flow to one of several different places.
#[macro_export]
macro_rules! switch {
    ($cond:expr; _ => $default:expr , $( $on:expr => $dest:expr ),*) => ({
        $crate::ops::Switch::on($cond).default($default) $( .case($on, $dest) )*
    });
    ($cond:expr; $( $on:expr => $dest:expr ),*) => ({
        $crate::ops::Switch::on($cond) $( .case($on, $dest) )*
    });
}

/// This instruction is designed to operate as a standard ‘call‘ instruction in most regards.
///
/// The primary difference is that it establishes an association with a label,
/// which is used by the runtime library to unwind the stack.
///
/// This instruction is used in languages with destructors to ensure
/// that proper cleanup is performed in the case of either a longjmp or a thrown exception.
/// Additionally, this is important for implementation of ‘catch‘ clauses in high-level languages that support them.
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

/// The ‘invoke‘ instruction causes control to transfer to a specified function,
/// with the possibility of control flow transfer to either the ‘normal‘ label or the ‘exception‘ label.
#[macro_export]
macro_rules! invoke {
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

/// The ‘resume‘ instruction is a terminator instruction that has no successors.
#[derive(Clone, Debug, PartialEq)]
pub struct Resume(ValueRef);

impl Resume {
    pub fn new(result: ValueRef) -> Self {
        Resume(result)
    }
}

impl InstructionBuilder for Resume {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe {
            LLVMBuildResume(builder.as_raw(), self.0.as_raw())
        })
    }
}

/// The ‘resume‘ instruction is a terminator instruction that has no successors.
#[macro_export]
macro_rules! resume {
    ($result:expr) => ({
        $crate::ops::Resume::new($result.into())
    })
}

/// The ‘unreachable‘ instruction has no defined semantics.
///
/// This instruction is used to inform the optimizer that a particular portion of the code is not reachable.
/// This can be used to indicate that the code after a no-return function cannot be reached, and other facts.
#[derive(Clone, Debug, PartialEq)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    fn build(&self, builder: &IRBuilder) -> Instruction {
        Instruction::from_raw(unsafe { LLVMBuildUnreachable(builder.as_raw()) })
    }
}

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

macro_rules! define_cast_instruction {
    ($operator:ident, $func:path) => (
        #[derive(Clone, Debug, PartialEq)]
        pub struct $operator<'a> {
            value: ValueRef,
            dest_ty: TypeRef,
            name: Cow<'a, str>,
        }

        impl<'a> $operator<'a> {
            pub fn new(value: ValueRef, dest_ty: TypeRef, name: Cow<'a, str>) -> Self {
                $operator {
                    value: value,
                    dest_ty: dest_ty,
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
                        self.dest_ty.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                })
            }
        }
    );

    ($operator:ident, $func:path, $alias:ident) => (
        define_cast_instruction!($operator, $func);

        #[macro_export]
        macro_rules! $alias {
            ($value:expr, $dest_ty:expr, $name:expr) => {
                $crate::ops::$operator::new($value.into(), $dest_ty.into(), $name.into())
            }
        }
    )
}

define_binary_operator!(Add, LLVMBuildAdd, add);
define_binary_operator!(NSWAdd, LLVMBuildNSWAdd, add_nsw);
define_binary_operator!(NUWAdd, LLVMBuildNUWAdd, add_nuw);
define_binary_operator!(FAdd, LLVMBuildFAdd, fadd);
define_binary_operator!(Sub, LLVMBuildSub, sub);
define_binary_operator!(NSWSub, LLVMBuildNSWSub, sub_nsw);
define_binary_operator!(NUWSub, LLVMBuildNUWSub, sub_nuw);
define_binary_operator!(FSub, LLVMBuildFSub, fsub);
define_binary_operator!(Mul, LLVMBuildMul, mul);
define_binary_operator!(NSWMul, LLVMBuildNSWMul, mul_nsw);
define_binary_operator!(NUWMul, LLVMBuildNUWMul, mul_nuw);
define_binary_operator!(FMul, LLVMBuildFMul, fmul);
define_binary_operator!(UDiv, LLVMBuildUDiv, udiv);
define_binary_operator!(ExactUDiv, LLVMBuildExactUDiv, udiv_exact);
define_binary_operator!(SDiv, LLVMBuildSDiv, sdiv);
define_binary_operator!(ExactSDiv, LLVMBuildExactSDiv, sdiv_exact);
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
define_unary_instruction!(NSWNeg, LLVMBuildNSWNeg, neg_nsw);
define_unary_instruction!(NUWNeg, LLVMBuildNUWNeg, neg_nuw);
define_unary_instruction!(FNeg, LLVMBuildFNeg, fneg);
define_unary_instruction!(Not, LLVMBuildNot, not);

define_cast_instruction!(Trunc, LLVMBuildTrunc, trunc);
define_cast_instruction!(ZExt, LLVMBuildZExt, zext);
define_cast_instruction!(SExt, LLVMBuildSExt, sext);
define_cast_instruction!(FPTrunc, LLVMBuildFPTrunc, fptrunc);
define_cast_instruction!(FPExt, LLVMBuildFPExt, fpext);
define_cast_instruction!(FPToUI, LLVMBuildFPToUI, fptoui);
define_cast_instruction!(FPToSI, LLVMBuildFPToSI, fptosi);
define_cast_instruction!(UIToFP, LLVMBuildUIToFP, uitofp);
define_cast_instruction!(SIToFP, LLVMBuildSIToFP, sitofp);
define_cast_instruction!(PtrToInt, LLVMBuildPtrToInt, ptr_to_int);
define_cast_instruction!(IntToPtr, LLVMBuildIntToPtr, int_to_ptr);
define_cast_instruction!(BitCast, LLVMBuildBitCast, bit_cast);
define_cast_instruction!(AddrSpaceCast, LLVMBuildAddrSpaceCast, addrspace_cast);
define_cast_instruction!(ZExtOrBitCast, LLVMBuildZExtOrBitCast, zext_or_bit_cast);
define_cast_instruction!(SExtOrBitCast, LLVMBuildSExtOrBitCast, sext_or_bit_cast);
define_cast_instruction!(TruncOrBitCast, LLVMBuildTruncOrBitCast, trunc_or_bit_cast);
define_cast_instruction!(PointerCast, LLVMBuildPointerCast, ptr_cast);
define_cast_instruction!(IntCast, LLVMBuildIntCast, int_cast);
define_cast_instruction!(FPCast, LLVMBuildFPCast, fp_cast);

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

        let inst = CondBr::on(bool_t.uint(1)).then(bb_then).or_else(bb_else);

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
            invoke!(fn_hello => "ret"; to bb_normal; unwind bb_catch; [i64t.uint(123), i64t.int(456)]);

        assert_eq!(
            builder.emit(inst).to_string().trim(),
            r#"%ret = invoke i64 @hello(i64 123, i64 456)
          to label %normal unwind label %catch"#
        );

        assert_eq!(
            builder.emit(resume!(i64t.uint(123))).to_string().trim(),
            "resume i64 123"
        );
        assert_eq!(builder.emit(Unreachable).to_string().trim(), "unreachable");
    }

    macro_rules! test_instruction {
        ($builder:ident, $name:ident !( $arg0_i64:ident ), $display:expr) => (
            assert_eq!( $builder.emit($name !( $arg0_i64, stringify!($name) ) ).to_string().trim(), $display )
        );
        ($builder:ident, $name:ident !( $arg0_i64:ident, $arg1_i64:ident ), $display:expr) => (
            assert_eq!( $builder.emit($name !( $arg0_i64, $arg1_i64, stringify!($name) ) ).to_string().trim(), $display )
        );
    }

    #[test]
    fn instructions() {
        let c = Context::new();
        let m = Module::with_name_in_context("br", &c);
        let b = IRBuilder::within_context(&c);

        let i32t = c.int32();
        let i64t = c.int64();
        let f32t = c.float();
        let f64t = c.double();

        let p_i64t = i64t.ptr();
        let p_f64t = f64t.ptr();
        let p_f64t_1 = f64t.ptr_in_address_space(1);

        let f_ty = FunctionType::new(
            c.void(),
            &[
                i64t,
                i64t,
                f64t,
                f64t,
                i32t,
                f32t,
                p_i64t.into(),
                p_f64t.into(),
            ],
            false,
        );
        let f = m.add_function("test", f_ty);

        let bb = f.append_basic_block_in_context("entry", &c);
        b.position(Position::AtEnd(bb));

        let arg0_i64 = f.get_param(0).unwrap();
        let arg1_i64 = f.get_param(1).unwrap();
        let arg2_f64 = f.get_param(2).unwrap();
        let arg3_f64 = f.get_param(3).unwrap();
        let arg4_i32 = f.get_param(4).unwrap();
        let arg5_f32 = f.get_param(5).unwrap();
        let arg6_p_i64 = f.get_param(6).unwrap();
        let arg7_p_f64 = f.get_param(7).unwrap();

        test_instruction!(b, add!(arg0_i64, arg1_i64), "%add = add i64 %0, %1");
        test_instruction!(
            b,
            add_nsw!(arg0_i64, arg1_i64),
            "%add_nsw = add nsw i64 %0, %1"
        );
        test_instruction!(
            b,
            add_nuw!(arg0_i64, arg1_i64),
            "%add_nuw = add nuw i64 %0, %1"
        );
        test_instruction!(b, fadd!(arg2_f64, arg3_f64), "%fadd = fadd double %2, %3");

        test_instruction!(b, sub!(arg0_i64, arg1_i64), "%sub = sub i64 %0, %1");
        test_instruction!(
            b,
            sub_nsw!(arg0_i64, arg1_i64),
            "%sub_nsw = sub nsw i64 %0, %1"
        );
        test_instruction!(
            b,
            sub_nuw!(arg0_i64, arg1_i64),
            "%sub_nuw = sub nuw i64 %0, %1"
        );
        test_instruction!(b, fsub!(arg2_f64, arg3_f64), "%fsub = fsub double %2, %3");

        test_instruction!(b, mul!(arg0_i64, arg1_i64), "%mul = mul i64 %0, %1");
        test_instruction!(
            b,
            mul_nsw!(arg0_i64, arg1_i64),
            "%mul_nsw = mul nsw i64 %0, %1"
        );
        test_instruction!(
            b,
            mul_nuw!(arg0_i64, arg1_i64),
            "%mul_nuw = mul nuw i64 %0, %1"
        );
        test_instruction!(b, fmul!(arg2_f64, arg3_f64), "%fmul = fmul double %2, %3");

        test_instruction!(b, udiv!(arg0_i64, arg1_i64), "%udiv = udiv i64 %0, %1");
        test_instruction!(
            b,
            udiv_exact!(arg0_i64, arg1_i64),
            "%udiv_exact = udiv exact i64 %0, %1"
        );
        test_instruction!(b, sdiv!(arg0_i64, arg1_i64), "%sdiv = sdiv i64 %0, %1");
        test_instruction!(
            b,
            sdiv_exact!(arg0_i64, arg1_i64),
            "%sdiv_exact = sdiv exact i64 %0, %1"
        );
        test_instruction!(b, fdiv!(arg2_f64, arg3_f64), "%fdiv = fdiv double %2, %3");

        test_instruction!(b, urem!(arg0_i64, arg1_i64), "%urem = urem i64 %0, %1");
        test_instruction!(b, srem!(arg0_i64, arg1_i64), "%srem = srem i64 %0, %1");
        test_instruction!(b, frem!(arg2_f64, arg3_f64), "%frem = frem double %2, %3");

        test_instruction!(b, shl!(arg0_i64, arg1_i64), "%shl = shl i64 %0, %1");
        test_instruction!(b, ashr!(arg0_i64, arg1_i64), "%ashr = ashr i64 %0, %1");
        test_instruction!(b, lshr!(arg0_i64, arg1_i64), "%lshr = lshr i64 %0, %1");

        test_instruction!(b, and!(arg0_i64, arg1_i64), "%and = and i64 %0, %1");
        test_instruction!(b, or!(arg0_i64, arg1_i64), "%or = or i64 %0, %1");
        test_instruction!(b, xor!(arg0_i64, arg1_i64), "%xor = xor i64 %0, %1");

        test_instruction!(b, neg!(arg0_i64), "%neg = sub i64 0, %0");
        test_instruction!(b, neg_nsw!(arg0_i64), "%neg_nsw = sub nsw i64 0, %0");
        test_instruction!(b, neg_nuw!(arg0_i64), "%neg_nuw = sub nuw i64 0, %0");
        test_instruction!(b, fneg!(arg2_f64), "%fneg = fsub double -0.000000e+00, %2");

        test_instruction!(b, not!(arg0_i64), "%not = xor i64 %0, -1");
        test_instruction!(
            b,
            not!(arg3_f64),
            "%not1 = xor double %3, 0xFFFFFFFFFFFFFFFF"
        );

        test_instruction!(b, trunc!(arg0_i64, i32t), "%trunc = trunc i64 %0 to i32");
        test_instruction!(b, zext!(arg4_i32, i64t), "%zext = zext i32 %4 to i64");
        test_instruction!(b, sext!(arg4_i32, i64t), "%sext = sext i32 %4 to i64");

        test_instruction!(
            b,
            fptrunc!(arg2_f64, f32t),
            "%fptrunc = fptrunc double %2 to float"
        );
        test_instruction!(
            b,
            fpext!(arg5_f32, f64t),
            "%fpext = fpext float %5 to double"
        );

        test_instruction!(
            b,
            fptoui!(arg2_f64, i64t),
            "%fptoui = fptoui double %2 to i64"
        );
        test_instruction!(
            b,
            fptosi!(arg2_f64, i64t),
            "%fptosi = fptosi double %2 to i64"
        );
        test_instruction!(
            b,
            uitofp!(arg0_i64, f64t),
            "%uitofp = uitofp i64 %0 to double"
        );
        test_instruction!(
            b,
            sitofp!(arg0_i64, f64t),
            "%sitofp = sitofp i64 %0 to double"
        );

        test_instruction!(
            b,
            ptr_to_int!(arg6_p_i64, i64t),
            "%ptr_to_int = ptrtoint i64* %6 to i64"
        );
        test_instruction!(
            b,
            int_to_ptr!(arg0_i64, p_i64t),
            "%int_to_ptr = inttoptr i64 %0 to i64*"
        );

        test_instruction!(
            b,
            bit_cast!(arg6_p_i64, p_f64t),
            "%bit_cast = bitcast i64* %6 to double*"
        );
        test_instruction!(
            b,
            addrspace_cast!(arg7_p_f64, p_f64t_1),
            "%addrspace_cast = addrspacecast double* %7 to double addrspace(1)*"
        );

        test_instruction!(
            b,
            trunc_or_bit_cast!(arg0_i64, i32t),
            "%trunc_or_bit_cast = trunc i64 %0 to i32"
        );
        test_instruction!(
            b,
            trunc_or_bit_cast!(arg0_i64, f64t),
            "%trunc_or_bit_cast2 = bitcast i64 %0 to double"
        );
        test_instruction!(
            b,
            zext_or_bit_cast!(arg4_i32, i64t),
            "%zext_or_bit_cast = zext i32 %4 to i64"
        );
        test_instruction!(
            b,
            zext_or_bit_cast!(arg4_i32, f32t),
            "%zext_or_bit_cast3 = bitcast i32 %4 to float"
        );
        test_instruction!(
            b,
            sext_or_bit_cast!(arg4_i32, i64t),
            "%sext_or_bit_cast = sext i32 %4 to i64"
        );
        test_instruction!(
            b,
            sext_or_bit_cast!(arg4_i32, f32t),
            "%sext_or_bit_cast4 = bitcast i32 %4 to float"
        );

        test_instruction!(
            b,
            ptr_cast!(arg6_p_i64, p_f64t),
            "%ptr_cast = bitcast i64* %6 to double*"
        );
        test_instruction!(
            b,
            int_cast!(arg0_i64, i32t),
            "%int_cast = trunc i64 %0 to i32"
        );
        test_instruction!(
            b,
            int_cast!(arg4_i32, i64t),
            "%int_cast5 = sext i32 %4 to i64"
        );
        test_instruction!(
            b,
            fp_cast!(arg2_f64, f32t),
            "%fp_cast = fptrunc double %2 to float"
        );
        test_instruction!(
            b,
            fp_cast!(arg5_f32, f64t),
            "%fp_cast6 = fpext float %5 to double"
        );
    }
}
