use std::borrow::Cow;
use std::fmt;
use std::mem;
use std::ptr;

use llvm::{LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMIntPredicate, LLVMRealPredicate};
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction;
}

/// Create a 'ret void' instruction.
#[derive(Clone, Debug, PartialEq)]
pub struct RetVoid;

impl InstructionBuilder for RetVoid {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildRetVoid(builder.as_raw()) }.into()
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildRet(builder.as_raw(), self.0.as_raw()) }.into()
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        let mut values = self.0
            .iter()
            .map(|v| v.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe { LLVMBuildAggregateRet(builder.as_raw(), values.as_mut_ptr(), values.len() as u32) }
            .into()
    }
}

/// The `ret` instruction is used to return control flow (and optionally a value) from a function back to the caller.
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildBr(builder.as_raw(), self.0.as_raw()) }.into()
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
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
        $crate::ops::Br::new($dest.into())
    );
    ($addr:expr => [$( $dest:expr ),*]) => (
        $crate::ops::IndirectBr::on($addr.into()) $( .jump_to($dest.into()) )*
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        let switch: Instruction = unsafe {
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

/// The `switch` instruction is used to transfer control flow to one of several different places.
#[macro_export]
macro_rules! switch {
    ($cond:expr; _ => $default:expr , $( $on:expr => $dest:expr ),*) => ({
        $crate::ops::Switch::on($cond).default($default) $( .case($on, $dest) )*
    });
    ($cond:expr; $( $on:expr => $dest:expr ),*) => ({
        $crate::ops::Switch::on($cond) $( .case($on, $dest) )*
    });
}

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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        let mut args = self.args
            .iter()
            .map(|arg| arg.as_raw())
            .collect::<Vec<LLVMValueRef>>();

        unsafe {
            LLVMBuildInvoke(
                builder.as_raw(),
                self.func.as_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                self.then.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                self.unwind.map_or(ptr::null_mut(), |bb| bb.as_raw()),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `invoke` instruction causes control to transfer to a specified function,
/// with the possibility of control flow transfer to either the `normal` label or the `exception` label.
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
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
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

        unsafe { LLVMSetCleanup(landing_pad.as_raw(), if self.cleanup { 1 } else { 0 }) }

        landing_pad
    }
}

/// The `resume` instruction is a terminator instruction that has no successors.
#[derive(Clone, Debug, PartialEq)]
pub struct Resume(ValueRef);

impl Resume {
    pub fn new(result: ValueRef) -> Self {
        Resume(result)
    }
}

impl InstructionBuilder for Resume {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildResume(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// The `resume` instruction is a terminator instruction that has no successors.
#[macro_export]
macro_rules! resume {
    ($result:expr) => ({
        $crate::ops::Resume::new($result.into())
    })
}

/// The `unreachable` instruction has no defined semantics.
///
/// This instruction is used to inform the optimizer that a particular portion of the code is not reachable.
/// This can be used to indicate that the code after a no-return function cannot be reached, and other facts.
#[derive(Clone, Debug, PartialEq)]
pub struct Unreachable;

impl InstructionBuilder for Unreachable {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildUnreachable(builder.as_raw()) }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Malloc<'a> {
    ty: TypeRef,
    size: Option<ValueRef>,
    name: Cow<'a, str>,
}

impl<'a> Malloc<'a> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Malloc {
            ty,
            size: None,
            name,
        }
    }
    pub fn array(ty: TypeRef, size: ValueRef, name: Cow<'a, str>) -> Self {
        Malloc {
            ty,
            size: Some(size),
            name,
        }
    }
}

impl<'a> InstructionBuilder for Malloc<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayMalloc(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            } else {
                LLVMBuildMalloc(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            }
        }.into()
    }
}

/// Invoke `malloc` function to allocates memory on the heap, need to be expicity released by its caller.
#[macro_export]
macro_rules! malloc {
    ($ty:expr, $name:expr) => ({
        $crate::ops::Malloc::new($ty.into(), $name.into())
    });
    ($array_ty:expr, $size:expr, $name:expr) => ({
        $crate::ops::Malloc::array($array_ty.into(), $size.into(), $name.into())
    });
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alloca<'a> {
    ty: TypeRef,
    size: Option<ValueRef>,
    name: Cow<'a, str>,
}

impl<'a> Alloca<'a> {
    pub fn new(ty: TypeRef, name: Cow<'a, str>) -> Self {
        Alloca {
            ty,
            size: None,
            name,
        }
    }
    pub fn array(ty: TypeRef, size: ValueRef, name: Cow<'a, str>) -> Self {
        Alloca {
            ty,
            size: Some(size),
            name,
        }
    }
}

impl<'a> InstructionBuilder for Alloca<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            if let Some(size) = self.size {
                LLVMBuildArrayAlloca(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    size.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            } else {
                LLVMBuildAlloca(
                    builder.as_raw(),
                    self.ty.as_raw(),
                    unchecked_cstring(self.name.clone()).as_ptr(),
                )
            }
        }.into()
    }
}

/// The `alloca` instruction allocates memory on the stack frame of the currently executing function,
/// to be automatically released when this function returns to its caller.
///
/// The object is always allocated in the generic address space (address space zero).
#[macro_export]
macro_rules! alloca {
    ($ty:expr, $name:expr) => ({
        $crate::ops::Alloca::new($ty.into(), $name.into())
    });
    ($array_ty:expr, $size:expr, $name:expr) => ({
        $crate::ops::Alloca::array($array_ty.into(), $size.into(), $name.into())
    });
}

#[derive(Clone, Debug, PartialEq)]
pub struct Free(ValueRef);

impl Free {
    pub fn new(ptr: ValueRef) -> Self {
        Free(ptr)
    }
}

impl InstructionBuilder for Free {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildFree(builder.as_raw(), self.0.as_raw()) }.into()
    }
}

/// Deallocates the memory allocation pointed to by ptr.
#[macro_export]
macro_rules! free {
    ($ptr:expr) => ({
        $crate::ops::Free::new($ptr.into())
    })
}

#[derive(Clone, Debug, PartialEq)]
pub struct Load<'a> {
    ptr: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> Load<'a> {
    pub fn new(ptr: ValueRef, name: Cow<'a, str>) -> Self {
        Load { ptr, name }
    }
}

impl<'a> InstructionBuilder for Load<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildLoad(
                builder.as_raw(),
                self.ptr.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `load` instruction is used to read from memory.
#[macro_export]
macro_rules! load {
    ($ptr:expr, $name:expr) => ({
        $crate::ops::Load::new($ptr.into(), $name.into())
    })
}

#[derive(Clone, Debug, PartialEq)]
pub struct Store {
    value: ValueRef,
    ptr: ValueRef,
}

impl Store {
    pub fn new(value: ValueRef, ptr: ValueRef) -> Self {
        Store { value, ptr }
    }
}

impl InstructionBuilder for Store {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe { LLVMBuildStore(builder.as_raw(), self.value.as_raw(), self.ptr.as_raw()) }.into()
    }
}

/// The `store` instruction is used to write to memory.
#[macro_export]
macro_rules! store {
    ($value:expr, $ptr:expr) => ({
        $crate::ops::Store::new($value.into(), $ptr.into())
    })
}

/// an instruction for type-safe pointer arithmetic to access elements of arrays and structs
#[derive(Clone, Debug, PartialEq)]
pub struct GetElementPtr<'a> {
    ptr: ValueRef,
    gep: GEP,
    name: Cow<'a, str>,
}

#[derive(Clone, Debug, PartialEq)]
enum GEP {
    Indices(Vec<ValueRef>),
    InBounds(Vec<ValueRef>),
    Struct(u32),
}

impl<'a> GetElementPtr<'a> {
    pub fn new(ptr: ValueRef, indices: Vec<ValueRef>, name: Cow<'a, str>) -> Self {
        GetElementPtr {
            ptr,
            gep: GEP::Indices(indices),
            name,
        }
    }

    pub fn in_bounds(ptr: ValueRef, indices: Vec<ValueRef>, name: Cow<'a, str>) -> Self {
        GetElementPtr {
            ptr,
            gep: GEP::InBounds(indices),
            name,
        }
    }

    pub fn in_struct(ptr: ValueRef, index: u32, name: Cow<'a, str>) -> Self {
        GetElementPtr {
            ptr,
            gep: GEP::Struct(index),
            name,
        }
    }
}

impl<'a> InstructionBuilder for GetElementPtr<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            match self.gep {
                GEP::Indices(ref indices) |
                GEP::InBounds(ref indices) => {
                    let mut indices = indices
                        .iter()
                        .map(|v| v.as_raw())
                        .collect::<Vec<LLVMValueRef>>();

                    let gep = if let GEP::Indices(_) = self.gep {
                        LLVMBuildGEP
                    } else {
                        LLVMBuildInBoundsGEP
                    };

                    gep(
                        builder.as_raw(),
                        self.ptr.as_raw(),
                        indices.as_mut_ptr(),
                        indices.len() as u32,
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }
                GEP::Struct(index) => {
                    LLVMBuildStructGEP(
                        builder.as_raw(),
                        self.ptr.as_raw(),
                        index,
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }
            }
        }.into()
    }
}

/// The `getelementptr` instruction is used to get the address of a subelement of an aggregate data structure.
///
/// It performs address calculation only and does not access memory.
/// The instruction can also be used to calculate a vector of such addresses.
///
/// If the `inbounds` keyword is present, the result value of the getelementptr is a poison value
/// if the base pointer is not an in bounds address of an allocated object, or if any of the addresses
/// that would be formed by successive addition of the offsets implied by the indices to the base address
/// with infinitely precise signed arithmetic are not an in bounds address of that allocated object.
/// The in bounds addresses for an allocated object are all the addresses that point into the object,
/// plus the address one byte past the end. In cases where the base is a vector of pointers
/// the inbounds keyword applies to each of the computations element-wise.
///
/// If the `inbounds` keyword is not present, the offsets are added to the base address
/// with silently-wrapping two’s complement arithmetic. If the offsets have a different width from the pointer,
/// they are sign-extended or truncated to the width of the pointer.
/// The result value of the getelementptr may be outside the object pointed to by the base pointer.
/// The result value may not necessarily be used to access memory though,
/// even if it happens to point into allocated storage.
#[macro_export]
macro_rules! gep {
    ($ptr:expr, [ $( $index:expr ),* ], $name:expr) => ({
        $crate::ops::GetElementPtr::new($ptr.into(), vec![ $( $index.into() ),* ], $name.into())
    });
    (inbounds $ptr:expr, [ $( $index:expr ),* ], $name:expr) => ({
        $crate::ops::GetElementPtr::in_bounds($ptr.into(), vec![ $( $index.into() ),* ], $name.into())
    });
    (structure $struct_ptr:expr, $index:expr, $name:expr) => ({
        $crate::ops::GetElementPtr::in_struct($struct_ptr.into(), $index, $name.into())
    });
}

/// Make a new global variable with an initializer that has array of i8 type
/// filled in with the null terminated string value specified.
/// The new global variable will be marked mergable with any others of the same contents.
/// If Name is specified, it is the name of the global variable created.
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalString<'a> {
    s: Cow<'a, str>,
    name: Cow<'a, str>,
}

impl<'a> GlobalString<'a> {
    pub fn new(s: Cow<'a, str>, name: Cow<'a, str>) -> Self {
        GlobalString { s, name }
    }
}

impl<'a> InstructionBuilder for GlobalString<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildGlobalString(
                builder.as_raw(),
                unchecked_cstring(self.s.clone()).as_ptr(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// Make a new global variable with initializer type i8*
#[macro_export]
macro_rules! global_str {
    ($s:expr, $name:expr) => ({
        $crate::ops::GlobalString::new($s.into(), $name.into())
    })
}

/// Same as `GlobalString`, but return a pointer with "i8*" type instead of a pointer to array of i8.
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalStringPtr<'a> {
    s: Cow<'a, str>,
    name: Cow<'a, str>,
}

impl<'a> GlobalStringPtr<'a> {
    pub fn new(s: Cow<'a, str>, name: Cow<'a, str>) -> Self {
        GlobalStringPtr { s, name }
    }
}

impl<'a> InstructionBuilder for GlobalStringPtr<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildGlobalStringPtr(
                builder.as_raw(),
                unchecked_cstring(self.s.clone()).as_ptr(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// Make a new global variable with initializer type i8*, return a pointer with "i8*" type instead of a pointer to array of i8.
#[macro_export]
macro_rules! global_str_ptr {
    ($s:expr, $name:expr) => ({
        $crate::ops::GlobalStringPtr::new($s.into(), $name.into())
    })
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
                $operator { value, name }
            }
        }

        impl<'a> $crate::builder::InstructionBuilder for $operator<'a> {
            fn emit_to(&self, builder: &IRBuilder) -> Instruction {
                unsafe {
                    $func(
                        builder.as_raw(),
                        self.value.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }.into()
            }
        }
    );

    ($operator:ident, $func:path, $alias:ident, $comment:expr) => (
        define_unary_instruction!($operator, $func);

        #[doc=$comment]
        #[macro_export]
        macro_rules! $alias {
            ($value:expr, $name:expr) => { $crate::ops::$operator::new($value.into(), $name.into()) }
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
                $operator { lhs, rhs, name }
            }
        }

        impl<'a> $crate::builder::InstructionBuilder for $operator<'a> {
            fn emit_to(&self, builder: &IRBuilder) -> Instruction {
                unsafe {
                    $func(
                        builder.as_raw(),
                        self.lhs.as_raw(),
                        self.rhs.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }.into()
            }
        }
    );

    ($operator:ident, $func:path, $alias:ident, $comment:expr) => (
        define_binary_operator!($operator, $func);

        #[doc=$comment]
        #[macro_export]
        macro_rules! $alias {
            ($lhs:expr, $rhs:expr, $name:expr) => {
                $crate::ops::$operator::new($lhs.into(), $rhs.into(), $name.into())
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
                $operator { value, dest_ty, name }
            }
        }

        impl<'a> $crate::builder::InstructionBuilder for $operator<'a> {
            fn emit_to(&self, builder: &IRBuilder) -> Instruction {
                unsafe {
                    $func(
                        builder.as_raw(),
                        self.value.as_raw(),
                        self.dest_ty.as_raw(),
                        unchecked_cstring(self.name.clone()).as_ptr(),
                    )
                }.into()
            }
        }
    );
    ($operator:ident, $func:path, $alias:ident, $comment:expr) => (
        define_cast_instruction!($operator, $func);

        #[doc=$comment]
        #[macro_export]
        macro_rules! $alias {
            ($value:expr, $dest_ty:expr, $name:expr) => {
                $crate::ops::$operator::new($value.into(), $dest_ty.into(), $name.into())
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

define_binary_operator!(
    Add,
    LLVMBuildAdd,
    add,
    "The `add` instruction returns the sum of its two operands."
);
define_binary_operator!(
    NSWAdd,
    LLVMBuildNSWAdd,
    add_nsw,
    "The `add` instruction returns the sum of its two operands, the result value of the `add` is a poison value if signed overflow occurs."
);
define_binary_operator!(
    NUWAdd,
    LLVMBuildNUWAdd,
    add_nuw,
    "The `add` instruction returns the sum of its two operands, the result value of the `add` is a poison value if unsigned overflow occurs."
);
define_binary_operator!(
    FAdd,
    LLVMBuildFAdd,
    fadd,
    "The `fadd` instruction returns the sum of its two operands."
);
define_binary_operator!(
    Sub,
    LLVMBuildSub,
    sub,
    "The `sub` instruction returns the difference of its two operands."
);
define_binary_operator!(
    NSWSub,
    LLVMBuildNSWSub,
    sub_nsw,
    "The `sub` instruction returns the difference of its two operands, the result value of the `sub` is a poison value if signed overflow occurs."
);
define_binary_operator!(
    NUWSub,
    LLVMBuildNUWSub,
    sub_nuw,
    "The `sub` instruction returns the difference of its two operands, the result value of the `sub` is a poison value if unsigned overflow occurs."
);
define_binary_operator!(
    FSub,
    LLVMBuildFSub,
    fsub,
    "The `fsub` instruction returns the difference of its two operands."
);
define_binary_operator!(
    Mul,
    LLVMBuildMul,
    mul,
    "The `mul` instruction returns the product of its two operands."
);
define_binary_operator!(
    NSWMul,
    LLVMBuildNSWMul,
    mul_nsw,
    "The `mul` instruction returns the product of its two operands, the result value of the `mul` is a poison value if signed overflow occurs."
);
define_binary_operator!(
    NUWMul,
    LLVMBuildNUWMul,
    mul_nuw,
    "The `mul` instruction returns the product of its two operands, the result value of the `mul` is a poison value if unsigned overflow occurs."
);
define_binary_operator!(
    FMul,
    LLVMBuildFMul,
    fmul,
    "The `fmul` instruction returns the product of its two operands."
);
define_binary_operator!(
    UDiv,
    LLVMBuildUDiv,
    udiv,
    "The `udiv` instruction returns the quotient of its two operands."
);
define_binary_operator!(
    ExactUDiv,
    LLVMBuildExactUDiv,
    udiv_exact,
    "The `udiv` instruction returns the quotient of its two operands, the result value of the `udiv` is a poison value if %`lhs` is not a multiple of %`rhs` (as such, “((a udiv exact b) mul b) == a”)."
);
define_binary_operator!(
    SDiv,
    LLVMBuildSDiv,
    sdiv,
    "The `sdiv` instruction returns the quotient of its two operands"
);
define_binary_operator!(
    ExactSDiv,
    LLVMBuildExactSDiv,
    sdiv_exact,
    "The `sdiv` instruction returns the quotient of its two operands, the result value of the `sdiv` is a poison value if the result would be rounded."
);
define_binary_operator!(
    FDiv,
    LLVMBuildFDiv,
    fdiv,
    "The `fdiv` instruction returns the quotient of its two operands."
);
define_binary_operator!(
    URem,
    LLVMBuildURem,
    urem,
    "The `urem` instruction returns the remainder from the unsigned division of its two arguments."
);
define_binary_operator!(
    SRem,
    LLVMBuildSRem,
    srem,
    "The `srem` instruction returns the remainder from the signed division of its two operands. "
);
define_binary_operator!(
    FRem,
    LLVMBuildFRem,
    frem,
    "The `frem` instruction returns the remainder from the division of its two operands.
"
);
define_binary_operator!(
    Shl,
    LLVMBuildShl,
    shl,
    "The `shl` instruction returns the first operand shifted to the left a specified number of bits."
);
define_binary_operator!(
    LShr,
    LLVMBuildLShr,
    lshr,
    "The `lshr` instruction (logical shift right) returns the first operand shifted to the right a specified number of bits with zero fill."
);
define_binary_operator!(
    AShr,
    LLVMBuildAShr,
    ashr,
    "The `ashr` instruction (arithmetic shift right) returns the first operand shifted to the right a specified number of bits with sign extension.
"
);
define_binary_operator!(
    And,
    LLVMBuildAnd,
    and,
    "The `and` instruction returns the bitwise logical and of its two operands."
);
define_binary_operator!(
    Or,
    LLVMBuildOr,
    or,
    "The `or` instruction returns the bitwise logical inclusive or of its two operands."
);
define_binary_operator!(
    Xor,
    LLVMBuildXor,
    xor,
    "The `xor` instruction returns the bitwise logical exclusive or of its two operands."
);

define_unary_instruction!(
    Neg,
    LLVMBuildNeg,
    neg,
    "The unary negation operator precedes its operand and negates it."
);
define_unary_instruction!(
    NSWNeg,
    LLVMBuildNSWNeg,
    neg_nsw,
    "The unary negation operator precedes its operand and negates it, the result value of the `neg` is a poison value if signed overflow occurs."
);
define_unary_instruction!(
    NUWNeg,
    LLVMBuildNUWNeg,
    neg_nuw,
    "The unary negation operator precedes its operand and negates it, the result value of the `neg` is a poison value if unsigned overflow occurs."
);
define_unary_instruction!(
    FNeg,
    LLVMBuildFNeg,
    fneg,
    "The unary negation operator precedes its operand and negates it."
);
define_unary_instruction!(
    Not,
    LLVMBuildNot,
    not,
    "The logical negation operator reverses the meaning of its operand."
);

define_cast_instruction!(
    Trunc,
    LLVMBuildTrunc,
    trunc,
    "The `trunc` instruction truncates its operand to the type."
);
define_cast_instruction!(
    ZExt,
    LLVMBuildZExt,
    zext,
    "The `zext` instruction zero extends its operand to type."
);
define_cast_instruction!(
    SExt,
    LLVMBuildSExt,
    sext,
    "The `sext` instruction takes a value to cast, and a type to cast it to. Both types must be of integer types, or vectors of the same number of integers. The bit size of the value must be smaller than the bit size of the destination type."
);
define_cast_instruction!(
    FPTrunc,
    LLVMBuildFPTrunc,
    fptrunc,
    "The `fptrunc` instruction truncates value to type."
);
define_cast_instruction!(
    FPExt,
    LLVMBuildFPExt,
    fpext,
    "The `fpext` extends a floating point value to a larger floating point value."
);
define_cast_instruction!(
    FPToUI,
    LLVMBuildFPToUI,
    fptoui,
    "The `fptoui` converts a floating point value to its unsigned integer equivalent of type."
);
define_cast_instruction!(
    FPToSI,
    LLVMBuildFPToSI,
    fptosi,
    "The `fptosi` instruction converts floating point value to type."
);
define_cast_instruction!(
    UIToFP,
    LLVMBuildUIToFP,
    uitofp,
    "The `uitofp` instruction regards value as an unsigned integer and converts that value to the type."
);
define_cast_instruction!(
    SIToFP,
    LLVMBuildSIToFP,
    sitofp,
    "The `sitofp` instruction regards value as a signed integer and converts that value to the type."
);
define_cast_instruction!(
    PtrToInt,
    LLVMBuildPtrToInt,
    ptr_to_int,
    "The `ptrtoint` instruction converts the pointer or a vector of pointers value to the integer (or vector of integers) type."
);
define_cast_instruction!(
    IntToPtr,
    LLVMBuildIntToPtr,
    int_to_ptr,
    "The `inttoptr` instruction converts an integer value to a pointer type."
);
define_cast_instruction!(
    BitCast,
    LLVMBuildBitCast,
    bit_cast,
    "The `bitcast` instruction converts value to type ty2 without changing any bits."
);
define_cast_instruction!(
    AddrSpaceCast,
    LLVMBuildAddrSpaceCast,
    addrspace_cast,
    "The `addrspacecast` instruction converts ptrval from pty in address space n to type pty2 in address space m."
);
define_cast_instruction!(
    ZExtOrBitCast,
    LLVMBuildZExtOrBitCast,
    zext_or_bit_cast,
    "Create a `zext` or `bitcast` cast instruction."
);
define_cast_instruction!(
    SExtOrBitCast,
    LLVMBuildSExtOrBitCast,
    sext_or_bit_cast,
    "Create a `sext` or `bitcast` cast instruction."
);
define_cast_instruction!(
    TruncOrBitCast,
    LLVMBuildTruncOrBitCast,
    trunc_or_bit_cast,
    "Create a `trunc` or `bitcast` cast instruction."
);
define_cast_instruction!(
    PointerCast,
    LLVMBuildPointerCast,
    ptr_cast,
    "Create a `bitcast`, `addrspacecast`, or `ptrtoint` cast instruction."
);
define_cast_instruction!(
    IntCast,
    LLVMBuildIntCast,
    int_cast,
    "Create a `zext`, `bitcast`, or `trunc` instruction for int -> int casts."
);
define_cast_instruction!(
    FPCast,
    LLVMBuildFPCast,
    fp_cast,
    "Create an `fpext`, `bitcast`, or `fptrunc` instruction for fp -> fp casts."
);

#[derive(Clone, Debug, PartialEq)]
pub struct ICmp<'a> {
    op: u32, // TODO: use LLVMIntPredicate when llvm-sys update
    lhs: ValueRef,
    rhs: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> ICmp<'a> {
    pub fn new(op: LLVMIntPredicate, lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        ICmp {
            op: op as u32,
            lhs,
            rhs,
            name,
        }
    }

    pub fn equals(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, name)
    }

    pub fn not_equals(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntNE, lhs, rhs, name)
    }

    pub fn unsigned_greater_than(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntUGT, lhs, rhs, name)
    }

    pub fn unsigned_greater_or_equal(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntUGE, lhs, rhs, name)
    }

    pub fn unsigned_less_than(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntULT, lhs, rhs, name)
    }

    pub fn unsigned_less_or_equal(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntULE, lhs, rhs, name)
    }

    pub fn signed_greater_than(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, name)
    }

    pub fn signed_greater_or_equal(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }

    pub fn signed_less_than(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, name)
    }

    pub fn signed_less_or_equal(lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        Self::new(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }
}

impl<'a> InstructionBuilder for ICmp<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildICmp(
                builder.as_raw(),
                mem::transmute(self.op),
                self.lhs.as_raw(),
                self.rhs.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `icmp` instruction returns a boolean value or a vector of boolean values
/// based on comparison of its two integer, integer vector, pointer, or pointer vector operands.
///
/// The `icmp` compares `lhs` and `rhs` according to the condition code given as cond.
/// The comparison performed always yields either an `i1` or `vector` of `i1` result, as follows:
///
/// - eq: yields true if the operands are equal, false otherwise. No sign interpretation is necessary or performed.
/// - ne: yields true if the operands are unequal, false otherwise. No sign interpretation is necessary or performed.
/// - ugt: interprets the operands as unsigned values and yields true if `lhs` is greater than `rhs`.
/// - uge: interprets the operands as unsigned values and yields true if `lhs` is greater than or equal to `rhs`.
/// - ult: interprets the operands as unsigned values and yields true if `lhs` is less than `rhs`.
/// - ule: interprets the operands as unsigned values and yields true if `lhs` is less than or equal to `rhs`.
/// - sgt: interprets the operands as signed values and yields true if `lhs` is greater than `rhs`.
/// - sge: interprets the operands as signed values and yields true if `lhs` is greater than or equal to `rhs`.
/// - slt: interprets the operands as signed values and yields true if `lhs` is less than `rhs`.
/// - sle: interprets the operands as signed values and yields true if `lhs` is less than or equal to `rhs`.
#[macro_export]
macro_rules! icmp {
    (eq $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntEQ, $lhs.into(), $rhs.into(), $name.into())
    );
    (ne $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntNE, $lhs.into(), $rhs.into(), $name.into())
    );
    (ugt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntUGT, $lhs.into(), $rhs.into(), $name.into())
    );
    (uge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntUGE, $lhs.into(), $rhs.into(), $name.into())
    );
    (ult $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntULT, $lhs.into(), $rhs.into(), $name.into())
    );
    (ule $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntULE, $lhs.into(), $rhs.into(), $name.into())
    );
    (sgt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntSGT, $lhs.into(), $rhs.into(), $name.into())
    );
    (sge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntSGE, $lhs.into(), $rhs.into(), $name.into())
    );
    (slt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntSLT, $lhs.into(), $rhs.into(), $name.into())
    );
    (sle $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::ICmp::new(::llvm::LLVMIntPredicate::LLVMIntSLE, $lhs.into(), $rhs.into(), $name.into())
    );
}

#[derive(Clone, Debug, PartialEq)]
pub struct FCmp<'a> {
    op: u32, // TODO: use LLVMRealPredicate when llvm-sys update
    lhs: ValueRef,
    rhs: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> FCmp<'a> {
    pub fn new(op: LLVMRealPredicate, lhs: ValueRef, rhs: ValueRef, name: Cow<'a, str>) -> Self {
        FCmp {
            op: op as u32,
            lhs,
            rhs,
            name,
        }
    }
}

impl<'a> InstructionBuilder for FCmp<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildFCmp(
                builder.as_raw(),
                mem::transmute(self.op),
                self.lhs.as_raw(),
                self.rhs.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `fcmp` instruction returns a boolean value or vector of boolean values based on comparison of its operands.
///
/// The `fcmp` instruction compares `lhs` and `rhs` according to the condition code given as cond.
/// If the operands are vectors, then the vectors are compared element by element.
/// Each comparison performed always yields an i1 result, as follows:
///
///  - false: always yields false, regardless of operands.
///  - oeq: yields true if both operands are not a QNAN and `lhs` is equal to `rhs`.
///  - ogt: yields true if both operands are not a QNAN and `lhs` is greater than `rhs`.
///  - oge: yields true if both operands are not a QNAN and `lhs` is greater than or equal to `rhs`.
///  - olt: yields true if both operands are not a QNAN and `lhs` is less than `rhs`.
///  - ole: yields true if both operands are not a QNAN and `lhs` is less than or equal to `rhs`.
///  - one: yields true if both operands are not a QNAN and `lhs` is not equal to `rhs`.
///  - ord: yields true if both operands are not a QNAN.
///  - ueq: yields true if either operand is a QNAN or `lhs` is equal to `rhs`.
///  - ugt: yields true if either operand is a QNAN or `lhs` is greater than `rhs`.
///  - uge: yields true if either operand is a QNAN or `lhs` is greater than or equal to `rhs`.
///  - ult: yields true if either operand is a QNAN or `lhs` is less than `rhs`.
///  - ule: yields true if either operand is a QNAN or `lhs` is less than or equal to `rhs`.
///  - une: yields true if either operand is a QNAN or `lhs` is not equal to `rhs`.
///  - uno: yields true if either operand is a QNAN.
///  - true: always yields true, regardless of operands.
#[macro_export]
macro_rules! fcmp {
    (false $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealPredicateFalse, $lhs.into(), $rhs.into(), $name.into())
    );
    (oeq $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealOEQ, $lhs.into(), $rhs.into(), $name.into())
    );
    (ogt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealOGT, $lhs.into(), $rhs.into(), $name.into())
    );
    (oge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealOGE, $lhs.into(), $rhs.into(), $name.into())
    );
    (olt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealOLT, $lhs.into(), $rhs.into(), $name.into())
    );
    (ole $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealOLE, $lhs.into(), $rhs.into(), $name.into())
    );
    (one $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealONE, $lhs.into(), $rhs.into(), $name.into())
    );
    (ord $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealORD, $lhs.into(), $rhs.into(), $name.into())
    );
    (uno $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealUNO, $lhs.into(), $rhs.into(), $name.into())
    );
    (ueq $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealUEQ, $lhs.into(), $rhs.into(), $name.into())
    );
    (ugt $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealUGT, $lhs.into(), $rhs.into(), $name.into())
    );
    (uge $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealUGE, $lhs.into(), $rhs.into(), $name.into())
    );
    (ult $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealULT, $lhs.into(), $rhs.into(), $name.into())
    );
    (ule $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealULE, $lhs.into(), $rhs.into(), $name.into())
    );
    (une $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealUNE, $lhs.into(), $rhs.into(), $name.into())
    );
    (true $lhs:expr, $rhs:expr ; $name:expr) => (
        $crate::ops::FCmp::new(::llvm::LLVMRealPredicate::LLVMRealPredicateTrue, $lhs.into(), $rhs.into(), $name.into())
    );
}

/// This instruction extracts a single (scalar) element from a VectorType value
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractElement<'a> {
    vector: ValueRef,
    index: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> ExtractElement<'a> {
    pub fn new(vector: ValueRef, index: ValueRef, name: Cow<'a, str>) -> Self {
        ExtractElement {
            vector,
            index,
            name,
        }
    }
}

impl<'a> InstructionBuilder for ExtractElement<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildExtractElement(
                builder.as_raw(),
                self.vector.as_raw(),
                self.index.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `extractelement` instruction extracts a single scalar element from a vector at a specified index.
#[macro_export]
macro_rules! extract_element {
    ($vector:expr, $index:expr, $name:expr) => ({
        $crate::ops::ExtractElement::new($vector.into(), $index.into(), $name.into())
    })
}

/// This instruction inserts a single (scalar) element into a VectorType value
#[derive(Clone, Debug, PartialEq)]
pub struct InsertElement<'a> {
    vector: ValueRef,
    element: ValueRef,
    index: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> InsertElement<'a> {
    pub fn new(vector: ValueRef, element: ValueRef, index: ValueRef, name: Cow<'a, str>) -> Self {
        InsertElement {
            vector,
            element,
            index,
            name,
        }
    }
}

impl<'a> InstructionBuilder for InsertElement<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildInsertElement(
                builder.as_raw(),
                self.vector.as_raw(),
                self.element.as_raw(),
                self.index.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `insertelement` instruction inserts a scalar element into a vector at a specified index.
#[macro_export]
macro_rules! insert_element {
    ($vector:expr, $element:expr, $index:expr, $name:expr) => ({
        $crate::ops::InsertElement::new($vector.into(), $element.into(), $index.into(), $name.into())
    })
}

/// This instruction constructs a fixed permutation of two input vectors.
#[derive(Clone, Debug, PartialEq)]
pub struct ShuffleVector<'a> {
    v1: ValueRef,
    v2: ValueRef,
    mask: ValueRef,
    name: Cow<'a, str>,
}

impl<'a> ShuffleVector<'a> {
    pub fn new(v1: ValueRef, v2: ValueRef, mask: ValueRef, name: Cow<'a, str>) -> Self {
        ShuffleVector { v1, v2, mask, name }
    }
}

impl<'a> InstructionBuilder for ShuffleVector<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildShuffleVector(
                builder.as_raw(),
                self.v1.as_raw(),
                self.v2.as_raw(),
                self.mask.as_raw(),
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `shufflevector` instruction constructs a permutation of elements from two input vectors,
/// returning a vector with the same element type as the input and length that is the same as the shuffle mask.
#[macro_export]
macro_rules! shuffle_vector {
    ($v1:expr, $v2:expr, $mask:expr, $name:expr) => ({
        $crate::ops::ShuffleVector::new($v1.into(), $v2.into(), $mask.into(), $name.into())
    })
}

/// This instruction extracts a struct member or array element value from an aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct ExtractValue<'a> {
    aggregate: ValueRef,
    index: u32,
    name: Cow<'a, str>,
}

impl<'a> ExtractValue<'a> {
    pub fn new(aggregate: ValueRef, index: u32, name: Cow<'a, str>) -> Self {
        ExtractValue {
            aggregate,
            index,
            name,
        }
    }
}

impl<'a> InstructionBuilder for ExtractValue<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildExtractValue(
                builder.as_raw(),
                self.aggregate.as_raw(),
                self.index,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `extractvalue` instruction extracts the value of a member field from an aggregate value.
#[macro_export]
macro_rules! extract_value {
    ($vector:expr, $index:expr, $name:expr) => ({
        $crate::ops::ExtractValue::new($vector.into(), $index, $name.into())
    })
}

/// This instruction inserts a struct field of array element value into an aggregate value.
#[derive(Clone, Debug, PartialEq)]
pub struct InsertValue<'a> {
    aggregate: ValueRef,
    element: ValueRef,
    index: u32,
    name: Cow<'a, str>,
}

impl<'a> InsertValue<'a> {
    pub fn new(aggregate: ValueRef, element: ValueRef, index: u32, name: Cow<'a, str>) -> Self {
        InsertValue {
            aggregate,
            element,
            index,
            name,
        }
    }
}

impl<'a> InstructionBuilder for InsertValue<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildInsertValue(
                builder.as_raw(),
                self.aggregate.as_raw(),
                self.element.as_raw(),
                self.index,
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

/// The `insertvalue` instruction inserts a value into a member field in an aggregate value.
#[macro_export]
macro_rules! insert_value {
    ($vector:expr, $element:expr, $index:expr, $name:expr) => ({
        $crate::ops::InsertValue::new($vector.into(), $element.into(), $index, $name.into())
    })
}

define_unary_instruction!(
    IsNull,
    LLVMBuildIsNull,
    is_null,
    "Determine whether a value instance is null."
);
define_unary_instruction!(
    IsNotNull,
    LLVMBuildIsNotNull,
    is_not_null,
    "Determine whether a value instance is not null."
);
define_binary_operator!(
    PtrDiff,
    LLVMBuildPtrDiff,
    ptr_diff,
    "Return the i64 difference between two pointer values, dividing out the size of the pointed-to objects."
);

#[derive(Clone, Debug, PartialEq)]
pub struct Fence<'a> {
    ordering: u32, // TODO: LLVMAtomicOrdering
    single_thread: bool,
    name: Cow<'a, str>,
}

impl<'a> Fence<'a> {
    pub fn new(ordering: LLVMAtomicOrdering, single_thread: bool, name: Cow<'a, str>) -> Self {
        Fence {
            ordering: ordering as u32,
            single_thread,
            name,
        }
    }
}

impl<'a> InstructionBuilder for Fence<'a> {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildFence(
                builder.as_raw(),
                mem::transmute(self.ordering),
                if self.single_thread { 1 } else { 0 },
                unchecked_cstring(self.name.clone()).as_ptr(),
            )
        }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicRMW {
    op: u32, // TODO: LLVMAtomicRMWBinOp
    ptr: ValueRef,
    value: ValueRef,
    ordering: u32, // TODO: LLVMAtomicOrdering
    single_thread: bool,
}

impl AtomicRMW {
    pub fn new(
        op: LLVMAtomicRMWBinOp,
        ptr: ValueRef,
        value: ValueRef,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Self {
        AtomicRMW {
            op: op as u32,
            ptr,
            value,
            ordering: ordering as u32,
            single_thread,
        }
    }
}

impl InstructionBuilder for AtomicRMW {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildAtomicRMW(
                builder.as_raw(),
                mem::transmute(self.op),
                self.ptr.as_raw(),
                self.value.as_raw(),
                mem::transmute(self.ordering),
                if self.single_thread { 1 } else { 0 },
            )
        }.into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicCmpXchg {
    ptr: ValueRef,
    cmp: ValueRef,
    new: ValueRef,
    success_ordering: u32, // TODO: LLVMAtomicOrdering
    failure_ordering: u32, // TODO: LLVMAtomicOrdering
    single_thread: bool,
}

impl AtomicCmpXchg {
    pub fn new(
        ptr: ValueRef,
        cmp: ValueRef,
        new: ValueRef,
        success_ordering: LLVMAtomicOrdering,
        failure_ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> Self {
        AtomicCmpXchg {
            ptr,
            cmp,
            new,
            success_ordering: success_ordering as u32,
            failure_ordering: failure_ordering as u32,
            single_thread,
        }
    }
}

impl InstructionBuilder for AtomicCmpXchg {
    fn emit_to(&self, builder: &IRBuilder) -> Instruction {
        unsafe {
            LLVMBuildAtomicCmpXchg(
                builder.as_raw(),
                self.ptr.as_raw(),
                self.cmp.as_raw(),
                self.new.as_raw(),
                mem::transmute(self.success_ordering),
                mem::transmute(self.failure_ordering),
                if self.single_thread { 1 } else { 0 },
            )
        }.into()
    }
}

/// ‘fence‘ instructions take an ordering argument which defines what synchronizes-with edges they add.
/// They can only be given acquire, release, acq_rel, and seq_cst orderings.
#[macro_export]
macro_rules! fence_ordering {
    (acquire) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire);
    (release) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingRelease);
    (acq_rel) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease);
    (seq_cst) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent);
    ($ordering:ident) => ("`fence` instruction can only be given acquire, release, acq_rel, and seq_cst orderings.");
}

/// The ‘fence‘ instruction is used to introduce happens-before edges between operations.
///
/// ‘fence‘ instructions take an ordering argument which defines what synchronizes-with edges they add.
/// They can only be given acquire, release, acq_rel, and seq_cst orderings.
#[macro_export]
macro_rules! fence {
    (singlethread $ordering:ident ; $name:expr) => (
        $crate::ops::Fence::new(
            fence_ordering!($ordering),
            true,
            $name.into(),
        )
    );
    ($ordering:ident ; $name:expr) => (
        $crate::ops::Fence::new(
            fence_ordering!($ordering),
            false,
            $name.into(),
        )
    );
}

#[macro_export]
macro_rules! atomic_ordering {
    (not_atomic) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic);
    (unordered) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingUnordered);
    (monotonic) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic);
    (acquire) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire);
    (release) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingRelease);
    (acq_rel) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease);
    (seq_cst) => (::llvm::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent);
    ($ordering:ident) => ("unknown atomic ordering");
}

#[macro_export]
macro_rules! atomic {
    (xchg $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXchg,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (add $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (sub $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (and $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAnd,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (nand $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpNand,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (or $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpOr,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (xor $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXor,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (max $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMax,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (min $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMin,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (xor $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXor,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (umax $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMax,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (umin $ptr:expr, $value:expr; $ordering:ident) => (
        $crate::ops::AtomicRMW::new(
            ::llvm::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMin,
            $ptr.into(),
            $value.into(),
            atomic_ordering!($ordering),
            false
        )
    );
    (cmpxchg $ptr:expr, $cmp:expr, $new:expr; $success_ordering:ident $failure_ordering:ident) => (
        $crate::ops::AtomicCmpXchg::new(
            $ptr.into(),
            $cmp.into(),
            $new.into(),
            atomic_ordering!($success_ordering),
            atomic_ordering!($failure_ordering),
            false
        )
    )
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

    pub fn emit<I: InstructionBuilder + fmt::Debug>(&self, inst: I) -> Instruction {
        trace!("{:?} emit: {:?}", self, inst);

        inst.emit_to(self)
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

        assert_eq!(ret!().emit_to(&builder).to_string().trim(), "ret void");
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

        assert_eq!(
            ret!(i64t.uint(123)).emit_to(&builder).to_string().trim(),
            "ret i64 123"
        );
    }

    #[test]
    fn aggregate_ret() {
        let context = Context::new();
        let module = Module::with_name_in_context("aggregate_ret", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let f64t = context.double();
        let ret = context.annonymous_struct(&[i64t, f64t], false);
        let function_type = FunctionType::new(ret.into(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        assert_eq!(
            ret!(i64t.uint(123), f64t.real(456f64))
                .emit_to(&builder)
                .to_string()
                .trim(),
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

        // unconditional branch
        let next = function.append_basic_block_in_context("next", &context);

        assert_eq!(
            br!(next).emit_to(&builder).to_string().trim(),
            "br label %next"
        );

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

        // indirect branch

        assert_eq!(
            br!(bb_then.addr() => [bb_then, bb_else])
                .emit_to(&builder)
                .to_string()
                .trim(),
            "indirectbr i8* blockaddress(@test, %then), [label %then, label %else]"
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

        let switch =
            switch!(i64t.uint(3);
                _ => function.append_basic_block_in_context("default", &context),
                i64t.uint(1) => function.append_basic_block_in_context("one", &context),
                i64t.uint(2) => function.append_basic_block_in_context("two", &context),
                i64t.uint(3) => function.append_basic_block_in_context("three", &context)
            );

        assert_eq!(
            switch.emit_to(&builder).to_string().trim(),
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

        assert_eq!(
            invoke!(fn_hello => "ret"; to bb_normal; unwind bb_catch; [i64t.uint(123), i64t.int(456)]).emit_to(&builder).to_string().trim(),
            r#"%ret = invoke i64 @hello(i64 123, i64 456)
          to label %normal unwind label %catch"#
        );

        assert_eq!(
            resume!(i64t.uint(123)).emit_to(&builder).to_string().trim(),
            "resume i64 123"
        );
        assert_eq!(
            Unreachable.emit_to(&builder).to_string().trim(),
            "unreachable"
        );
    }

    macro_rules! test_instruction {
        ($builder:ident, $name:ident !( $arg0_i64:ident ), $display:expr) => (
            assert_eq!( $name !( $arg0_i64, stringify!($name) ).emit_to(& $builder).to_string().trim(), $display )
        );
        ($builder:ident, $name:ident !( $arg0_i64:ident, $arg1_i64:ident ), $display:expr) => (
            assert_eq!( $name !( $arg0_i64, $arg1_i64, stringify!($name) ).emit_to(& $builder).to_string().trim(), $display )
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

    #[test]
    fn vector() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let function_type = FunctionType::new(
            context.void(),
            &[i64t.vector(3).into(), i64t.vector(3).into()],
            false,
        );
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_vector = function.get_param(0).unwrap();
        let arg1_vector = function.get_param(1).unwrap();

        let idx = i64t.int(1);

        assert_eq!(
            extract_element!(arg0_vector, idx, "extract_element")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_element = extractelement <3 x i64> %0, i64 1"
        );

        assert_eq!(
            insert_element!(arg0_vector, i64t.int(10), idx, "insert_element")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_element = insertelement <3 x i64> %0, i64 10, i64 1"
        );

        let i32t = context.int32();
        let mask = vector![i32t.int(1), i32t.int(0), i32t.int(2)];

        assert_eq!(
            shuffle_vector!(arg0_vector, arg1_vector, mask, "shuffle_vector")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%shuffle_vector = shufflevector <3 x i64> %0, <3 x i64> %1, <3 x i32> <i32 1, i32 0, i32 2>"
        );
    }

    #[test]
    fn aggregate() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let i32t = context.int32();
        let i64t = context.int64();
        let array_t = i64t.array(4).into();
        let struct_t = context.annonymous_struct(&[i32t, i64t], false);
        let function_type = FunctionType::new(context.void(), &[array_t, struct_t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_array = function.get_param(0).unwrap();
        let arg1_struct = function.get_param(1).unwrap();

        assert_eq!(
            extract_value!(arg0_array, 1, "extract_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_value = extractvalue [4 x i64] %0, 1"
        );

        assert_eq!(
            extract_value!(arg1_struct, 1, "extract_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%extract_value1 = extractvalue { i32, i64 } %1, 1"
        );

        assert_eq!(
            insert_value!(arg0_array, i64t.int(123), 1, "insert_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_value = insertvalue [4 x i64] %0, i64 123, 1"
        );

        assert_eq!(
            insert_value!(arg1_struct, i64t.int(123), 1, "insert_value")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%insert_value2 = insertvalue { i32, i64 } %1, i64 123, 1"
        );
    }

    #[test]
    fn misc() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let p_i64t = i64t.ptr();

        let function_type =
            FunctionType::new(context.void(), &[p_i64t.into(), p_i64t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_p_i64 = function.get_param(0).unwrap();
        let arg1_p_i64 = function.get_param(1).unwrap();

        assert_eq!(
            is_null!(arg0_p_i64, "is_null")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%is_null = icmp eq i64* %0, null"
        );

        assert_eq!(
            is_not_null!(arg0_p_i64, "is_not_null")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%is_not_null = icmp ne i64* %0, null"
        );

        ptr_diff!(arg0_p_i64, arg1_p_i64, "ptr_diff").emit_to(&builder);

        assert_eq!(
            last_instructions(bb, 4),
            vec![
                "%2 = ptrtoint i64* %0 to i64",
                "%3 = ptrtoint i64* %1 to i64",
                "%4 = sub i64 %2, %3",
                "%ptr_diff = sdiv exact i64 %4, ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)",
            ]
        );
    }

    fn last_instructions(bb: BasicBlock, n: usize) -> Vec<String> {
        let mut insts = bb.instructions()
            .rev()
            .take(n)
            .map(|i| i.to_string().trim().to_owned())
            .collect::<Vec<String>>();

        insts.reverse();

        insts
    }

    #[test]
    fn memory() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let p_i64t = i64t.ptr();

        let function_type = FunctionType::new(context.void(), &[p_i64t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_p_i64 = function.get_param(0).unwrap();

        let p = malloc!(i64t, "malloc").emit_to(&builder);

        free!(p).emit_to(&builder);

        assert_eq!(
            last_instructions(bb, 4),
            vec![
                "%malloccall = tail call i8* @malloc(i32 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32))",
                "%malloc = bitcast i8* %malloccall to i64*",
                "%1 = bitcast i64* %malloc to i8*",
                "tail call void @free(i8* %1)",
            ]
        );

        malloc!(i64t, i64t.int(123), "array_malloc").emit_to(&builder);

        assert_eq!(
            last_instructions(bb, 4),
            vec![
                "%2 = trunc i64 123 to i32",
                "%mallocsize = mul i32 %2, ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32)",
                "%malloccall1 = tail call i8* @malloc(i32 %mallocsize)",
                "%array_malloc = bitcast i8* %malloccall1 to i64*",
            ]
        );

        assert_eq!(
            alloca!(i64t, "alloca").emit_to(&builder).to_string().trim(),
            "%alloca = alloca i64"
        );

        assert_eq!(
            alloca!(i64t, i64t.int(123), "array_alloca")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%array_alloca = alloca i64, i64 123"
        );

        assert_eq!(
            load!(arg0_p_i64, "load")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%load = load i64, i64* %0"
        );

        assert_eq!(
            store!(i64t.int(123), arg0_p_i64)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "store i64 123, i64* %0"
        );

        assert_eq!(
            global_str!("global_str", "global_str")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "@global_str = private unnamed_addr constant [11 x i8] c\"global_str\\00\""
        );

        assert_eq!(
            global_str_ptr!("global_str_ptr", "global_str_ptr")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "i8* getelementptr inbounds ([15 x i8], [15 x i8]* @global_str_ptr, i32 0, i32 0)"
        );

        assert_eq!(
            module.global_vars().rev().next().unwrap().to_string(),
            "@global_str_ptr = private unnamed_addr constant [15 x i8] c\"global_str_ptr\\00\""
        )
    }

    #[test]
    fn gep() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i32t = context.int32();
        let i64t = context.int64();
        let array_t = i64t.array(4);
        let vector_t = i64t.vector(4);
        let struct_t = context.named_struct("struct", &[i32t, i64t, vector_t.into()], false);

        let p_array = alloca!(array_t, "p_array").emit_to(&builder);
        let p_vector = alloca!(vector_t, "p_vector").emit_to(&builder);
        let p_struct = alloca!(struct_t, "p_struct").emit_to(&builder);

        assert_eq!(
            gep!(p_array, [i64t.int(1)], "gep")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%gep = getelementptr [4 x i64], [4 x i64]* %p_array, i64 1"
        );

        assert_eq!(
            gep!(inbounds p_vector, [i64t.int(1)], "inbounds_gep")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%inbounds_gep = getelementptr inbounds <4 x i64>, <4 x i64>* %p_vector, i64 1"
        );

        assert_eq!(
            gep!(structure p_struct, 1, "struct_gep")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%struct_gep = getelementptr inbounds %struct, %struct* %p_struct, i32 0, i32 1"
        );

        assert_eq!(
            gep!(inbounds p_struct, [i32t.int(2), i32t.int(1)], "inbounds_gep")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%inbounds_gep1 = getelementptr inbounds %struct, %struct* %p_struct, i32 2, i32 1"
        );
    }

    macro_rules! test_icmp {
        ($builder:expr, $pred:ident !( $lhs:expr, $rhs:expr)) => ({
            assert_eq!(
                icmp!($pred $lhs, $rhs; format!("icmp_{}", stringify!($pred)))
                    .emit_to(& $builder)
                    .to_string()
                    .trim(),
                format!("%icmp_{0} = icmp {0} i64* %lhs, %rhs", stringify!($pred))
            )
        })
    }

    macro_rules! test_fcmp {
        ($builder:expr, $pred:ident !( $lhs:expr, $rhs:expr)) => ({
            assert_eq!(
                fcmp!($pred $lhs, $rhs; format!("fcmp_{}", stringify!($pred)))
                    .emit_to(& $builder)
                    .to_string()
                    .trim(),
                format!("%fcmp_{0} = fcmp {0} i64* %lhs, %rhs", stringify!($pred))
            )
        })
    }

    #[test]
    fn cmp() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let function_type = FunctionType::new(context.void(), &[], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let i64t = context.int64();
        let lhs = alloca!(i64t, "lhs").emit_to(&builder);
        let rhs = alloca!(i64t, "rhs").emit_to(&builder);

        test_icmp!(builder, eq!(lhs, rhs));
        test_icmp!(builder, ne!(lhs, rhs));
        test_icmp!(builder, ugt!(lhs, rhs));
        test_icmp!(builder, uge!(lhs, rhs));
        test_icmp!(builder, ult!(lhs, rhs));
        test_icmp!(builder, ule!(lhs, rhs));
        test_icmp!(builder, sgt!(lhs, rhs));
        test_icmp!(builder, sge!(lhs, rhs));
        test_icmp!(builder, slt!(lhs, rhs));
        test_icmp!(builder, sle!(lhs, rhs));

        test_fcmp!(builder, oeq!(lhs, rhs));
        test_fcmp!(builder, ogt!(lhs, rhs));
        test_fcmp!(builder, oge!(lhs, rhs));
        test_fcmp!(builder, olt!(lhs, rhs));
        test_fcmp!(builder, ole!(lhs, rhs));
        test_fcmp!(builder, one!(lhs, rhs));
        test_fcmp!(builder, ord!(lhs, rhs));
        test_fcmp!(builder, uno!(lhs, rhs));
        test_fcmp!(builder, ueq!(lhs, rhs));
        test_fcmp!(builder, ugt!(lhs, rhs));
        test_fcmp!(builder, uge!(lhs, rhs));
        test_fcmp!(builder, ult!(lhs, rhs));
        test_fcmp!(builder, ule!(lhs, rhs));
        test_fcmp!(builder, une!(lhs, rhs));
    }

    macro_rules! test_atomic {
        ($builder:expr, atomic !( $op:ident $ptr:expr, $value:expr ; $ordering:ident ), $display:expr) => (
            assert_eq!(
                atomic!($op $ptr, $value ; $ordering)
                    .emit_to(& $builder)
                    .to_string()
                    .trim(),
                $display
            )
        )
    }

    #[test]
    fn atomic() {
        let context = Context::new();
        let module = Module::with_name_in_context("br", &context);
        let builder = IRBuilder::within_context(&context);

        let i64t = context.int64();
        let p_i64t = i64t.ptr();

        let function_type = FunctionType::new(context.void(), &[p_i64t.into()], false);
        let function = module.add_function("test", function_type);

        let bb = function.append_basic_block_in_context("entry", &context);
        builder.position(Position::AtEnd(bb));

        let arg0_p_i64 = function.get_param(0).unwrap();

        test_atomic!(
            builder,
            atomic!(xchg arg0_p_i64, i64t.int(123); not_atomic),
            "%1 = atomicrmw xchg i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(add arg0_p_i64, i64t.int(123); unordered),
            "%2 = atomicrmw add i64* %0, i64 123 unordered"
        );
        test_atomic!(
            builder,
            atomic!(sub arg0_p_i64, i64t.int(123); monotonic),
            "%3 = atomicrmw sub i64* %0, i64 123 monotonic"
        );
        test_atomic!(
            builder,
            atomic!(and arg0_p_i64, i64t.int(123); acquire),
            "%4 = atomicrmw and i64* %0, i64 123 acquire"
        );
        test_atomic!(
            builder,
            atomic!(nand arg0_p_i64, i64t.int(123); release),
            "%5 = atomicrmw nand i64* %0, i64 123 release"
        );
        test_atomic!(
            builder,
            atomic!(or arg0_p_i64, i64t.int(123); acq_rel),
            "%6 = atomicrmw or i64* %0, i64 123 acq_rel"
        );
        test_atomic!(
            builder,
            atomic!(xor arg0_p_i64, i64t.int(123); seq_cst),
            "%7 = atomicrmw xor i64* %0, i64 123 seq_cst"
        );
        test_atomic!(
            builder,
            atomic!(max arg0_p_i64, i64t.int(123); not_atomic),
            "%8 = atomicrmw max i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(min arg0_p_i64, i64t.int(123); not_atomic),
            "%9 = atomicrmw min i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(umax arg0_p_i64, i64t.int(123); not_atomic),
            "%10 = atomicrmw umax i64* %0, i64 123"
        );
        test_atomic!(
            builder,
            atomic!(umin arg0_p_i64, i64t.int(123); not_atomic),
            "%11 = atomicrmw umin i64* %0, i64 123"
        );

        assert_eq!(
            fence!(acquire; "fence")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%fence = fence acquire"
        );
        assert_eq!(
            fence!(singlethread acq_rel; "fence")
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%fence1 = fence singlethread acq_rel"
        );

        assert_eq!(
            atomic!(cmpxchg arg0_p_i64, i64t.int(123), i64t.int(456); unordered unordered)
                .emit_to(&builder)
                .to_string()
                .trim(),
            "%12 = cmpxchg i64* %0, i64 123, i64 456 unordered unordered"
        );
    }
}
