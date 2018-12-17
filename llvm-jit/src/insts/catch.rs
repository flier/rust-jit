use std::borrow::Cow;
use std::ptr;

use crate::llvm::core::*;
use crate::llvm::prelude::*;

use crate::block::BasicBlock;
use crate::insts::{IRBuilder, InstructionBuilder, TerminatorInst};
use crate::utils::{AsRaw, IntoRaw};
use crate::value::{Instruction, ValueRef};

/// The `catchswitch` instruction is used by LLVM’s exception handling system to describe
/// the set of possible catch handlers that may be executed by the EH personality routine.
#[derive(Clone, Debug, PartialEq)]
pub struct CatchSwitch<'a> {
    parent: Option<CatchPadInst>,
    unwind: Option<BasicBlock>,
    name: Cow<'a, str>,
    handlers: Vec<BasicBlock>,
}

/// The `catchswitch` instruction is used by LLVM’s exception handling system to describe
/// the set of possible catch handlers that may be executed by the EH personality routine.
pub fn catchswitch<'a, N, I, B>(
    parent: Option<CatchPadInst>,
    unwind: Option<BasicBlock>,
    name: N,
    handlers: I,
) -> CatchSwitch<'a>
where
    N: Into<Cow<'a, str>>,
    I: IntoIterator<Item = B>,
    B: Into<BasicBlock>,
{
    CatchSwitch {
        parent,
        unwind,
        name: name.into(),
        handlers: handlers.into_iter().map(|h| h.into()).collect(),
    }
}

#[macro_export]
macro_rules! catchswitch {
    (within none [ $( label $handler:expr ),* ] unwind label $unwind:ident; $name: expr) => {
        $crate::insts::catchswitch(None, Some($unwind), $name, vec![ $( $handler ),* ])
    };
    (within none [ $( label $handler:expr ),* ] unwind to caller; $name: expr) => {
        $crate::insts::catchswitch(None, None, $name, vec![ $( $handler ),* ])
    };
    (within $parent:ident [ $( label $handler:expr ),* ] unwind label $unwind:ident; $name: expr) => {
        $crate::insts::catchswitch(Some($parent), Some($unwind), $name, vec![ $( $handler ),* ])
    };
    (within $parent:ident [ $( label $handler:expr ),* ] unwind to caller; $name: expr) => {
        $crate::insts::catchswitch(Some($parent), None, $name, vec![ $( $handler ),* ])
    };

    (within none [ $( label $handler:expr ),* ] unwind label $unwind:ident) => {
        catchswitch!(within none [ $( label $handler ),* ] unwind label $unwind; "catchswitch")
    };
    (within none [ $( label $handler:expr ),* ] unwind to caller) => {
        catchswitch!(within none [ $( label $handler ),* ] unwind to caller; "catchswitch")
    };
    (within $parent:ident [ $( label $handler:expr ),* ] unwind label $unwind:ident) => {
        catchswitch!(within $parent [ $( label $handler ),* ] unwind label $unwind; "catchswitch")
    };
    (within $parent:ident [ $( label $handler:expr ),* ] unwind to caller) => {
        catchswitch!(within $parent [ $( label $handler ),* ] unwind to caller; "catchswitch")
    };
}

impl<'a> CatchSwitch<'a> {
    /// Add a destination to the `catchswitch` instruction
    pub fn add_handler<B: Into<BasicBlock>>(&mut self, handler: B) -> &mut Self {
        self.handlers.push(handler.into());
        self
    }
}

impl<'a> InstructionBuilder for CatchSwitch<'a> {
    type Target = CatchSwitchInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        let catch_switch: CatchSwitchInst = unsafe {
            LLVMBuildCatchSwitch(
                builder.as_raw(),
                self.parent.map_or(ptr::null_mut(), |catchpad| catchpad.into_raw()),
                self.unwind.map_or(ptr::null_mut(), |bb| bb.into_raw()),
                self.handlers.len() as u32,
                cstr!(self.name),
            )
        }
        .into();

        for handler in self.handlers {
            unsafe { LLVMAddHandler(catch_switch.as_raw(), handler.into_raw()) }
        }

        catch_switch
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CatchSwitchInst(Instruction);

inherit_from!(CatchSwitchInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst for CatchSwitchInst {}

impl CatchSwitchInst {
    /// Add a destination to the `catchswitch` instruction
    pub fn add_handler<B: AsRaw<RawType = LLVMBasicBlockRef>>(&self, handler: B) -> &Self {
        unsafe { LLVMAddHandler(self.as_raw(), handler.as_raw()) };
        self
    }

    /// Obtain the basic blocks acting as handlers for a `catchswitch` instruction.
    pub fn handlers(&self) -> Vec<BasicBlock> {
        let len = unsafe { LLVMGetNumHandlers(self.as_raw()) };
        let mut handlers = vec![ptr::null_mut(); len as usize];
        unsafe { LLVMGetHandlers(self.as_raw(), handlers.as_mut_ptr()) };

        handlers.into_iter().map(|h| h.into()).collect()
    }
}

/// The `catchpad` instruction is used by LLVM’s exception handling system to specify
/// that a basic block begins a catch handler —
/// one where a personality routine attempts to transfer control to catch an exception.
#[derive(Clone, Debug, PartialEq)]
pub struct CatchPad<'a> {
    parent: CatchSwitchInst,
    args: Vec<ValueRef>,
    name: Cow<'a, str>,
}

#[macro_export]
macro_rules! catchpad {
    (within $parent:ident [ $( $arg:expr ),* ] ; $name:expr) => {
        $crate::insts::catchpad($parent, vec![ $( $arg ),* ], $name)
    };
    (within $parent:ident [ $( $arg:expr ),* ]) => {
        catchpad!( within $parent [ $( $arg ),* ] ; "catchpad" )
    };
}

/// The `catchpad` instruction is used by LLVM’s exception handling system to specify
/// that a basic block begins a catch handler —
/// one where a personality routine attempts to transfer control to catch an exception.
pub fn catchpad<'a, T, I, N>(parent: T, args: I, name: N) -> CatchPad<'a>
where
    T: Into<CatchSwitchInst>,
    I: IntoIterator<Item = ValueRef>,
    N: Into<Cow<'a, str>>,
{
    CatchPad {
        parent: parent.into(),
        args: args.into_iter().collect(),
        name: name.into(),
    }
}

impl<'a> InstructionBuilder for CatchPad<'a> {
    type Target = CatchPadInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        let mut args = self.args.into_iter().map(|arg| arg.into_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMBuildCatchPad(
                builder.as_raw(),
                self.parent.into_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                cstr!(self.name),
            )
        }
        .into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CatchPadInst(Instruction);

inherit_from!(CatchPadInst, Instruction, ValueRef, LLVMValueRef);

impl CatchPadInst {
    /// Get the parent `catchswitch` instruction of a catchpad instruction.
    pub fn parent(&self) -> CatchSwitchInst {
        unsafe { LLVMGetParentCatchSwitch(self.as_raw()) }.into()
    }

    /// Set the parent `catchswitch` instruction of a catchpad instruction.
    pub fn set_parent(&self, parent: &CatchSwitchInst) {
        unsafe { LLVMSetParentCatchSwitch(self.as_raw(), parent.as_raw()) }
    }
}

/// The `cleanuppad` instruction is used by LLVM’s exception handling system
/// to specify that a basic block is a cleanup block —
/// one where a personality routine attempts to transfer control to run cleanup actions.
#[derive(Clone, Debug, PartialEq)]
pub struct CleanupPad<'a> {
    parent: CatchSwitchInst,
    args: Vec<ValueRef>,
    name: Cow<'a, str>,
}

/// The `cleanuppad` instruction is used by LLVM’s exception handling system
/// to specify that a basic block is a cleanup block —
/// one where a personality routine attempts to transfer control to run cleanup actions.
pub fn cleanuppad<'a, T, I, N>(parent: T, args: I, name: N) -> CleanupPad<'a>
where
    T: Into<CatchSwitchInst>,
    I: IntoIterator<Item = ValueRef>,
    N: Into<Cow<'a, str>>,
{
    CleanupPad {
        parent: parent.into(),
        args: args.into_iter().map(|v| v.into()).collect(),
        name: name.into(),
    }
}

impl<'a> InstructionBuilder for CleanupPad<'a> {
    type Target = Instruction;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        let mut args = self.args.into_iter().map(|arg| arg.into_raw()).collect::<Vec<_>>();

        unsafe {
            LLVMBuildCleanupPad(
                builder.as_raw(),
                self.parent.into_raw(),
                args.as_mut_ptr(),
                args.len() as u32,
                cstr!(self.name),
            )
        }
        .into()
    }
}

/// The `catchret` instruction is a terminator instruction that has a single successor.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CatchRet {
    catchpad: CatchPadInst,
    next: BasicBlock,
}

#[macro_export]
macro_rules! catchret {
    (from $catchpad:ident to label $next:ident) => {
        $crate::insts::catchret($catchpad, $next)
    };
}

/// The `catchret` instruction is a terminator instruction that has a single successor.
pub fn catchret<T, B>(catchpad: T, next: B) -> CatchRet
where
    T: Into<CatchPadInst>,
    B: Into<BasicBlock>,
{
    CatchRet {
        catchpad: catchpad.into(),
        next: next.into(),
    }
}

impl InstructionBuilder for CatchRet {
    type Target = CatchRetInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe { LLVMBuildCatchRet(builder.as_raw(), self.catchpad.into_raw(), self.next.as_raw()) }.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CatchRetInst(Instruction);

inherit_from!(CatchRetInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst for CatchRetInst {}

/// The `cleanupret` instruction is a terminator instruction that has an optional successor.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CleanupRet {
    catchpad: CatchPadInst,
    next: Option<BasicBlock>,
}

#[macro_export]
macro_rules! cleanupret {
    (from $catchpad:ident to label $next:ident) => {
        $crate::insts::cleanupret($catchpad, $next)
    };
}

/// The `cleanupret` instruction is a terminator instruction that has an optional successor.
pub fn cleanupret<T, B>(catchpad: T, next: Option<B>) -> CleanupRet
where
    T: Into<CatchPadInst>,
    B: Into<BasicBlock>,
{
    CleanupRet {
        catchpad: catchpad.into(),
        next: next.map(|b| b.into()),
    }
}

impl InstructionBuilder for CleanupRet {
    type Target = CleanupRetInst;

    fn emit_to(self, builder: &IRBuilder) -> Self::Target {
        trace!("{:?} emit instruction: {:?}", builder, self);

        unsafe {
            LLVMBuildCleanupRet(
                builder.as_raw(),
                self.catchpad.into_raw(),
                self.next.map_or(ptr::null_mut(), |bb| bb.as_raw()),
            )
        }
        .into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CleanupRetInst(Instruction);

inherit_from!(CleanupRetInst, Instruction, ValueRef, LLVMValueRef);

impl TerminatorInst for CleanupRetInst {}

impl IRBuilder {
    /// The `catchswitch` instruction is used by LLVM’s exception handling system to describe
    /// the set of possible catch handlers that may be executed by the EH personality routine.
    pub fn catchswitch<'a, N, I, B>(
        &self,
        parent: Option<CatchPadInst>,
        unwind: Option<BasicBlock>,
        name: N,
        handlers: I,
    ) -> CatchSwitchInst
    where
        N: Into<Cow<'a, str>>,
        I: IntoIterator<Item = B>,
        B: Into<BasicBlock>,
    {
        catchswitch(parent, unwind, name, handlers).emit_to(self)
    }

    /// The `catchpad` instruction is used by LLVM’s exception handling system to specify
    /// that a basic block begins a catch handler —
    /// one where a personality routine attempts to transfer control to catch an exception.
    pub fn catchpad<'a, T, I, N>(&self, parent: T, args: I, name: N) -> CatchPadInst
    where
        T: Into<CatchSwitchInst>,
        I: IntoIterator<Item = ValueRef>,
        N: Into<Cow<'a, str>>,
    {
        catchpad(parent, args, name).emit_to(self)
    }

    /// The `cleanuppad` instruction is used by LLVM’s exception handling system
    /// to specify that a basic block is a cleanup block —
    /// one where a personality routine attempts to transfer control to run cleanup actions.
    pub fn cleanuppad<'a, T, I, V, N>(&self, parent: T, args: I, name: N) -> Instruction
    where
        T: Into<CatchSwitchInst>,
        I: IntoIterator<Item = ValueRef>,
        N: Into<Cow<'a, str>>,
    {
        cleanuppad(parent, args, name).emit_to(self)
    }

    /// The `catchret` instruction is a terminator instruction that has a single successor.
    pub fn catchret<T, B>(&self, catchpad: T, next: B) -> CatchRetInst
    where
        T: Into<CatchPadInst>,
        B: Into<BasicBlock>,
    {
        catchret(catchpad, next).emit_to(self)
    }

    /// The `cleanupret` instruction is a terminator instruction that has an optional successor.
    pub fn cleanupret<T, B>(&self, catchpad: T, next: Option<B>) -> CleanupRetInst
    where
        T: Into<CatchPadInst>,
        B: Into<BasicBlock>,
    {
        cleanupret(catchpad, next).emit_to(self)
    }
}

// #[cfg(test)]
// mod tests {
//     use insts::*;
//     use prelude::*;

//     #[test]
//     fn catch_switch() {
//         let c = Context::new();
//         let m = c.create_module("terminator");
//         let b = c.create_builder();

//         let i8_t = c.int8_t();
//         let i8_ptr_t = i8_t.ptr_t();
//         let cxx_throw_exception = m.add_function(
//             "_CxxThrowException",
//             FunctionType::new(c.void_t(), types![i8_ptr_t], false),
//         );
//         c.create_builder().within(
//             cxx_throw_exception.append_basic_block_in_context("entry", &c),
//             || ret!(),
//         );

//         cxx_throw_exception.verify().unwrap();

//         let test = m.add_function("test", FunctionType::new(c.void_t(), &[], false));

//         let entry_bb = test.append_basic_block_in_context("entry", &c);
//         let catch_dispatch_bb = test.append_basic_block_in_context("catch.dispatch", &c);
//         let catch_bb = test.append_basic_block_in_context("catch", &c);
//         let catch_dispatch2_bb = test.append_basic_block_in_context("catch.dispatch2", &c);
//         let catch3_bb = test.append_basic_block_in_context("catch", &c);
//         let try_cont_bb = test.append_basic_block_in_context("try.cont", &c);
//         let try_cont6_bb = test.append_basic_block_in_context("try.cont6", &c);
//         let unreachable_bb = test.append_basic_block_in_context("unreachable", &c);

//         b.within(
//             entry_bb,
//             || invoke!(cxx_throw_exception(i8_ptr_t.null()) to label unreachable_bb unwind label catch_dispatch_bb),
//         );
//         let catchswitch_inst = b.within(
//             catch_dispatch_bb,
//             || catchswitch!(within none [label catch_bb] unwind to caller),
//         );
//         let (catchpad_inst, _) = b.within(catch_bb, || {
//             (
//                 catchpad!(within catchswitch_inst []),
//                 invoke!(cxx_throw_exception(i8_ptr_t.null()) to label unreachable_bb unwind label catch_dispatch2_bb),
//             )
//         });
//         let catchswitch2_inst = b.within(
//             catch_dispatch2_bb,
//             || catchswitch!(within catchpad_inst [label catch3_bb] unwind to caller),
//         );
//         let catchpad3_inst = b.within(catch3_bb, || catchpad!(within catchswitch2_inst []));
//         b.within(catch3_bb, || catchret!(from catchpad3_inst to label try_cont_bb));
//         b.within(try_cont_bb, || catchret!(from catchpad_inst to label try_cont6_bb));
//         b.within(try_cont6_bb, || ret!());
//         b.within(unreachable_bb, || unreachable());

//         test.verify().unwrap();

//         m.verify().unwrap();

//         // void f() {
//         //   try {
//         //     throw;
//         //   } catch (...) {
//         //     try {
//         //       throw;
//         //     } catch (...) {
//         //     }
//         //   }
//         // }

//         assert_eq!(m.to_string(), "");
//     }
// }
