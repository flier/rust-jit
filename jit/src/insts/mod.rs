//! The definitions of all of the subclasses of the `Instruction`.
//!
//! This is meant to be an easy way to get access to all instruction subclasses.

mod builder;
#[macro_use]
mod ret;
#[macro_use]
mod br;
#[macro_use]
mod switch;
#[macro_use]
mod terminator;
#[macro_use]
mod memory;
#[macro_use]
mod gep;
#[macro_use]
mod globalstr;
#[macro_use]
mod binop;
#[macro_use]
mod unary;
#[macro_use]
mod cast;
#[macro_use]
mod cmp;
#[macro_use]
mod phi;
#[macro_use]
mod select;
#[macro_use]
mod call;
#[macro_use]
mod vaarg;
#[macro_use]
mod vector;
#[macro_use]
mod aggregate;
#[macro_use]
mod atomic;
mod ast;
#[macro_use]
mod catch;
#[macro_use]
mod invoke;

pub use self::aggregate::{extract_value, insert_value, ExtractValue, InsertValue};
pub use self::ast::AstNode;
pub use self::atomic::{AtomicCmpXchg, AtomicRMW, Fence};
pub use self::binop::*;
pub use self::br::{Br, BranchInst, CondBr, IndirectBr};
pub use self::builder::{IRBuilder, InstructionBuilder, Position};
pub use self::call::{call, Call, CallConv, CallInst, CallSite};
pub use self::cast::*;
pub use self::catch::{
    catchpad, catchret, catchswitch, cleanuppad, cleanupret, CatchPad, CatchPadInst, CatchRet, CatchRetInst,
    CatchSwitch, CatchSwitchInst, CleanupPad, CleanupRet, CleanupRetInst,
};
pub use self::cmp::{FCmp, FCmpInst, ICmp, ICmpInst};
pub use self::gep::{GetElementPtr, GetElementPtrInst};
pub use self::globalstr::{global_str, global_str_ptr, GlobalString, GlobalStringPtr};
pub use self::invoke::{invoke, Invoke, InvokeInst};
pub use self::memory::{
    free, load, store, Alloca, AllocaInst, Free, Load, LoadInst, Malloc, MemAccessInst, Store, StoreInst,
};
pub use self::phi::{phi, Phi, PhiNode};
pub use self::ret::{Ret, ReturnInst};
pub use self::select::{select, Select};
pub use self::switch::{switch, Switch, SwitchInst};
pub use self::terminator::{
    landing_pad, resume, unreachable, LandingPad, LandingPadInst, Resume, ResumeInst, TerminatorInst, Unreachable,
    UnreachableInst,
};
pub use self::unary::*;
pub use self::vaarg::{va_arg, VaArg};
pub use self::vector::{extract_element, insert_element, shuffle_vector, ExtractElement, InsertElement, ShuffleVector};
