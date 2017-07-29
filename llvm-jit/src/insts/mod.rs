mod builder;
#[macro_use]
mod ret;
#[macro_use]
mod br;
#[macro_use]
mod switch;
#[macro_use]
mod invoke;
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

pub use self::aggregate::{ExtractValue, InsertValue, extract_value, insert_value};
pub use self::atomic::{AtomicCmpXchg, AtomicRMW, Fence};
pub use self::binop::*;
pub use self::br::{Br, CondBr, IndirectBr};
pub use self::builder::{IRBuilder, InstructionBuilder, Position};
pub use self::call::{Call, CallConv, CallInst, CallSite};
pub use self::cast::*;
pub use self::cmp::{FCmp, FCmpInst, ICmp, ICmpInst};
pub use self::gep::GetElementPtr;
pub use self::globalstr::{GlobalString, GlobalStringPtr};
pub use self::invoke::{Invoke, InvokeInst};
pub use self::memory::{Alloca, AllocaInst, Free, Load, Malloc, Store};
pub use self::phi::{Phi, PhiNode};
pub use self::ret::{AggregateRet, Ret, RetVoid, TerminatorInst};
pub use self::select::Select;
pub use self::switch::{Switch, SwitchInst};
pub use self::terminator::{LandingPad, Resume, Unreachable};
pub use self::unary::{FNeg, IsNotNull, IsNull, NSWNeg, NUWNeg, Neg, Not};
pub use self::vaarg::VaArg;
pub use self::vector::{ExtractElement, InsertElement, ShuffleVector};
