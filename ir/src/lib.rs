#[macro_use]
extern crate log;
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate llvm_ir_parse;

pub mod expr;
mod ir;
mod kw;
mod op;
mod stmt;
mod ty;
mod value;

pub use self::expr::Expr;
pub use self::ir::Ir;
pub use self::op::Operand;
pub use self::stmt::*;
pub use self::ty::Type;
pub use self::value::Value;
