use std::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::Result;

use crate::constant::Constant;
use crate::expr::Expr;
use crate::stmt::Stmt;

#[derive(Debug, Parse)]
pub enum Ir {
    Stmt(Stmt),
    Expr(Expr),
    Constant(Constant),
}

impl fmt::Display for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ir::Stmt(stmt) => stmt.fmt(f),
            Ir::Expr(expr) => expr.fmt(f),
            Ir::Constant(constant) => constant.fmt(f),
        }
    }
}

impl ToTokens for Ir {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Ir::Stmt(stmt) => stmt.to_tokens(tokens),
            Ir::Expr(expr) => expr.to_tokens(tokens),
            Ir::Constant(constant) => constant.to_tokens(tokens),
        }
    }
}
