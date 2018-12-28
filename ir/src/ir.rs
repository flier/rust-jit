use std::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::Result;

use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::value::Value;

#[derive(Debug, Parse)]
pub enum Ir {
    Stmt(Stmt),
    Expr(Expr),
    Value(Value),
}

impl fmt::Display for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ir::Stmt(stmt) => stmt.fmt(f),
            Ir::Expr(expr) => expr.fmt(f),
            Ir::Value(value) => value.fmt(f),
        }
    }
}

impl ToTokens for Ir {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Ir::Stmt(stmt) => stmt.to_tokens(tokens),
            Ir::Expr(expr) => expr.to_tokens(tokens),
            Ir::Value(value) => value.to_tokens(tokens),
        }
    }
}
