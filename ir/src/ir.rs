use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::value::Value;

pub enum Ir {
    Stmt(Stmt),
    Expr(Expr),
    Value(Value),
}

impl Parse for Ir {
    fn parse(input: ParseStream) -> Result<Self> {
        input
            .parse()
            .map(Ir::Stmt)
            .or_else(|_| input.parse().map(Ir::Expr))
            .or_else(|_| input.parse().map(Ir::Value))
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
