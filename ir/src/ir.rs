use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::expr::Expr;
use crate::kw;
use crate::op::Operand;
use crate::stmt::Ret;
use crate::value::Value;

pub enum Ir {
    Ret(Ret),
    Unreachable(kw::unreachable),
    Assign(Operand, Expr),
    Expr(Expr),
    Value(Value),
}

impl Parse for Ir {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::ret) {
            input.parse().map(Ir::Ret)
        } else if input.peek(kw::unreachable) {
            input.parse().map(Ir::Unreachable)
        } else if input.peek(Token![%]) {
            let result = input.parse::<Operand>()?;
            let _eq = input.parse::<Token![=]>()?;
            let op = input.parse::<Expr>()?;

            Ok(Ir::Assign(result, op))
        } else if let Ok(expr) = input.parse().map(Ir::Expr) {
            Ok(expr)
        } else {
            input.parse().map(Ir::Value)
        }
    }
}

impl ToTokens for Ir {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Ir::Ret(ret) => quote! { #ret },
            Ir::Unreachable(_) => quote! { ::llvm_jit::insts::Unreachable },
            Ir::Assign(operand, expr) => quote! { let #operand = #expr; },
            Ir::Expr(expr) => quote! { #expr },
            Ir::Value(value) => quote! { #value },
        }
        .to_tokens(tokens)
    }
}
