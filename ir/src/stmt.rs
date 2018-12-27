use std::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::token::CustomKeyword;
use syn::Result;

use crate::expr::Expr;
use crate::kw;
use crate::op::Operand;
use crate::ty::Type;
use crate::value::Value;

pub enum Stmt {
    Ret(Ret),
    Unreachable(kw::unreachable),
    Assign(Operand, Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Ret(ret) => ret.fmt(f),
            Stmt::Unreachable(_) => kw::unreachable::ident().fmt(f),
            Stmt::Assign(op, expr) => write!(f, "{} = {}", op, expr),
        }
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(kw::ret) {
            input.parse().map(Stmt::Ret)
        } else if lookahead.peek(kw::unreachable) {
            input.parse().map(Stmt::Unreachable)
        } else if lookahead.peek(Token![%]) {
            let result = input.parse::<Operand>()?;
            let _eq = input.parse::<Token![=]>()?;
            let op = input.parse::<Expr>()?;

            Ok(Stmt::Assign(result, op))
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Stmt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Stmt::Ret(ret) => quote! { #ret },
            Stmt::Unreachable(_) => quote! { ::llvm_jit::insts::Unreachable },
            Stmt::Assign(operand, expr) => quote! { let #operand = #expr; },
        }
        .to_tokens(tokens)
    }
}

pub struct Ret(Option<(Type, Value)>);

impl fmt::Display for Ret {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {}",
            kw::ret::ident(),
            self.0
                .as_ref()
                .map(|(ty, value)| format!("{} {}", ty, value))
                .unwrap_or(kw::void::ident().to_string())
        )
    }
}

impl Parse for Ret {
    fn parse(input: ParseStream) -> Result<Self> {
        let _ret = input.parse::<kw::ret>()?;

        if input.peek(kw::void) {
            let _void = input.parse::<kw::void>()?;

            Ok(Ret(None))
        } else {
            let ty = input.parse::<Type>()?;
            let val = input.parse::<Value>()?;

            Ok(Ret(Some((ty, val))))
        }
    }
}

impl ToTokens for Ret {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some((_ty, val)) = self.0.as_ref() {
            quote! {
                ret!(#val)
            }
        } else {
            quote! {
                ret!()
            }
        }
        .to_tokens(tokens)
    }
}
