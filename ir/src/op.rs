use std::fmt;

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitInt, Result};

use crate::value::Value;

#[derive(Clone, Debug)]
pub enum Operand {
    Local(Ident),
    Global(Ident),
    Value(Value),
}

impl Operand {
    pub fn ident(&self) -> Option<&Ident> {
        match self {
            Operand::Local(ident) | Operand::Global(ident) => Some(ident),
            _ => None,
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Local(ident) => write!(f, "%{}", ident),
            Operand::Global(ident) => write!(f, "@{}", ident),
            Operand::Value(value) => value.fmt(f),
        }
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![%]) && (input.peek2(Ident) || input.peek2(LitInt)) {
            let _rem = input.parse::<Token![%]>()?;

            if input.peek(Ident) {
                input.parse::<Ident>().map(Operand::Local)
            } else if input.peek(LitInt) {
                input
                    .parse::<LitInt>()
                    .map(|int| Ident::new(&format!("v{}", int.value()), Span::call_site()))
                    .map(Operand::Local)
            } else {
                unreachable!()
            }
        } else if input.peek(Token![@]) && (input.peek2(Ident) || input.peek2(LitInt)) {
            let _rem = input.parse::<Token![@]>()?;

            if input.peek(Ident) {
                input.parse::<Ident>().map(Operand::Global)
            } else if input.peek(LitInt) {
                input
                    .parse::<LitInt>()
                    .map(|int| Ident::new(&format!("v{}", int.value()), Span::call_site()))
                    .map(Operand::Global)
            } else {
                unreachable!()
            }
        } else {
            input.parse().map(Operand::Value)
        }
    }
}

impl ToTokens for Operand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Operand::Local(ident) | Operand::Global(ident) => ident.to_tokens(tokens),
            Operand::Value(value) => value.to_tokens(tokens),
        }
    }
}
