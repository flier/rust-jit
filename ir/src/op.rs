use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitInt, Result};

use crate::value::Value;

pub enum Operand {
    Ident(Ident),
    Value(Value),
}

impl Operand {
    pub fn ident(&self) -> Option<&Ident> {
        match self {
            Operand::Ident(ident) => Some(ident),
            _ => None,
        }
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![%]) && (input.peek2(Ident) || input.peek2(LitInt)) {
            let _rem = input.parse::<Token![%]>()?;

            if input.peek(Ident) {
                input.parse::<Ident>().map(Operand::Ident)
            } else if input.peek(LitInt) {
                input
                    .parse::<LitInt>()
                    .map(|int| Ident::new(&format!("v{}", int.value()), Span::call_site()))
                    .map(Operand::Ident)
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
            Operand::Ident(ident) => ident.to_tokens(tokens),
            Operand::Value(value) => value.to_tokens(tokens),
        }
    }
}
