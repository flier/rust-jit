use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitBool, LitChar, LitFloat, LitInt, LitStr, Result};

pub enum Operand {
    Ident(Ident),
    Str(LitStr),
    Char(LitChar),
    Int(LitInt),
    Float(LitFloat),
    Bool(LitBool),
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
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![%]) && (input.peek2(Ident) || input.peek2(LitInt)) {
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
        } else if lookahead.peek(LitStr) {
            input.parse().map(Operand::Str)
        } else if lookahead.peek(LitChar) {
            input.parse().map(Operand::Char)
        } else if lookahead.peek(LitInt) {
            input.parse().map(Operand::Int)
        } else if lookahead.peek(LitFloat) {
            input.parse().map(Operand::Float)
        } else if lookahead.peek(LitBool) {
            input.parse().map(Operand::Bool)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Operand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Operand::Ident(ident) => ident.to_tokens(tokens),
            Operand::Str(s) => s.value().to_tokens(tokens),
            Operand::Char(c) => c.value().to_tokens(tokens),
            Operand::Int(n) => n.value().to_tokens(tokens),
            Operand::Float(f) => f.value().to_tokens(tokens),
            Operand::Bool(b) => b.value.to_tokens(tokens),
        }
    }
}
