use syn::parse::{Parse, ParseStream};
use syn::{Error, Ident, LitInt, Result};

use crate::constant::Constant;

#[derive(Clone, Debug)]
pub enum Value {
    Named(Ident),
    Unnamed(u64),
}

#[derive(Clone, Debug)]
pub enum Operand {
    Local(Value),
    Global(Value),
    Constant(Constant),
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![%]) || input.peek(Token![@]) && (input.peek2(Ident) || input.peek2(LitInt)) {
            let is_local = input
                .parse::<Token![%]>()
                .map(|_| true)
                .or_else(|_| input.parse::<Token![@]>().map(|_| false))?;

            let value = if input.peek(Ident) {
                input.parse().map(Value::Named)
            } else if input.peek(LitInt) {
                input.parse::<LitInt>().map(|lit| lit.value()).map(Value::Unnamed)
            } else {
                unreachable!()
            };

            value.map(if is_local { Operand::Local } else { Operand::Global })
        } else {
            input.parse().map(Operand::Constant)
        }
        .map_err(|err| Error::new(err.span(), format!("invalid operand, {}", err)))
    }
}

#[cfg(feature = "display")]
mod display {
    use std::fmt;

    use super::*;

    impl fmt::Display for Value {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Value::Named(id) => id.fmt(f),
                Value::Unnamed(idx) => idx.fmt(f),
            }
        }
    }

    impl fmt::Display for Operand {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Operand::Local(ident) => write!(f, "%{}", ident),
                Operand::Global(ident) => write!(f, "@{}", ident),
                Operand::Constant(value) => value.fmt(f),
            }
        }
    }

}

#[cfg(feature = "gen")]
mod gen {
    use proc_macro2::{Span, TokenStream};
    use quote::ToTokens;

    use super::*;

    const UNNAMED_PREFIX: &str = "__unnamed";

    impl ToTokens for Value {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Value::Named(id) => id.clone(),
                Value::Unnamed(idx) => Ident::new(&format!("{}_{}", UNNAMED_PREFIX, idx), Span::call_site()),
            }
            .to_tokens(tokens)
        }
    }

    impl ToTokens for Operand {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Operand::Local(value) | Operand::Global(value) => value.to_tokens(tokens),
                Operand::Constant(constant) => constant.to_tokens(tokens),
            }
        }
    }
}
