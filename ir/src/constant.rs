use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket};
use syn::{Error, LitBool, LitFloat, LitInt, LitStr, Result};

use crate::kw;
use crate::op::Operand;
use crate::ty::Type;

#[derive(Clone, Debug)]
pub enum Constant {
    Bool(bool),
    Int(i64),
    Uint(u64),
    Float(f64),
    Str(String),
    Null,
    None,
    Zeroed,
    Struct(Punctuated<Field, Token![,]>, bool),
    Array(Punctuated<Field, Token![,]>),
    Vector(Punctuated<Field, Token![,]>),
}

#[derive(Clone, Debug)]
pub struct Field {
    ty: Type,
    op: Operand,
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Field {
            ty: input.parse()?,
            op: input.parse()?,
        })
    }
}

impl Parse for Constant {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(LitBool) {
            Ok(Constant::Bool(input.parse::<LitBool>()?.value))
        } else if lookahead.peek(LitInt) {
            input.parse::<LitInt>().map(|n| n.value()).map(Constant::Uint)
        } else if lookahead.peek(LitFloat) {
            input.parse::<LitFloat>().map(|f| f.value()).map(Constant::Float)
        } else if lookahead.peek(Token![-]) && (input.peek2(LitInt) || input.peek2(LitFloat)) {
            let _ = input.parse::<Token![-]>()?;

            if input.peek(LitInt) {
                input.parse::<LitInt>().map(|n| -(n.value() as i64)).map(Constant::Int)
            } else if input.peek(LitFloat) {
                input.parse::<LitFloat>().map(|f| -f.value()).map(Constant::Float)
            } else {
                unreachable!()
            }
        } else if lookahead.peek(kw::c) && input.peek2(LitStr) {
            let _ = input.parse::<kw::c>()?;

            input.parse::<LitStr>().map(|s| s.value()).map(Constant::Str)
        } else if lookahead.peek(kw::null) {
            input.parse::<kw::null>().map(|_| Constant::Null)
        } else if lookahead.peek(kw::none) {
            input.parse::<kw::none>().map(|_| Constant::None)
        } else if lookahead.peek(kw::zeroinitializer) {
            input.parse::<kw::zeroinitializer>().map(|_| Constant::Zeroed)
        } else if lookahead.peek(Brace) {
            let content;
            let _brace = braced!(content in input);

            content
                .parse_terminated(Field::parse)
                .map(|fields| Constant::Struct(fields, false))
        } else if lookahead.peek(Bracket) {
            let content;
            let _bracket = bracketed!(content in input);

            content.parse_terminated(Field::parse).map(Constant::Array)
        } else if lookahead.peek(Token![<]) {
            let _ = input.parse::<Token![<]>()?;

            let constant = if input.peek(Brace) {
                let content;
                let _brace = braced!(content in input);
                content
                    .parse_terminated(Field::parse)
                    .map(|fields| Constant::Struct(fields, true))?
            } else {
                let mut elements = Punctuated::new();

                loop {
                    if input.peek(Token![>]) {
                        break;
                    }

                    elements.push_value(input.parse()?);

                    if input.peek(Token![>]) {
                        break;
                    }

                    elements.push_punct(input.parse()?);
                }

                Constant::Vector(elements)
            };

            let _ = input.parse::<Token![>]>()?;

            Ok(constant)
        } else {
            let err = lookahead.error();
            let tt = input.fork().cursor().token_tree().map(|(tt, _)| tt).unwrap();

            Err(Error::new(
                err.span(),
                format!("invalid constant, {}, but got `{:?}`", err, tt),
            ))
        }
    }
}

#[cfg(feature = "display")]
mod display {
    use std::fmt;

    use syn::token::CustomKeyword;

    use super::*;

    impl fmt::Display for Constant {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Constant::Bool(v) => v.fmt(f),
                Constant::Int(v) => v.fmt(f),
                Constant::Uint(v) => v.fmt(f),
                Constant::Float(v) => v.fmt(f),
                Constant::Str(v) => v.fmt(f),
                Constant::Null => kw::null::ident().fmt(f),
                Constant::None => kw::none::ident().fmt(f),
                Constant::Zeroed => kw::zeroinitializer::ident().fmt(f),
                Constant::Struct(fields, packed) => {
                    let fields = itertools::join(fields.iter().map(|field| format!("{} {}", field.ty, field.op)), ",");

                    if *packed {
                        write!(f, "<{}>", fields)
                    } else {
                        write!(f, "{}", fields)
                    }
                }
                Constant::Array(elements) => write!(
                    f,
                    "[{}]",
                    itertools::join(elements.iter().map(|field| format!("{} {}", field.ty, field.op)), ",")
                ),
                Constant::Vector(elements) => write!(
                    f,
                    "<{}>",
                    itertools::join(elements.iter().map(|field| format!("{} {}", field.ty, field.op)), ",")
                ),
            }
        }
    }
}

#[cfg(feature = "gen")]
mod gen {
    use proc_macro2::TokenStream;
    use quote::ToTokens;

    use super::*;

    impl ToTokens for Constant {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Constant::Bool(b) => b.to_tokens(tokens),
                Constant::Int(n) => n.to_tokens(tokens),
                Constant::Uint(n) => n.to_tokens(tokens),
                Constant::Float(f) => f.to_tokens(tokens),
                Constant::Str(s) => s.to_tokens(tokens),
                Constant::Null => quote! { ::std::ptr::null_mut() }.to_tokens(tokens),
                Constant::None => quote! { None }.to_tokens(tokens),
                Constant::Zeroed => quote! { unsafe { ::std::mem::zeroed() } }.to_tokens(tokens),
                Constant::Struct(fields, packed) => {
                    let fields = fields.iter().map(|field| &field.op);

                    let expand = if *packed {
                        quote! { struct_of!( #( #fields ),* ; packed) }
                    } else {
                        quote! { struct_of!( #( #fields ),* ) }
                    };

                    trace!("expand {:?} to `{}`", self, expand);

                    expand.to_tokens(tokens)
                }
                Constant::Array(elements) => {
                    let ty = elements.first().map(|p| &p.value().ty);
                    let elements = elements.iter().map(|field| &field.op);

                    let expand = quote! { array_of!{ #ty [#( #elements ),*] } };

                    trace!("expand {:?} to {}", self, expand);

                    expand.to_tokens(tokens)
                }
                Constant::Vector(elements) => {
                    let elements = elements.iter().map(|field| &field.op);

                    let expand = quote! { ( #( #elements ),* ) };

                    trace!("expand {:?} to {}", self, expand);

                    expand.to_tokens(tokens)
                }
            }
        }
    }
}
