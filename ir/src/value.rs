use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket};
use syn::{Lit, LitBool, LitFloat, LitInt, Result};

use crate::kw;
use crate::op::Operand;
use crate::ty::Type;

pub enum Value {
    Boolean(bool),
    Integer(u64),
    Float(f64),
    Null(kw::null),
    None(kw::none),
    Struct(Punctuated<Field, Token![,]>),
    Array(Punctuated<Field, Token![,]>),
    Vector(Punctuated<Field, Token![,]>),
    Zero(kw::zeroinitializer),
}

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

impl Parse for Value {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(LitBool) {
            Ok(Value::Boolean(input.parse::<LitBool>()?.value))
        } else if lookahead.peek(LitInt) {
            Ok(Value::Integer(input.parse::<LitInt>()?.value()))
        } else if lookahead.peek(LitFloat) {
            Ok(Value::Float(input.parse::<LitFloat>()?.value()))
        } else if lookahead.peek(Lit) {
            // TODO: represents an integer larger than 64 bits.
            unimplemented!()
        } else if lookahead.peek(kw::null) {
            input.parse().map(Value::Null)
        } else if lookahead.peek(kw::none) {
            input.parse().map(Value::None)
        } else if lookahead.peek(kw::zeroinitializer) {
            input.parse().map(Value::Zero)
        } else if lookahead.peek(Brace) {
            let content;
            let _brace = braced!(content in input);

            content.parse_terminated(Field::parse).map(Value::Struct)
        } else if lookahead.peek(Bracket) {
            let content;
            let bracket = bracketed!(content in input);

            content.parse_terminated(Field::parse).map(Value::Array)
        } else if lookahead.peek(Token![<]) {
            let _ = input.parse::<Token![<]>()?;
            let v = Punctuated::parse_terminated_with(input, Field::parse).map(Value::Vector);
            let _ = input.parse::<Token![>]>()?;

            v
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Value::Boolean(b) => b.to_tokens(tokens),
            Value::Integer(n) => n.to_tokens(tokens),
            Value::Float(f) => f.to_tokens(tokens),
            Value::Null(kw) => kw.to_tokens(tokens),
            Value::None(kw) => kw.to_tokens(tokens),
            Value::Struct { .. } => {}
            Value::Array { .. } => {}
            Value::Vector { .. } => {}
            Value::Zero(kw) => kw.to_tokens(tokens),
        }
    }
}
