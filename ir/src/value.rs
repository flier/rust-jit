use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket};
use syn::{Lit, LitBool, LitFloat, LitInt, LitStr, Result};

use crate::kw;
use crate::op::Operand;
use crate::ty::Type;

#[derive(Clone, Debug)]
pub enum Value {
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

impl Parse for Value {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(LitBool) {
            Ok(Value::Bool(input.parse::<LitBool>()?.value))
        } else if lookahead.peek(LitInt) {
            input.parse::<LitInt>().map(|n| n.value()).map(Value::Uint)
        } else if lookahead.peek(Token![-]) && input.peek2(LitInt) {
            let _ = input.parse::<Token![-]>()?;

            input.parse::<LitInt>().map(|n| -(n.value() as i64)).map(Value::Int)
        } else if lookahead.peek(LitFloat) {
            input.parse::<LitFloat>().map(|f| f.value()).map(Value::Float)
        } else if lookahead.peek(Token![-]) && input.peek2(LitFloat) {
            let _ = input.parse::<Token![-]>()?;

            input.parse::<LitFloat>().map(|f| -f.value()).map(Value::Float)
        } else if lookahead.peek(kw::c) && input.peek2(LitStr) {
            let _ = input.parse::<kw::c>()?;

            input.parse::<LitStr>().map(|s| s.value()).map(Value::Str)
        } else if lookahead.peek(kw::null) {
            input.parse::<kw::null>().map(|_| Value::Null)
        } else if lookahead.peek(kw::none) {
            input.parse::<kw::none>().map(|_| Value::None)
        } else if lookahead.peek(kw::zeroinitializer) {
            input.parse::<kw::zeroinitializer>().map(|_| Value::Zeroed)
        } else if lookahead.peek(Brace) {
            let content;
            let _brace = braced!(content in input);

            content
                .parse_terminated(Field::parse)
                .map(|fields| Value::Struct(fields, false))
        } else if lookahead.peek(Token![<]) && input.peek2(Brace) {
            let _ = input.parse::<Token![<]>()?;
            let content;
            let _brace = braced!(content in input);
            let s = content
                .parse_terminated(Field::parse)
                .map(|fields| Value::Struct(fields, true))?;
            let _ = input.parse::<Token![>]>()?;

            Ok(s)
        } else if lookahead.peek(Bracket) {
            let content;
            let _bracket = bracketed!(content in input);

            content.parse_terminated(Field::parse).map(Value::Array)
        } else if lookahead.peek(Token![<]) {
            let _ = input.parse::<Token![<]>()?;
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

            let _ = input.parse::<Token![>]>()?;

            Ok(Value::Vector(elements))
        } else if lookahead.peek(Lit) {
            // TODO: represents an integer larger than 64 bits.
            unimplemented!()
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Value::Bool(b) => b.to_tokens(tokens),
            Value::Int(n) => n.to_tokens(tokens),
            Value::Uint(n) => n.to_tokens(tokens),
            Value::Float(f) => f.to_tokens(tokens),
            Value::Str(s) => s.to_tokens(tokens),
            Value::Null => quote! { ::std::ptr::null_mut() }.to_tokens(tokens),
            Value::None => quote! { None }.to_tokens(tokens),
            Value::Zeroed => quote! { unsafe { ::std::mem::zeroed() } }.to_tokens(tokens),
            Value::Struct(fields, packed) => {
                let fields = fields.iter().map(|field| &field.op);

                let expand = if *packed {
                    quote! { struct_of!( #( #fields ),* ; packed) }
                } else {
                    quote! { struct_of!( #( #fields ),* ) }
                };

                trace!("expand {:?} to `{}`", self, expand);

                expand.to_tokens(tokens)
            }
            Value::Array(elements) => {
                let ty = elements.first().map(|p| &p.value().ty);
                let elements = elements.iter().map(|field| &field.op);

                let expand = quote! { array_of!{ #ty [#( #elements ),*] } };

                trace!("expand {:?} to {}", self, expand);

                expand.to_tokens(tokens)
            }
            Value::Vector(elements) => {
                let elements = elements.iter().map(|field| &field.op);

                let expand = quote! { ( #( #elements ),* ) };

                trace!("expand {:?} to {}", self, expand);

                expand.to_tokens(tokens)
            }
        }
    }
}
