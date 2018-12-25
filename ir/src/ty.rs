use std::rc::Rc;

use proc_macro2::Ident;
use regex::Regex;
use syn::parse::{Parse, ParseStream};
use syn::token::CustomKeyword;
use syn::{LitInt, Result};

use crate::kw;

lazy_static! {
    static ref RE_INTEGER: Regex = Regex::new("i(\\d+)").unwrap();
}

pub enum Type {
    Void,
    Integer(usize),
    Half,
    Float,
    Double,
    Fp128,
    Fp80,
    Mmx,
    Vector(Rc<Type>, usize),
    Pointer(Rc<Type>),
    Named(Ident),
}

impl From<Ident> for Type {
    fn from(ident: Ident) -> Self {
        let s = ident.to_string();

        if s == kw::void::ident() {
            Type::Void
        } else if s == kw::half::ident() {
            Type::Half
        } else if s == kw::double::ident() {
            Type::Double
        } else if s == kw::fp128::ident() {
            Type::Fp128
        } else if s == kw::x86_fp80::ident() {
            Type::Fp80
        } else if s == kw::x86_mmx::ident() {
            Type::Mmx
        } else if let Some(captures) = RE_INTEGER.captures(&s) {
            captures
                .get(1)
                .map(|s| s.as_str().parse::<usize>().unwrap())
                .map(Type::Integer)
                .unwrap()
        } else {
            Type::Named(ident)
        }
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        let ty = if lookahead.peek(kw::void) {
            let _ = input.parse::<kw::void>()?;

            Type::Void
        } else if lookahead.peek(kw::half) {
            let _ = input.parse::<kw::half>()?;

            Type::Half
        } else if lookahead.peek(kw::float) {
            let _ = input.parse::<kw::float>()?;

            Type::Float
        } else if lookahead.peek(kw::double) {
            let _ = input.parse::<kw::double>()?;

            Type::Double
        } else if lookahead.peek(kw::fp128) {
            let _ = input.parse::<kw::fp128>()?;

            Type::Fp128
        } else if lookahead.peek(kw::x86_fp80) {
            let _ = input.parse::<kw::x86_fp80>()?;

            Type::Fp80
        } else if lookahead.peek(kw::x86_mmx) {
            let _ = input.parse::<kw::x86_mmx>()?;

            Type::Mmx
        } else if lookahead.peek(Token![<]) {
            let n: LitInt = input.parse()?;
            let _: kw::x = input.parse()?;
            let ty: Type = input.parse()?;
            let _: Token![>] = input.parse()?;

            Type::Vector(Rc::new(ty), n.value() as usize)
        } else {
            let ident: Ident = input.parse()?;
            let typename = ident.to_string();

            if let Some(captures) = RE_INTEGER.captures(&typename) {
                captures
                    .get(1)
                    .map(|s| s.as_str().parse::<usize>().unwrap())
                    .map(Type::Integer)
                    .unwrap()
            } else {
                Type::Named(ident)
            }
        };

        if input.peek(Token![*]) {
            let _ = input.parse::<Token![*]>()?;

            Ok(Type::Pointer(Rc::new(ty)))
        } else {
            Ok(ty)
        }
    }
}
