use std::rc::Rc;

use proc_macro2::Ident;
use regex::Regex;
use syn::parse::{Parse, ParseStream};
use syn::{LitInt, Result};

use crate::kw;

lazy_static! {
    static ref RE_INTEGER: Regex = Regex::new("i(\\d+)").unwrap();
}

pub enum Type {
    Void(kw::void),
    Integer(usize),
    Half(kw::half),
    Float(kw::float),
    Double(kw::double),
    Fp128(kw::fp128),
    Fp80(kw::x86_fp80),
    Mmx(kw::x86_mmx),
    Vector(Rc<Type>, usize),
    Pointer(Rc<Type>),
    Named(Ident),
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        let ty = if lookahead.peek(kw::void) {
            input.parse().map(Type::Void)?
        } else if lookahead.peek(kw::half) {
            input.parse().map(Type::Half)?
        } else if lookahead.peek(kw::float) {
            input.parse().map(Type::Float)?
        } else if lookahead.peek(kw::double) {
            input.parse().map(Type::Double)?
        } else if lookahead.peek(kw::fp128) {
            input.parse().map(Type::Fp128)?
        } else if lookahead.peek(kw::x86_fp80) {
            input.parse().map(Type::Fp80)?
        } else if lookahead.peek(kw::x86_mmx) {
            input.parse().map(Type::Mmx)?
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
