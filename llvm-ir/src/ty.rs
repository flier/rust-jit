use proc_macro2::Ident;
use regex::Regex;
use syn::parse::{Parse, ParseStream};
use syn::Result;

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
    Named(Ident),
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::void) {
            Ok(Type::Void)
        } else if input.peek(kw::half) {
            Ok(Type::Half)
        } else if input.peek(kw::float) {
            Ok(Type::Float)
        } else if input.peek(kw::double) {
            Ok(Type::Double)
        } else if input.peek(kw::fp128) {
            Ok(Type::Fp128)
        } else if input.peek(kw::x86_fp80) {
            Ok(Type::Fp80)
        } else if input.peek(kw::x86_mmx) {
            Ok(Type::Mmx)
        } else {
            let ident = input.parse::<Ident>()?;
            let typename = ident.to_string();

            if let Some(captures) = RE_INTEGER.captures(&typename) {
                captures
                    .get(1)
                    .map(|s| s.as_str().parse::<usize>().unwrap())
                    .map(|bits| Type::Integer(bits))
                    .ok_or(input.error("invalid integer"))
            } else {
                Ok(Type::Named(ident))
            }
        }
    }
}
