use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::insts::Ret;
use crate::kw;

pub enum IrCode {
    Ret(Ret),
}

impl Parse for IrCode {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(kw::ret) {
            input.parse().map(IrCode::Ret)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for IrCode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            IrCode::Ret(ret) => ret.to_tokens(tokens),
        }
    }
}
