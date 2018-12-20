use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Lit, Result};

pub struct Value(Lit);

impl Parse for Value {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Value)
    }
}

impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}
