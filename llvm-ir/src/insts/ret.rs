use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::kw;
use crate::ty::Type;
use crate::value::Value;

pub struct Ret(Option<(Type, Value)>);

impl Parse for Ret {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::ret>()?;

        if input.peek(kw::void) {
            input.parse::<kw::void>()?;

            Ok(Ret(None))
        } else {
            let ty = input.parse::<Type>()?;
            let val = input.parse::<Value>()?;

            Ok(Ret(Some((ty, val))))
        }
    }
}

impl ToTokens for Ret {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expanded = if let Some((_ty, val)) = self.0.as_ref() {
            quote! {
                ret!(#val)
            }
        } else {
            quote! {
                ret!()
            }
        };

        tokens.append_all(expanded)
    }
}
