use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::insts::*;
use crate::kw;

pub enum IrCode {
    Ret(Ret),
    Unreachable(kw::unreachable),
    Assign(Operand, Operation),
}

impl Parse for IrCode {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(kw::ret) {
            input.parse().map(IrCode::Ret)
        } else if lookahead.peek(kw::unreachable) {
            input.parse().map(IrCode::Unreachable)
        } else {
            let result = input.parse::<Operand>()?;
            let _eq = input.parse::<Token![=]>()?;
            let op = input.parse::<Operation>()?;

            Ok(IrCode::Assign(result, op))
        }
    }
}

impl ToTokens for IrCode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            IrCode::Ret(ret) => quote! { #ret },
            IrCode::Unreachable(_) => quote! { ::llvm_jit::insts::Unreachable },
            IrCode::Assign(operand, operation) => quote! { let #operand = #operation; },
        }
        .to_tokens(tokens)
    }
}
