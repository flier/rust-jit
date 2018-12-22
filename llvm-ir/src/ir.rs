use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Result;

use crate::expr::Expr;
use crate::kw;
use crate::op::Operand;
use crate::stmt::Ret;

pub enum IrCode {
    Ret(Ret),
    Unreachable(kw::unreachable),
    Expr(Operand, Expr),
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
            let op = input.parse::<Expr>()?;

            Ok(IrCode::Expr(result, op))
        }
    }
}

impl ToTokens for IrCode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            IrCode::Ret(ret) => quote! { #ret },
            IrCode::Unreachable(_) => quote! { ::llvm_jit::insts::Unreachable },
            IrCode::Expr(operand, expr) => quote! { let #operand = #expr; },
        }
        .to_tokens(tokens)
    }
}
