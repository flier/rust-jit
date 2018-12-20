extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate lazy_static;

use proc_macro::TokenStream;

mod insts;
mod ir;
mod kw;
mod ty;
mod value;

#[proc_macro]
pub fn ir(input: TokenStream) -> TokenStream {
    let ir = parse_macro_input!(input as ir::IrCode);

    let expanded = quote! {
        #ir
    };

    TokenStream::from(expanded)
}
