extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

use llvm_ir as ir;

#[proc_macro]
pub fn ir(input: TokenStream) -> TokenStream {
    let ir = parse_macro_input!(input as ir::IrCode);

    let expanded = quote! {
        #ir
    };

    TokenStream::from(expanded)
}
