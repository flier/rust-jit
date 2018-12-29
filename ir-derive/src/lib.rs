extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

use llvm_ir as ir;

#[proc_macro]
pub fn ir(input: TokenStream) -> TokenStream {
    let ir = parse_macro_input!(input as ir::Ir);

    let expanded = quote! {
        #ir
    };

    TokenStream::from(expanded)
}

#[proc_macro]
pub fn expr(input: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(input as ir::Expr);

    let expanded = quote! {
        #expr
    };

    TokenStream::from(expanded)
}

#[proc_macro]
pub fn stmt(input: TokenStream) -> TokenStream {
    let stmt = parse_macro_input!(input as ir::Stmt);

    let expanded = quote! {
        #stmt
    };

    TokenStream::from(expanded)
}

#[proc_macro]
pub fn constant(input: TokenStream) -> TokenStream {
    let constant = parse_macro_input!(input as ir::Constant);

    let expanded = quote! {
        #constant
    };

    TokenStream::from(expanded)
}
