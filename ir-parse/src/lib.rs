#![recursion_limit = "128"]

extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use syn::{Data, DataEnum, DataStruct, DeriveInput, Fields, Variant};

#[proc_macro_derive(Parse, attributes(ignore))]
pub fn parse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let expanded = match input.data {
        Data::Struct(DataStruct { fields, .. }) => parse_struct(name, fields),
        Data::Enum(DataEnum { variants, .. }) => parse_enum(name, variants.iter()),
        Data::Union(_) => panic!("not support union"),
    };

    expanded.into()
}

fn parse_struct<'a>(name: proc_macro2::Ident, fields: Fields) -> proc_macro2::TokenStream {
    match fields {
        Fields::Named(fields) => {
            let fields = fields.named.iter().map(|field| {
                let field_name = field.ident.as_ref().unwrap();

                if field.attrs.iter().any(|attr| attr.path.is_ident("ignore")) {
                    quote! {
                        #field_name: Default::default(),
                    }
                } else {
                    quote! {
                        #field_name: input.parse()?
                    }
                }
            });

            quote! {
                impl ::syn::parse::Parse for #name {
                    fn parse(input: ::syn::parse::ParseStream) -> ::syn::Result<Self> {
                        Ok(#name { #( #fields ),* })
                    }
                }
            }
        }
        Fields::Unnamed(fields) => {
            let fields = fields.unnamed.iter().map(|field| {
                if field.attrs.iter().any(|attr| attr.path.is_ident("ignore")) {
                    quote! { input.parse()? }
                } else {
                    quote! { Default::default() }
                }
            });

            quote! {
                impl ::syn::parse::Parse for #name {
                    fn parse(input: ::syn::parse::ParseStream) -> ::syn::Result<Self> {
                        Ok(#name ( #( #fields ),* ))
                    }
                }
            }
        }
        Fields::Unit => quote! {
            quote! {
                impl ::syn::parse::Parse for #name {
                    fn parse(input: ::syn::parse::ParseStream) -> ::syn::Result<Self> {
                        Ok(#name)
                    }
                }
            }
        },
    }
}

fn parse_enum<'a>(name: proc_macro2::Ident, variants: impl Iterator<Item = &'a Variant>) -> proc_macro2::TokenStream {
    let mut steps = variants.map(|var| {
        let varname = &var.ident;

        match &var.fields {
            Fields::Named(_fields) => panic!("not support named variant fields"),
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    quote! {
                        match input.fork().parse().map(#name :: #varname) {
                            Ok(_) => input.parse().map(#name :: #varname),
                            Err(err) => Err(::syn::Error::new(err.span(),
                                format!("invalid {}::{}, {}", stringify!(#name), stringify!(#varname), err)))
                        }
                    }
                } else {
                    panic!("not support multi variant fields")
                }
            }
            Fields::Unit => panic!("not support variant unit"),
        }
    });

    let first_step = steps.next().unwrap();

    quote! {
        impl ::syn::parse::Parse for #name {
            fn parse(input: ::syn::parse::ParseStream) -> ::syn::Result<Self> {
                #first_step #( .or_else(|_| #steps ) )* .map_err(|err|
                    ::syn::Error::new(err.span(), format!("invalid {}, {}", stringify!(#name), err))
                )
            }
        }
    }
}
