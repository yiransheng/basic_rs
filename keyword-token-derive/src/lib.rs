#![recursion_limit = "128"]

#[macro_use]
extern crate quote;
#[macro_use]
extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use syn::{parse2, DeriveInput};

use crate::states::impl_keyword_token;

mod states;

#[proc_macro_derive(KeywordToken)]
pub fn derive_keyword_token(input: TokenStream) -> TokenStream {
    let item: DeriveInput = parse2(input.into()).unwrap();
    let code = impl_keyword_token(&item);

    let r = quote::quote!(#code).into();

    // println!("{}", r);

    r
}
