use crate::{Behaviour, Cx};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Attribute, DeriveInput, Expr, Ident, Lifetime, Token,
};

impl Cx {
    pub(super) fn gen_trait_impl(&self) -> syn::Result<TokenStream> {
        let derivee = &self.derivee;
        let flat_ty = &self.flat_ty;
        let ref_ty = &self.ref_ty;
        let to_flat = self.gen_to_flat()?;

        let expanded = quote! {
            impl flat_repr::Plain for #derivee {
                type FlatRepr = #flat_ty;
                type RefRepr<'a> = #ref_ty<'a>;

                #to_flat
            }
        };

        Ok(expanded)
    }
}
