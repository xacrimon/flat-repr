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
    pub(super) fn gen_ref_ty(&self) -> syn::Result<TokenStream> {
        let vis = &self.vis;
        let ref_ty = &self.ref_ty;
        let li = parse_quote! { 'a };

        let fields = self.fields.iter().map(|field| {
            let name = &field.name;
            let ref_ty = self.field_ref_ty(&field.ty, &field.behaviour, &li);
            quote! { #vis #name: #ref_ty }
        });

        let expanded = quote! {
            #[allow(non_camel_case_types)]
            #vis struct #ref_ty<#li> {
                #(#fields),*
            }
        };

        Ok(expanded)
    }

    pub(super) fn field_ref_ty(
        &self,
        ty: &syn::Type,
        behaviour: &Behaviour,
        li: &syn::Lifetime,
    ) -> syn::Type {
        match behaviour {
            Behaviour::Copy => parse_quote! { #ty },
            Behaviour::InlineString => parse_quote! { &#li str },
            Behaviour::InlineList(ty) => parse_quote! { &#li [#ty] },
        }
    }
}
