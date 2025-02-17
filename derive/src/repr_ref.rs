use crate::{named_fields, Behaviour};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Attribute, DeriveInput, Expr, Ident, Token,
};

pub fn gen(input: &DeriveInput) -> syn::Result<TokenStream> {
    let ty_vis = &input.vis;
    let input_ty_name = &input.ident;
    let ty_name = format_ident!("BetterRepr_Ref_{}", input.ident);
    let li = parse_quote! { 'a };

    let fields = named_fields(&input.data)?
        .into_iter()
        .map(|(span, name, vis, ty, behaviour)| {
            let repr_ty = mapped_ty(ty, &behaviour, &li);
            quote_spanned! { span => #vis #name: #repr_ty }
        });

    let expanded = quote! {
        #[allow(non_camel_case_types)]
        #ty_vis struct #ty_name<#li> {
            #(#fields),*
        }

        impl better_repr::ReprRef for #input_ty_name {
            type Ty<#li> = #ty_name<#li>;
        }
    };

    Ok(expanded)
}

pub fn mapped_ty(ty: &syn::Type, behaviour: &Behaviour, li: &syn::Lifetime) -> syn::Type {
    match behaviour {
        Behaviour::Copy => parse_quote! { #ty },
        Behaviour::InlineString => parse_quote! { &#li str },
        Behaviour::InlineList(ty) => parse_quote! { &#li [#ty] },
    }
}
