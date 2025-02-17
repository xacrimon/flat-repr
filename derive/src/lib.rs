mod repr_flat;
mod repr_ref;
mod trait_impl;

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Attribute, DeriveInput, Expr, Ident, Token,
};

#[proc_macro_derive(BetterRepr, attributes(better_repr))]
pub fn better_repr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let repr_flat = repr_flat::gen(&input).unwrap();
    let repr_ref = repr_ref::gen(&input).unwrap();

    let expanded = quote! {
        #repr_flat
        #repr_ref
    };

    proc_macro::TokenStream::from(expanded)
}

fn named_fields<'a>(
    data: &'a syn::Data,
) -> syn::Result<
    Vec<(
        Span,
        &'a syn::Ident,
        &'a syn::Visibility,
        &'a syn::Type,
        Behaviour,
    )>,
> {
    let parse_field = |field: &'a syn::Field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        let behaviour = parse_behaviour(field)?;
        Ok((field.span(), name, &field.vis, ty, behaviour))
    };

    match data {
        syn::Data::Struct(data) => data.fields.iter().map(parse_field).collect(),
        _ => unimplemented!(),
    }
}

fn parse_behaviour(field: &syn::Field) -> syn::Result<Behaviour> {
    let maybe_attr: Option<&Attribute> = field
        .attrs
        .iter()
        .filter(|attr| attr.meta.path().is_ident("better_repr"))
        .next();

    match maybe_attr {
        Some(attr) => attr.parse_args(),
        None => Ok(Behaviour::Copy),
    }
}

enum Behaviour {
    Copy,
    InlineString,
    InlineList(syn::Type),
    //OutlinedOption(syn::Type),
    // InlineSequence, TODO: a list but with dst elements
}

impl Parse for Behaviour {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let behaviour = input.parse::<Ident>()?;

        if behaviour == "copy" {
            Ok(Behaviour::Copy)
        } else if behaviour == "inline_string" {
            Ok(Behaviour::InlineString)
        } else if behaviour == "inline_list" {
            input.parse::<Token![,]>()?;
            input.parse::<Token![<]>()?;
            let ty = input.parse::<syn::Type>()?;
            input.parse::<Token![>]>()?;
            Ok(Behaviour::InlineList(ty))
        } else {
            Err(syn::Error::new(behaviour.span(), "unknown behaviour"))
        }
    }
}
