// TODO:
// - outlineoption
// - clone behaviour
// - assert type invariants like Field: Copy
// - customize offset type
// - custommize length type
// - inlinesequence

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
    Attribute, Expr, Token,
};

#[proc_macro_derive(BetterRepr, attributes(better_repr))]
pub fn better_repr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let cx = Cx::with_input(input).unwrap();

    let trait_impl = cx
        .gen_trait_impl()
        .unwrap_or_else(syn::Error::into_compile_error);
    let flat_ty = cx
        .gen_flat_ty()
        .unwrap_or_else(syn::Error::into_compile_error);
    let ref_ty = cx
        .gen_ref_ty()
        .unwrap_or_else(syn::Error::into_compile_error);

    let expanded = quote! {
        #trait_impl
        #flat_ty
        #ref_ty
    };

    proc_macro::TokenStream::from(expanded)
}

struct Cx {
    derivee: syn::Ident,
    vis: syn::Visibility,
    fields: Vec<Field>,
    ref_ty: syn::Ident,
    flat_ty: syn::Ident,
    flat_header_ty: syn::Ident,
}

impl Cx {
    fn with_input(input: syn::DeriveInput) -> syn::Result<Self> {
        let data = match input.data {
            syn::Data::Struct(data) => data,
            _ => unimplemented!(),
        };

        let derivee = input.ident;
        let vis = input.vis;
        let fields = match data.fields {
            syn::Fields::Named(fields) => fields
                .named
                .into_iter()
                .map(Field::with)
                .collect::<syn::Result<_>>()?,
            _ => unimplemented!(),
        };

        let ref_ty = format_ident!("FlatRepr_Ref_{}", derivee);
        let flat_ty = format_ident!("FlatRepr_Flat_{}", derivee);
        let flat_header_ty = format_ident!("FlatRepr_FlatHeader_{}", derivee);

        Ok(Self {
            derivee,
            vis,
            fields,
            ref_ty,
            flat_ty,
            flat_header_ty,
        })
    }
}

struct Field {
    name: syn::Ident,
    vis: syn::Visibility,
    ty: syn::Type,
    behaviour: Behaviour,
}

impl Field {
    fn with(field: syn::Field) -> syn::Result<Self> {
        let behaviour = field
            .attrs
            .iter()
            .find(|attr| attr.meta.path().is_ident("better_repr"))
            .map(|attr| attr.parse_args())
            .unwrap_or(Ok(Behaviour::Copy))?;

        Ok(Field {
            name: field.ident.unwrap(),
            vis: field.vis,
            ty: field.ty,
            behaviour,
        })
    }
}

enum Behaviour {
    Copy,
    InlineString,
    InlineList(syn::Type),
    OutlinedCopyOption(syn::Type),
}

impl Parse for Behaviour {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let behaviour = input.parse::<syn::Ident>()?;

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
        } else if behaviour == "outlined_copy_option" {
            input.parse::<Token![,]>()?;
            input.parse::<Token![<]>()?;
            let ty = input.parse::<syn::Type>()?;
            input.parse::<Token![>]>()?;
            Ok(Behaviour::OutlinedCopyOption(ty))
        } else {
            Err(syn::Error::new(behaviour.span(), "unknown behaviour"))
        }
    }
}
