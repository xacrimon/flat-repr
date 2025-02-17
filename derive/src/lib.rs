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
    ext::IdentExt,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Attribute, DeriveInput, Expr, Token,
};

#[proc_macro_derive(Flattenable, attributes(flat_repr))]
pub fn derive_flattenable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let expanded = derive_flattenable_impl(input).unwrap_or_else(syn::Error::into_compile_error);
    proc_macro::TokenStream::from(expanded)
}

fn derive_flattenable_impl(input: DeriveInput) -> syn::Result<TokenStream> {
    let cx: Cx = Cx::with_input(input)?;
    let trait_impl = cx.gen_trait_impl()?;
    let flat_ty = cx.gen_flat_ty()?;
    let ref_ty = cx.gen_ref_ty()?;

    let expanded = quote! {
        #trait_impl
        #flat_ty
        #ref_ty
    };

    Ok(expanded)
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
        let attr = field
            .attrs
            .iter()
            .find(|attr| attr.meta.path().is_ident("flat_repr"));

        let behaviour = match attr {
            Some(attr) => {
                let name_value = attr.meta.require_name_value()?;
                let expr = &name_value.value;
                let lit: syn::LitStr = parse_quote! { #expr };
                let behaviour = lit.parse()?;
                behaviour
            }
            None => Behaviour::ByValue,
        };

        Ok(Field {
            name: field.ident.unwrap(),
            vis: field.vis,
            ty: field.ty,
            behaviour,
        })
    }
}

enum Behaviour {
    ByValue,
    InlineString,
    InlineList(syn::Type),
    OutlinedCopyOption(syn::Type),
}

impl Parse for Behaviour {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let call_ident = input.call(syn::Ident::parse_any)?;
        let args;

        if call_ident == "self" {
            syn::parenthesized!(args in input);
            let pass_kind = args.parse::<syn::Ident>()?;

            if pass_kind == "by_value" {
                return Ok(Behaviour::ByValue);
            } else {
                return Err(syn::Error::new(pass_kind.span(), "unknown pas kind"));
            }
        }

        if call_ident == "string" {
            syn::parenthesized!(args in input);
            let place = args.parse::<syn::Ident>()?;
            if place != "inline" {
                return Err(syn::Error::new(place.span(), "unknown place"));
            }

            return Ok(Behaviour::InlineString);
        }

        if call_ident == "list" {
            syn::parenthesized!(args in input);
            let ty = args.parse::<syn::Type>()?;
            let _ = args.parse::<Token![,]>();
            let place = args.parse::<syn::Ident>()?;
            if place != "inline" {
                return Err(syn::Error::new(place.span(), "unknown place"));
            }

            return Ok(Behaviour::InlineList(ty));
        }

        if call_ident == "option" {
            syn::parenthesized!(args in input);
            let ty = args.parse::<syn::Type>()?;
            let _ = args.parse::<Token![,]>();
            let pass_kind = args.parse::<syn::Ident>()?;
            if pass_kind != "by_value" {
                return Err(syn::Error::new(pass_kind.span(), "unknown pass kind"));
            }
            let _ = args.parse::<Token![,]>();
            let place = args.parse::<syn::Ident>()?;
            if place != "small_none" {
                return Err(syn::Error::new(place.span(), "unknown option layout"));
            }
            return Ok(Behaviour::OutlinedCopyOption(ty));
        }

        Err(syn::Error::new(call_ident.span(), "unknown call ident"))
    }
}
