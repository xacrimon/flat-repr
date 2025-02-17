use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    token::Token,
    Attribute, DeriveInput, Expr, Ident, Token,
};

use crate::{Behaviour, Cx};

impl Cx {
    pub(super) fn gen_flat_ty(&self) -> syn::Result<TokenStream> {
        let flat_ty = &self.flat_ty;
        let flat_header_ty = &self.flat_header_ty;
        let vis = &self.vis;
        let impl_accessors = self.impl_accessors()?;
        let impl_drop = self.impl_drop()?;
        let impl_header_ty = self.impl_header_ty()?;

        let expanded = quote! {
            #[allow(non_camel_case_types)]
            #[repr(C)]
            #vis struct #flat_ty {
                header: #flat_header_ty,
                tail: [u8]
            }

            impl #flat_ty {
                fn dst_size(&self) -> usize {
                    std::mem::size_of_val::<Self>(self)
                }
            }

            #impl_accessors
            #impl_drop
            #impl_header_ty
        };

        Ok(expanded)
    }

    pub(super) fn gen_to_flat(&self) -> syn::Result<TokenStream> {
        let expanded = quote! {
            fn to_flat(&self) -> Box<Self::FlatRepr> {
                todo!()
            }
        };

        Ok(expanded)
    }

    fn impl_header_ty(&self) -> syn::Result<TokenStream> {
        let flat_header_ty = &self.flat_header_ty;
        let fields = self.fields.iter().map(|field| {
            let name = &field.name;
            let ref_ty = self.replacement_ty(&field.ty, &field.behaviour);
            quote! { #name: #ref_ty }
        });

        let expanded = quote! {
            #[allow(non_camel_case_types)]
            struct #flat_header_ty {
                #(#fields),*
            }
        };

        Ok(expanded)
    }

    fn impl_accessors(&self) -> syn::Result<TokenStream> {
        let vis = &self.vis;
        let flat_ty = &self.flat_ty;

        let accessors = self
            .fields
            .iter()
            .map(|field| {
                let li = parse_quote! { '_ };
                let name = &field.name;
                let ret_ty = self.field_ref_ty(&field.ty, &field.behaviour, &li);
                let ref_expr = self.field_access_expr(&field.name, &field.behaviour)?;

                let expanded = quote! {
                    #vis fn #name(&self) -> #ret_ty {
                        #ref_expr
                    }
                };

                Ok(expanded)
            })
            .collect::<syn::Result<Vec<_>>>()?;

        let expanded = quote! {
            impl #flat_ty {
                #(#accessors)*
            }
        };

        Ok(expanded)
    }

    fn field_access_expr(
        &self,
        field: &syn::Ident,
        behaviour: &Behaviour,
    ) -> syn::Result<TokenStream> {
        let offset: Expr = parse_quote! { self.header.#field.offset as isize };
        let len: Expr = parse_quote! { self.header.#field.len as usize };
        let ptr = quote! { (&raw const self.tail) };
        let ptr = quote! { #ptr.cast::<u8>().offset(#offset) };

        let expanded = match behaviour {
            Behaviour::Copy => quote! { self.header.#field },
            Behaviour::InlineString => {
                quote! { unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(#ptr, #len)) } }
            }
            Behaviour::InlineList(elem_ty) => {
                quote! { unsafe { std::slice::from_raw_parts(#ptr.cast::<#elem_ty>(), #len) } }
            }
        };

        Ok(expanded)
    }

    fn impl_drop(&self) -> syn::Result<TokenStream> {
        let flat_ty = &self.flat_ty;
        let destructors = self.fields.iter().map(|field| {
            let name = &field.name;
            let behaviour = &field.behaviour;

            let offset: Expr = parse_quote! { self.header.#name.offset as isize };
            let len: Expr = parse_quote! { self.header.#name.len as usize };
            let ptr = quote! { (&raw mut self.tail) };
            let ptr = quote! { #ptr.cast::<u8>().offset(#offset) };

            match behaviour {
                Behaviour::Copy | Behaviour::InlineString => quote! {},
                Behaviour::InlineList(elem_ty) => {
                    let slice_ptr = quote! { std::ptr::slice_from_raw_parts_mut(#ptr.cast::<#elem_ty>(), #len) };
                    quote! {
                        unsafe {
                            std::ptr::drop_in_place(#slice_ptr);
                        }
                    }
                }
            }
        });

        let expanded = quote! {
            impl Drop for #flat_ty {
                fn drop(&mut self) {
                    #(#destructors;)*
                }
            }
        };

        Ok(expanded)
    }

    fn replacement_ty(&self, ty: &syn::Type, behaviour: &Behaviour) -> syn::Type {
        match behaviour {
            Behaviour::Copy => parse_quote! { #ty },
            Behaviour::InlineString | Behaviour::InlineList(_) => {
                parse_quote! { better_repr::DynAlloc }
            }
        }
    }
}
