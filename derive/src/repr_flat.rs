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

use crate::{Behaviour, Cx, Field};

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
        let flat_header_ty = &self.flat_header_ty;
        let header_size: Expr = parse_quote! { std::mem::size_of::<#flat_header_ty>() };
        let header_align: Expr = parse_quote! { std::mem::align_of::<#flat_header_ty>() };

        let mut assign_offsets = Vec::new();
        let mut initialize_tail = Vec::new();
        let mut offset_for_field = HashMap::new();

        for Field {
            name, behaviour, ..
        } in &self.fields
        {
            let (len, elem_size, align): (Expr, Expr, Expr) = match behaviour {
                Behaviour::Copy => continue,
                Behaviour::InlineString => (
                    parse_quote! { self.#name.len() },
                    parse_quote! { 1 },
                    parse_quote! { 1 },
                ),
                Behaviour::InlineList(elem_ty) => (
                    parse_quote! { self.#name.len() },
                    parse_quote! { std::mem::size_of::<#elem_ty>() },
                    parse_quote! { std::mem::align_of::<#elem_ty>() },
                ),
                Behaviour::OutlinedCopyOption(inner_ty) => (
                    parse_quote! { 1 },
                    parse_quote! { std::mem::size_of::<#inner_ty>() },
                    parse_quote! { std::mem::align_of::<#inner_ty>() },
                ),
            };

            let mk_alloc = if let Behaviour::OutlinedCopyOption(_) = behaviour {
                quote! { {
                    if self.#name.is_some() {
                        better_repr::DynAlloc::next_offset(&mut next_offset, 1, #elem_size, #align, #header_align)
                    } else {
                        better_repr::DynAlloc::<()>::NONE
                    }
                } }
            } else {
                quote! { better_repr::DynAlloc::next_offset(&mut next_offset, #len, #elem_size, #align, #header_align) }
            };

            let offset_var = format_ident!("{}_dyn_alloc", name);
            assign_offsets.push(quote! { let #offset_var = #mk_alloc; });
            offset_for_field.insert(name.clone(), offset_var.clone());

            let write_data = match behaviour {
                Behaviour::Copy => unreachable!(),
                Behaviour::InlineString => {
                    let src = quote! { self.#name.as_ptr() };

                    quote! {
                        let dst = &raw mut (*dst).tail;
                        let dst = dst.cast::<u8>().offset(#offset_var.offset as isize);
                        std::ptr::copy_nonoverlapping(#src, dst, #offset_var.len as usize);
                    }
                }
                Behaviour::InlineList(elem_ty) => {
                    let src = quote! { self.#name.as_ptr() };

                    quote! {
                        let dst = &raw mut (*dst).tail;
                        let dst = dst.cast::<#elem_ty>().byte_offset(#offset_var.offset as isize);
                        std::ptr::copy_nonoverlapping(#src, dst, #offset_var.len as usize);
                    }
                }
                Behaviour::OutlinedCopyOption(inner_ty) => {
                    let src = quote! {self.#name.as_ref().map(|r| &raw const *r) };

                    quote! {
                        if let Some(src) = #src {
                            let dst = &raw mut (*dst).tail;
                            let dst = dst.cast::<#inner_ty>().byte_offset(#offset_var.offset as isize);
                            std::ptr::copy_nonoverlapping(src, dst, 1);
                        }
                    }
                }
            };

            initialize_tail.push(quote! { unsafe { #write_data } });
        }

        let header_initializers = self.fields.iter().map(|field| {
            let name = &field.name;
            let behaviour = &field.behaviour;

            if let Behaviour::Copy = behaviour {
                quote! { #name: self.#name }
            } else {
                let alloc_var = &offset_for_field[name];
                quote! { #name: #alloc_var }
            }
        });

        let write_head = quote! {
            {
                let head = #flat_header_ty {
                    #(#header_initializers),*
                };

                let head_ptr = dst as *mut #flat_header_ty;
                unsafe { head_ptr.write(head); }
            }
        };

        // TODO: fix this
        let box_slice_len = quote! { #header_size + next_offset };
        let combined_layout = quote! { std::alloc::Layout::from_size_align(#header_size + next_offset, #header_align).unwrap() };

        let expanded = quote! {
            fn to_flat(&self) -> Box<Self::FlatRepr> {
                let mut next_offset: usize = 0;
                #(#assign_offsets)*

                let dst = Box::<[std::mem::MaybeUninit<u8>]>::new_uninit_slice(#box_slice_len);
                let dst = std::ptr::slice_from_raw_parts_mut(Box::into_raw(dst) as *mut u8, next_offset);
                let dst = dst as *mut Self::FlatRepr;

                #write_head

                #(#initialize_tail)*

                unsafe { Box::from_raw(dst) }
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
            Behaviour::OutlinedCopyOption(inner_ty) => {
                quote! { unsafe { if self.header.#field != better_repr::DynAlloc::<()>::NONE {Some(&*#ptr.cast::<#inner_ty>())} else {None} } }
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
                Behaviour::OutlinedCopyOption(inner_ty) => {
                    quote! {
                        if self.header.#name != better_repr::DynAlloc::<()>::NONE {
                            unsafe {
                                let ptr = #ptr.cast::<#inner_ty>();
                                std::ptr::drop_in_place(ptr);
                            }
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
            Behaviour::OutlinedCopyOption(_) => {
                parse_quote! { better_repr::DynAlloc<()> }
            }
        }
    }
}
