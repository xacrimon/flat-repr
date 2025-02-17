use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Attribute, DeriveInput, Expr, Ident, Token,
};

use crate::{named_fields, repr_ref, Behaviour};

pub fn gen(input: &DeriveInput) -> syn::Result<TokenStream> {
    let ty_vis = &input.vis;
    let input_ty_name = &input.ident;
    let ty_name = format_ident!("BetterRepr_Flat_{}", input.ident);
    let ty_name_head = format_ident!("BetterRepr_Flat_Head_{}", input.ident);

    let fields: Vec<_> = named_fields(&input.data)?
        .into_iter()
        .map(|(span, name, _, ty, behaviour)| {
            let repr_ty = pick_repr_flat_type(ty, &behaviour);
            quote_spanned! { span => #name: #repr_ty }
        })
        .collect();

    let accessors =
        named_fields(&input.data)?
            .into_iter()
            .map(|(span, name, vis, ty, behaviour)| {
                let li = parse_quote! { '_ };
                let ret_ty = repr_ref::mapped_ty(ty, &behaviour, &li);
                let accessor = gen_repr_flat_field_ref(name, &behaviour);

                quote_spanned! { span =>
                    #vis fn #name(&self) -> #ret_ty {
                        #accessor
                    }
                }
            });

    let to_flat = gen_repr_flat_to_flat(
        &ty_name,
        &&ty_name_head,
        named_fields(&input.data)?
            .into_iter()
            .map(|(span, name, _, ty, behaviour)| (span, name, ty, behaviour))
            .collect(),
    );

    let drop_impl = gen_repr_flat_drop_impl(
        &ty_name,
        named_fields(&input.data)?
            .into_iter()
            .map(|(span, name, _, ty, behaviour)| (name, ty, behaviour))
            .collect(),
    );

    let expanded = quote! {
        #[allow(non_camel_case_types)]
        #[repr(C)]
        #ty_vis struct #ty_name {
            head: #ty_name_head,
            ds: [u8]
        }

        #[allow(non_camel_case_types)]
        #ty_vis struct #ty_name_head {
            #(#fields),*
        }

        impl #ty_name {
            #ty_vis fn size(&self) -> usize {
                std::mem::size_of_val::<Self>(self)
            }

            #(#accessors)*
        }

        impl better_repr::ReprFlat for #input_ty_name {
            type Ty = #ty_name;

            #to_flat
        }

        #drop_impl
    };

    Ok(expanded)
}

fn pick_repr_flat_type(ty: &syn::Type, behaviour: &Behaviour) -> syn::Type {
    match behaviour {
        Behaviour::Copy => parse_quote! { #ty },
        Behaviour::InlineString | Behaviour::InlineList(_) => {
            parse_quote! { better_repr::DynAlloc }
        }
    }
}

fn gen_repr_flat_field_ref(field: &syn::Ident, behaviour: &Behaviour) -> syn::Expr {
    let offset: Expr = parse_quote! { self.head.#field.offset as isize };
    let len: Expr = parse_quote! { self.head.#field.len as usize };

    match behaviour {
        Behaviour::Copy => parse_quote! { self.head.#field },
        Behaviour::InlineString => {
            let ptr = quote! { (&raw const (*(self as *const Self)).ds) };
            let ptr = quote! { #ptr.cast::<u8>().offset(#offset) };
            parse_quote! { unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(#ptr, #len)) } }
        }
        Behaviour::InlineList(elem_ty) => {
            let ptr = quote! { (&raw const (*(self as *const Self)).ds) };
            let ptr = quote! { #ptr.cast::<u8>().offset(#offset).cast::<#elem_ty>() };
            parse_quote! { unsafe { std::slice::from_raw_parts(#ptr, #len) } }
        }
    }
}

fn gen_repr_flat_to_flat(
    ty_name: &Ident,
    ty_name_head: &Ident,
    fields: Vec<(Span, &syn::Ident, &syn::Type, Behaviour)>,
) -> syn::TraitItemFn {
    let fixed_sz: Expr = parse_quote! { std::mem::size_of::<#ty_name_head>() };
    let fixed_align: Expr = parse_quote! { std::mem::align_of::<#ty_name_head>() };

    let mut alloc_setups = Vec::new();
    let mut alloc_initializers = Vec::new();
    let mut alloc_vars = HashMap::new();
    for (span, name, field_ty, behaviour) in &fields {
        let (size, align): (Expr, Expr) = match behaviour {
            Behaviour::Copy => continue,
            Behaviour::InlineString => (parse_quote! { self.#name.len() }, parse_quote! { 1 }),
            Behaviour::InlineList(elem_ty) => (
                parse_quote! { self.#name.len() * std::mem::size_of::<#elem_ty>() },
                parse_quote! { std::mem::align_of::<#elem_ty>() },
            ),
        };

        let get_offset = quote! {
            {
                while next_offset % #align != 0 {
                    next_offset += 1;
                }

                let offset = next_offset;
                next_offset += #size;

                offset
            }
        };

        let mk_alloc = quote! {
            {
                let offset = #get_offset;
                let len = #size;
                better_repr::DynAlloc { offset: offset as u16, len: (len / #align) as u16 }
            }
        };

        let alloc_var = format_ident!("{}_dyn_alloc", name);
        alloc_setups.push(quote! { let #alloc_var = #mk_alloc; });
        alloc_vars.insert(*name, alloc_var.clone());

        let write_data = match behaviour {
            Behaviour::Copy => unreachable!(),
            Behaviour::InlineString => {
                let src = quote! { self.#name.as_ptr() };

                quote! {
                    let dst = &raw mut (*dst).ds;
                    let dst = dst.cast::<u8>().offset(#alloc_var.offset as isize);
                    std::ptr::copy_nonoverlapping(#src, dst, #alloc_var.len as usize);
                }
            }
            Behaviour::InlineList(elem_ty) => {
                let src = quote! { self.#name.as_ptr() };

                quote! {
                    let dst = &raw mut (*dst).ds;
                    let dst = dst.cast::<#elem_ty>().byte_offset(#alloc_var.offset as isize);
                    std::ptr::copy_nonoverlapping(#src, dst, #alloc_var.len as usize);
                }
            }
        };

        alloc_initializers.push(quote! { unsafe { #write_data } });
    }

    let initializers = fields.into_iter().map(|(span, name, ty, behaviour)| {
        if let Behaviour::Copy = behaviour {
            quote! { #name: self.#name }
        } else {
            let alloc_var = &alloc_vars[name];
            quote! { #name: #alloc_var }
        }
    });

    let write_head = quote! {
        {
            let head = #ty_name_head {
                #(#initializers),*
            };

            let head_ptr = dst as *mut #ty_name_head;
            unsafe { head_ptr.write(head); }
        }
    };

    // TODO: fix this
    let box_slice_len = quote! { #fixed_sz + next_offset };
    let combined_layout = quote! { std::alloc::Layout::from_size_align(#fixed_sz + next_offset, #fixed_align).unwrap() };

    parse_quote! {
        fn to_flat(&self) -> Box<Self::Ty> {
            let mut next_offset: usize = 0;
            #(#alloc_setups)*

            dbg!(#fixed_sz);
            dbg!(#box_slice_len);
            let dst = Box::<[std::mem::MaybeUninit<u8>]>::new_uninit_slice(#box_slice_len);
            dbg!(std::mem::size_of_val(&*dst));
            let dst = std::ptr::slice_from_raw_parts_mut(Box::into_raw(dst) as *mut u8, next_offset);
            let dst = dst as *mut Self::Ty;
            unsafe {dbg!(std::mem::size_of_val(&*dst));}

            #write_head

            #(#alloc_initializers)*

            unsafe { Box::from_raw(dst) }
        }
    }
}

fn gen_repr_flat_drop_impl(
    ty_name: &Ident,
    fields: Vec<(&syn::Ident, &syn::Type, Behaviour)>,
) -> syn::ItemImpl {
    fn drop_field(field: &syn::Ident, ty: &syn::Type, behaviour: Behaviour) -> syn::Expr {
        let offset: Expr = parse_quote! { self.head.#field.offset as isize };
        let len: Expr = parse_quote! { self.head.#field.len as usize };

        match behaviour {
            Behaviour::Copy => parse_quote! { {} }, // don't need to copy Copy types
            Behaviour::InlineString => parse_quote! { {} }, // inline strings are just a slice of u8's, nothing to drop
            Behaviour::InlineList(elem_ty) => {
                // TODO: only drop if not copy?
                let ptr =
                    quote! { (self as *mut Self).cast::<u8>().offset(#offset).cast::<#elem_ty>() };
                let slice_ptr =
                    quote! { unsafe { std::ptr::slice_from_raw_parts_mut(#ptr, #len) } };
                parse_quote! { unsafe { std::ptr::drop_in_place(#slice_ptr) } }
            }
        }
    }

    let drop_fields = fields
        .into_iter()
        .map(|(name, ty, behaviour)| drop_field(name, ty, behaviour));

    parse_quote! {
        impl Drop for #ty_name {
            fn drop(&mut self) {
                #(#drop_fields);*
            }
        }
    }
}
