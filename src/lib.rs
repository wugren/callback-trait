use proc_macro2::{Ident, TokenStream};
use quote::{quote};
use syn::{FnArg, GenericParam, Generics, ItemTrait, Lifetime, parse_macro_input, Pat, PathArguments, ReturnType, Signature, Token, TraitItem, Type, TypeParamBound, WhereClause};
use syn::__private::Span;
use syn::punctuated::Punctuated;
use syn::token::SelfValue;

#[proc_macro_attribute]
pub fn callback_trait(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as ItemTrait);
    impl_macro(input, true).unwrap_or_else(to_compile_errors).into()
}

#[proc_macro_attribute]
pub fn unsafe_callback_trait(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as ItemTrait);
    impl_macro(input, false).unwrap_or_else(to_compile_errors).into()
}

fn impl_macro(input: ItemTrait, is_safe: bool) -> Result<TokenStream, Vec<syn::Error>> {
    let name = &input.ident;
    let original_generics = &input.generics;
    let items = &input.items;
    let func_items = items.iter().filter(|item| {
        if let TraitItem::Fn(_) = item {
            true
        } else {
            false
        }
    }).collect::<Vec<_>>();
    let unfunc_items = items.iter().filter(|item| {
        if let TraitItem::Fn(_) = item {
            false
        } else {
            true
        }
    }).collect::<Vec<_>>();
    let func_count = func_items.len();
    if func_count != 1 {
        return Err(vec![syn::Error::new_spanned(
            input,
            "expected exactly one method",
        )]);
    }
    let (func_attr, func_sign) = if let TraitItem::Fn(func_item) = &func_items[0] {
        (&func_item.attrs, &func_item.sig)
    } else {
        unreachable!()
    };


    let func_proc = generate_func_impl(func_sign, is_safe);
    let generics = generate_generics(func_sign, original_generics, is_safe);

    let async_trait = if func_sign.asyncness.is_some() {
        quote! {#[async_trait::async_trait]}
    } else {
        quote! {}
    };
    let where_clause = &generics.where_clause;
    let expanded = quote! {
            #async_trait
            #input

            #async_trait
            impl #generics #name #original_generics for ______F___ #where_clause {
                #(#unfunc_items)*
                #(#func_attr)*
                #func_sign {
                    #func_proc
                }
            }
        };
    Ok(expanded)
}

fn is_async_trait_impl(ty: &Box<Type>) -> bool {
    if let Type::Path(p) = ty.as_ref() {
        for seg in p.path.segments.iter() {
            if seg.ident.to_string() == "Pin" {
                if let PathArguments::AngleBracketed(arg) = &seg.arguments {
                    for sub_arg in arg.args.iter() {
                        if let syn::GenericArgument::Type(ty) = sub_arg {
                            if let Type::Path(p) = ty {
                                for seg in p.path.segments.iter() {
                                    if seg.ident.to_string() == "Box" {
                                        if let PathArguments::AngleBracketed(arg) = &seg.arguments {
                                            for sub_arg in arg.args.iter() {
                                                if let syn::GenericArgument::Type(ty) = sub_arg {
                                                    if let Type::TraitObject(ty_trait) = ty {
                                                        for bound in ty_trait.bounds.iter() {
                                                            if let TypeParamBound::Trait(trait_bound) = bound {
                                                                for seg in trait_bound.path.segments.iter() {
                                                                    if seg.ident.to_string() == "Future" {
                                                                        return true
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

fn get_async_trait_furture_bounds(ty: &Box<Type>) -> Option<Punctuated<TypeParamBound, Token![+]>> {
    if let Type::Path(p) = ty.as_ref() {
        for seg in p.path.segments.iter() {
            if seg.ident.to_string() == "Pin" {
                if let PathArguments::AngleBracketed(arg) = &seg.arguments {
                    for sub_arg in arg.args.iter() {
                        if let syn::GenericArgument::Type(ty) = sub_arg {
                            if let Type::Path(p) = ty {
                                for seg in p.path.segments.iter() {
                                    if seg.ident.to_string() == "Box" {
                                        if let PathArguments::AngleBracketed(arg) = &seg.arguments {
                                            for sub_arg in arg.args.iter() {
                                                if let syn::GenericArgument::Type(ty) = sub_arg {
                                                    if let Type::TraitObject(ty_trait) = ty {
                                                        for bound in ty_trait.bounds.iter() {
                                                            if let TypeParamBound::Trait(trait_bound) = bound {
                                                                for seg in trait_bound.path.segments.iter() {
                                                                    if seg.ident.to_string() == "Future" {
                                                                        let mut ty = Box::new(ty.clone());
                                                                        if let Type::TraitObject(ty_trait) = ty.as_mut() {
                                                                            for bound in ty_trait.bounds.iter_mut() {
                                                                                if let TypeParamBound::Lifetime(lifetime) = bound {
                                                                                    lifetime.apostrophe = Span::call_site();
                                                                                    lifetime.ident = Ident::new("static", Span::call_site());
                                                                                }
                                                                            }
                                                                            return Some(ty_trait.bounds.clone());
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn generate_generics(func: &Signature, generics: &Generics, is_safe: bool) -> Generics {
    let rt = if let ReturnType::Type(_, ty) = &func.output {
        Some(ty.clone())
    } else {
        None
    };
    let input_types = func.inputs.iter().filter(|item| {
        if let FnArg::Receiver(_) = item {
            false
        } else {
            true
        }

    }).map(|item| {
        match item {
            FnArg::Receiver(v) => {
                v.ty.clone()
            }
            FnArg::Typed(v) => {
                let mut ty = v.ty.clone();
                if let Type::Reference(r) = ty.as_mut() {
                    if is_safe {
                        r.lifetime = None;
                    } else {
                        r.lifetime = Some(Lifetime::new("'static", Span::call_site()));
                    }
                }
                ty
            }
        }
    }).collect::<Punctuated<Box<Type>, Token![,]>>();

    let mut generics = generics.clone();
    if func.asyncness.is_none() {
        generics.params.push(GenericParam::Type(syn::parse_quote! { ______F___ }));
        if rt.is_some() && is_async_trait_impl(rt.as_ref().unwrap()) {
            generics.params.push(GenericParam::Type(syn::parse_quote! { ______Fut___ }));
            let fut = get_async_trait_furture_bounds(rt.as_ref().unwrap()).unwrap();
            if generics.where_clause.is_none() {
                generics.where_clause = Some(syn::parse_quote! {
                where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)->______Fut___,
                ______Fut___: #fut
            });
            } else {
                let where_clause: WhereClause = syn::parse_quote! {
                    where ______F___: core::marker::Send + core::marker::Sync + 'static +  Fn(#input_types)->______Fut___,
                    ______Fut___: #fut
                };
                generics.where_clause.as_mut().unwrap().predicates.extend(where_clause.predicates);
            }
        } else {
            if rt.is_some() {
                if generics.where_clause.is_none() {
                    generics.where_clause = Some(syn::parse_quote! {
                where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)->#rt
            });
                } else {
                    let where_clause: WhereClause = syn::parse_quote! {
                where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)->#rt
            };
                    generics.where_clause.as_mut().unwrap().predicates.extend(where_clause.predicates);
                }
            } else {
                if generics.where_clause.is_none() {
                    generics.where_clause = Some(syn::parse_quote! {
                        where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)
                    });
                } else {
                    let where_clause: WhereClause = syn::parse_quote! {
                        where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)
                    };
                    generics.where_clause.as_mut().unwrap().predicates.extend(where_clause.predicates);
                }
            }
        }
    } else {
        generics.params.push(GenericParam::Type(syn::parse_quote! { ______F___ }));
        generics.params.push(GenericParam::Type(syn::parse_quote! { ______Fut___ }));
        if rt.is_some() {
            if generics.where_clause.is_none() {
                generics.where_clause = Some(syn::parse_quote! {
                    where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)->______Fut___,
                    ______Fut___: core::future::Future<Output=#rt> + 'static + core::marker::Send
                });
            } else {
                let where_clause: WhereClause = syn::parse_quote! {
                    where ______F___: core::marker::Send + core::marker::Sync + 'static +  Fn(#input_types)->______Fut___,
                    ______Fut___: core::future::Future<Output=#rt> + 'static + core::marker::Send
                };
                generics.where_clause.as_mut().unwrap().predicates.extend(where_clause.predicates);
            }
        } else {
            if generics.where_clause.is_none() {
                generics.where_clause = Some(syn::parse_quote! {
                    where ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(#input_types)->______Fut___,
                    ______Fut___: core::future::Future<Output=()> + 'static + core::marker::Send
                });
            } else {
                let where_clause: WhereClause = syn::parse_quote! {
                    where ______F___: core::marker::Send + core::marker::Sync + 'static +  Fn(#input_types)->______Fut___,
                    ______Fut___: core::future::Future<Output=()> + 'static + core::marker::Send
                };
                generics.where_clause.as_mut().unwrap().predicates.extend(where_clause.predicates);
            }
        }

    }
    generics
}

fn generate_func_impl(func: &Signature, is_safe: bool) -> TokenStream {
    let types = func.inputs.iter().filter(|item| {
        if let FnArg::Receiver(_) = item {
            false
        } else {
            true
        }
    }).map(|item| {
        match item {
            FnArg::Typed(v) => {
                &v.pat
            }
            _ => unreachable!()
        }
    }).collect::<Punctuated<&Box<Pat>, Token![,]>>();

    let receivers = func.inputs.iter().filter(|item| {
        if let FnArg::Receiver(_) = item {
            true
        } else {
            false
        }
    }).map(|item| {
        match item {
            FnArg::Receiver(v) => {
                &v.self_token
            }
            _ => unreachable!()
        }
    }).collect::<Punctuated<&SelfValue, Token![,]>>();

    let static_cast = if is_safe {
        Vec::new()
    } else {
        func.inputs.iter().filter(|item| {
            match item {
                FnArg::Receiver(_) => {
                    false
                }
                FnArg::Typed(v) => {
                    if let Type::Reference(_) = v.ty.as_ref() {
                        true
                    } else {
                        false
                    }
                }
            }
        }).map(|item| {
            match item {
                FnArg::Typed(v) => {
                    let ty = &v.ty;
                    let pat = &v.pat;
                    if let Type::Reference(mut r) = ty.as_ref().clone() {
                        r.lifetime = Some(Lifetime::new("'static", Span::call_site()));
                        quote! {
                            let #pat: #r = unsafe { ::core::mem::transmute(#pat) };
                        }
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!()
            }
        }).collect::<Vec<TokenStream>>()
    };

    if func.asyncness.is_none() {
        let func_impl = if let ReturnType::Type(_, ty) = &func.output {
            if is_async_trait_impl(ty) {
                quote! {
                    Box::pin(async move {
                        #(#static_cast)*
                        let fut = (#receivers)(#types);
                        fut.await
                    })
                }
            } else {
                quote!{
                    (#receivers)(#types)
                }
            }
        } else {
            quote!{
                (#receivers)(#types)
            }
        };
        quote! {
            #func_impl
        }
    } else {
        quote! {
            #(#static_cast)*
            let fut = (#receivers)(#types);
            fut.await
        }
    }
}
fn to_compile_errors(errors: Vec<syn::Error>) -> TokenStream {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    quote!(#(#compile_errors)*)
}

#[test]
fn test_impl_macro() {
    let input = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
    };
    let result = impl_macro(input, true).unwrap();
    let expected = quote! {
        #[async_trait::async_trait]
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
        #[async_trait::async_trait]
        impl<______F___, ______Fut___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&u32, i16) -> ______Fut___,
                ______Fut___: core::future::Future<Output=Result<(), u32> > + 'static + core::marker::Send
        {
            async fn call(&self, p1: &u32, p2: i16) -> Result<(), u32> {
                let fut = (self)(p1, p2);
                fut.await
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro2() {
    let input = syn::parse_quote! {
        pub trait SampleTrait<T>: 'static + Send + Sync where T: Send {
            async fn call(&self, p1: u256, p2: u32, t: T) -> Result<u64, Error>;
        }
    };
    let result = impl_macro(input, true).unwrap();
    let expected = quote! {
        #[async_trait::async_trait]
        pub trait SampleTrait<T>: 'static + Send + Sync where T: Send {
            async fn call(&self, p1: u256, p2: u32, t: T) -> Result<u64, Error>;
        }
        #[async_trait::async_trait]
        impl<T, ______F___, ______Fut___> SampleTrait<T> for ______F___
            where
                T: Send,
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(u256, u32, T) -> ______Fut___,
                ______Fut___: core::future::Future<Output=Result<u64, Error> > + 'static + core::marker::Send
        {
            async fn call(&self, p1: u256, p2: u32, t: T) -> Result<u64, Error> {
                let fut = (self)(p1, p2, t);
                fut.await
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro3() {
    let input = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            #[must_use]
            #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
            fn call<'life0, 'async_trait>(
                &'life0 self,
                p1: u64,
                p2: u32,
            ) -> ::core::pin::Pin<
                Box<
                    dyn ::core::future::Future<Output = Result<u64, i32>>
                        + ::core::marker::Send
                        + 'async_trait,
                >,
            >
            where
                'life0: 'async_trait,
                Self: 'async_trait;
        }

    };
    let result = impl_macro(input, true).unwrap();
    let expected = quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            #[must_use]
            #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
            fn call<'life0, 'async_trait>(
                &'life0 self,
                p1: u64,
                p2: u32,
            ) -> ::core::pin::Pin<
                Box<
                    dyn ::core::future::Future<Output = Result<u64, i32> >
                        + ::core::marker::Send
                        + 'async_trait,
                >,
            >
            where
                'life0: 'async_trait,
                Self: 'async_trait;
        }

        impl<______F___, ______Fut___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(u64, u32) -> ______Fut___,
                ______Fut___: ::core::future::Future<Output=Result<u64, i32> > + ::core::marker::Send + 'static
        {
            #[must_use]
            #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
            fn call<'life0, 'async_trait>(
                &'life0 self,
                p1: u64,
                p2: u32,
            ) -> ::core::pin::Pin<
                Box<
                    dyn ::core::future::Future<Output = Result<u64, i32> >
                        + ::core::marker::Send
                        + 'async_trait,
                >,
            >
            where
                'life0: 'async_trait,
                Self: 'async_trait {
            Box::pin(async move {
                let fut = (self)(p1, p2);
                fut.await
            })
        }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro4() {
    let input = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
    };
    let result = impl_macro(input, true).unwrap();
    let expected = quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
        impl<______F___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&u32, i16) -> Result<(), u32>
        {
            fn call(&self, p1: &u32, p2: i16) -> Result<(), u32> {
                (self)(p1, p2)
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro5() {
    let input: ItemTrait = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16);
        }
    };
    let result = impl_macro(input, true).unwrap();
    let expected = quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16);
        }
        impl<______F___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&u32, i16)
        {
            fn call(&self, p1: &u32, p2: i16) {
                (self)(p1, p2)
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro6() {
    let input: ItemTrait = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16);
        }
    };
    let result = impl_macro(input, true).unwrap();
    let expected = quote! {
        #[async_trait::async_trait]
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16);
        }
        #[async_trait::async_trait]
        impl<______F___,______Fut___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&u32, i16) -> ______Fut___,
                ______Fut___: core::future::Future<Output=() > + 'static + core::marker::Send
        {
            async fn call(&self, p1: &u32, p2: i16) {
                let fut = (self)(p1, p2);
                fut.await
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro7() {
    let input = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
    };
    let result = impl_macro(input, false).unwrap();
    let expected = quote! {
        #[async_trait::async_trait]
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
        #[async_trait::async_trait]
        impl<______F___, ______Fut___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&'static u32, i16) -> ______Fut___,
                ______Fut___: core::future::Future<Output=Result<(), u32> > + 'static + core::marker::Send
        {
            async fn call(&self, p1: &u32, p2: i16) -> Result<(), u32> {
                let p1: &'static u32 = unsafe {::core::mem::transmute(p1)};
                let fut = (self)(p1, p2);
                fut.await
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro8() {
    let input = syn::parse_quote! {
        pub trait SampleTrait<T>: 'static + Send + Sync where T: Send {
            async fn call(&self, p1: u256, p2: u32, t: T) -> Result<u64, Error>;
        }
    };
    let result = impl_macro(input, false).unwrap();
    let expected = quote! {
        #[async_trait::async_trait]
        pub trait SampleTrait<T>: 'static + Send + Sync where T: Send {
            async fn call(&self, p1: u256, p2: u32, t: T) -> Result<u64, Error>;
        }
        #[async_trait::async_trait]
        impl<T, ______F___, ______Fut___> SampleTrait<T> for ______F___
            where
                T: Send,
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(u256, u32, T) -> ______Fut___,
                ______Fut___: core::future::Future<Output=Result<u64, Error> > + 'static + core::marker::Send
        {
            async fn call(&self, p1: u256, p2: u32, t: T) -> Result<u64, Error> {
                let fut = (self)(p1, p2, t);
                fut.await
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro9() {
    let input = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            #[must_use]
            #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
            fn call<'life0, 'async_trait>(
                &'life0 self,
                p1: u64,
                p2: u32,
            ) -> ::core::pin::Pin<
                Box<
                    dyn ::core::future::Future<Output = Result<u64, i32>>
                        + ::core::marker::Send
                        + 'async_trait,
                >,
            >
            where
                'life0: 'async_trait,
                Self: 'async_trait;
        }

    };
    let result = impl_macro(input, false).unwrap();
    let expected = quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            #[must_use]
            #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
            fn call<'life0, 'async_trait>(
                &'life0 self,
                p1: u64,
                p2: u32,
            ) -> ::core::pin::Pin<
                Box<
                    dyn ::core::future::Future<Output = Result<u64, i32> >
                        + ::core::marker::Send
                        + 'async_trait,
                >,
            >
            where
                'life0: 'async_trait,
                Self: 'async_trait;
        }

        impl<______F___, ______Fut___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(u64, u32) -> ______Fut___,
                ______Fut___: ::core::future::Future<Output=Result<u64, i32> > + ::core::marker::Send + 'static
        {
            #[must_use]
            #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
            fn call<'life0, 'async_trait>(
                &'life0 self,
                p1: u64,
                p2: u32,
            ) -> ::core::pin::Pin<
                Box<
                    dyn ::core::future::Future<Output = Result<u64, i32> >
                        + ::core::marker::Send
                        + 'async_trait,
                >,
            >
            where
                'life0: 'async_trait,
                Self: 'async_trait {
            Box::pin(async move {
                let fut = (self)(p1, p2);
                fut.await
            })
        }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro10() {
    let input = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
    };
    let result = impl_macro(input, false).unwrap();
    let expected = quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16) -> Result<(), u32>;
        }
        impl<______F___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&'static u32, i16) -> Result<(), u32>
        {
            fn call(&self, p1: &u32, p2: i16) -> Result<(), u32> {
                (self)(p1, p2)
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro11() {
    let input: ItemTrait = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16);
        }
    };
    let result = impl_macro(input, false).unwrap();
    let expected = quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            fn call(&self, p1: &u32, p2: i16);
        }
        impl<______F___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&'static u32, i16)
        {
            fn call(&self, p1: &u32, p2: i16) {
                (self)(p1, p2)
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}

#[test]
fn test_impl_macro12() {
    let input: ItemTrait = syn::parse_quote! {
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16);
        }
    };
    let result = impl_macro(input, false).unwrap();
    let expected = quote! {
        #[async_trait::async_trait]
        pub trait SampleTrait: 'static + Send + Sync {
            async fn call(&self, p1: &u32, p2: i16);
        }
        #[async_trait::async_trait]
        impl<______F___,______Fut___> SampleTrait for ______F___
            where
                ______F___: core::marker::Send + core::marker::Sync + 'static + Fn(&'static u32, i16) -> ______Fut___,
                ______Fut___: core::future::Future<Output=() > + 'static + core::marker::Send
        {
            async fn call(&self, p1: &u32, p2: i16) {
                let p1: &'static u32 = unsafe {::core::mem::transmute(p1)};
                let fut = (self)(p1, p2);
                fut.await
            }
        }
    };
    assert_eq!(result.to_string(), expected.to_string());
}
