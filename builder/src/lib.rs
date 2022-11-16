use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, AngleBracketedGenericArguments,
    Data, DataStruct, DeriveInput, Error, Fields, FieldsNamed, Ident, Lit, Meta, MetaNameValue,
    NestedMeta, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    let origin_ident = input.ident;
    let builder_ident = format_ident!("{origin_ident}Builder");

    let fields = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => named,
        _ => unimplemented!(),
    };

    let definition = {
        let fields = fields.iter().map(|f| {
            let ident = &f.ident;
            let ty = &f.ty;
            match name_of_type(ty).as_str() {
                "Option" => quote! { #ident: #ty },
                _ => quote! { #ident: std::option::Option<#ty> },
            }
        });
        quote! {
            pub struct #builder_ident {
                #(#fields,)*
            }
        }
    };

    let constructor = {
        let idents = fields.iter().map(|f| &f.ident);
        quote! {
            impl #origin_ident {
                pub fn builder() -> #builder_ident {
                    #builder_ident {
                        #(#idents: std::option::Option::None,)*
                    }
                }
            }
        }
    };

    let implmentation = {
        fn setter(ident: impl ToTokens, ty: impl ToTokens) -> proc_macro2::TokenStream {
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        }

        let setters = fields.iter().map(|f| {
            let ident = f.ident.as_ref().unwrap();
            let ty = &f.ty;
            // 1. get attributes
            // 2. if exists and path is builder
            //   - key is valid
            //      - value is equal to field name => generate one-at-a-time method
            //      - not equal => generate one-at-a-time method and all-at-once method
            //   - key isn't valid
            //      => compile error
            // 3. not exists
            //   => generate all-at-once method
            let meta_list = f
                .attrs
                .iter()
                .filter(|attr| attr.path.is_ident("builder"))
                .map(|attr| match attr.parse_meta().unwrap() {
                    Meta::List(meta_list) => meta_list,
                    _ => unimplemented!(),
                })
                // currently check first attribute only
                .next();

            match meta_list {
                // currently check first meta only
                Some(list) => match &list.nested[0] {
                    NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) => {
                        if !path.is_ident("each") {
                            return Error::new(list.span(), "expected `builder(each = \"...\")`")
                                .into_compile_error();
                        }
                        let Lit::Str(lit) = lit else { unimplemented!() };
                        let repeat = Ident::new(&lit.value(), lit.span());
                        let inner_ty = generic_inner_type(ty);
                        let output = quote! {
                            fn #repeat(&mut self, #repeat: #inner_ty) -> &mut Self {
                                self.#ident.get_or_insert_with(std::vec::Vec::new).push(#repeat);
                                self
                            }
                        };
                        if *ident != lit.value() {
                            let setter = setter(ident, ty);
                            quote! {
                                #output
                                #setter
                            }
                        } else {
                            output
                        }
                    }
                    _ => unimplemented!(),
                },
                None => match name_of_type(ty).as_str() {
                    "Option" => setter(ident, generic_inner_type(ty)),
                    _ => setter(ident, ty),
                },
            }
        });

        let build = {
            let (opt, non_opt) = fields
                .iter()
                .partition::<Vec<_>, _>(|f| name_of_type(&f.ty).as_str() == "Option");
            let (vec, remain) = non_opt.into_iter().partition::<Vec<_>, _>(|f| {
                f.attrs.iter().any(|attr| attr.path.is_ident("builder"))
            });
            let opt = opt.into_iter().map(|f| &f.ident);
            let remains = remain.into_iter().map(|f| &f.ident);
            let repeated = vec.into_iter().map(|f| &f.ident);
            quote! {
                pub fn build(&mut self) -> std::result::Result<#origin_ident, std::boxed::Box<dyn std::error::Error>> {
                    std::result::Result::Ok(#origin_ident {
                        // optional fields
                        #(#opt: self.#opt.take(),)*

                        // repeated fields
                        #(#repeated: self.#repeated.take().unwrap_or_else(std::vec::Vec::new),)*

                        // remaining fields
                        #(#remains: self.#remains.take().ok_or_else(|| concat!("field `", stringify!(#remains), "` is missing."))?,)*
                    })
                }
            }
        };
        quote! {
            impl #builder_ident {
                #(#setters)*

                #build
            }
        }
    };

    quote! {
        #constructor

        #definition

        #implmentation
    }
    .into()
}

fn get_path(ty: &Type) -> Option<&Path> {
    match ty {
        Type::Path(TypePath { path, .. }) => Some(path),
        _ => None,
    }
}

fn generic_inner_type(ty: &Type) -> &Punctuated<syn::GenericArgument, syn::token::Comma> {
    match &get_path(ty).unwrap().segments[0].arguments {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => args,
        _ => unimplemented!(),
    }
}

fn name_of_type(ty: &Type) -> String {
    let Some(path) = get_path(ty) else { unimplemented!() };
    path.segments[0].ident.to_string()
}
