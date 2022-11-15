use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, AngleBracketedGenericArguments, DeriveInput, Ident,
    Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    let ident = input.ident;
    let builder_ident = Ident::new(&format!("{}Builder", ident), ident.span());

    let syn::Data::Struct(struct_data) = input.data else { unimplemented!() };

    let builder_constructor_impl = quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident::default()
            }
        }
    };

    let builder_fields = struct_data.fields.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;
        match name_of_type(ty).as_str() {
            "Option" | "Vec" => quote! { #ident: #ty },
            _ => quote! { #ident: Option<#ty> },
        }
    });

    let builder_definition = quote! {
        #[derive(Clone, Default)]
        pub struct #builder_ident {
            #(#builder_fields,)*
        }
    };

    let setters = struct_data.fields.iter().map(|f| {
        let ident = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        match name_of_type(ty).as_str() {
            "Option" => {
                let ty = generic_inner_type(ty).unwrap();
                quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                }
            }
            "Vec" => {
                let Some(attr) = f.attrs.first() else {
                    return quote! {
                        fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }
                    }
                };
                let meta = attr.parse_meta().unwrap();
                let Meta::List(MetaList { nested, .. }) = meta else { unimplemented!("meta") };
                match nested.first().unwrap() {
                    NestedMeta::Meta(Meta::NameValue(MetaNameValue { lit, .. })) => {
                        let Lit::Str(litstr) = lit else { unimplemented!() };
                        let lit = &litstr.value();
                        if lit == &ident.to_string() {
                            return quote! {};
                        }
                        let lit = Ident::new(lit, litstr.span());
                        let ty = generic_inner_type(ty);
                        quote! {
                            fn #lit(&mut self, a: #ty) -> &mut Self {
                                self.#ident.push(a);
                                self
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            _ => quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            },
        }
    });

    let req_idents = struct_data
        .fields
        .iter()
        .filter(|f| !matches!(name_of_type(&f.ty).as_str(), "Option" | "Vec"))
        .map(|f| &f.ident);
    let opt_idents = struct_data
        .fields
        .iter()
        .filter(|f| matches!(name_of_type(&f.ty).as_str(), "Option" | "Vec"))
        .map(|f| &f.ident);
    let build_fn = quote! {
        pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
            let builder = self.clone();
            Ok(#ident {
                #(#req_idents: builder.#req_idents.ok_or_else(|| concat!("field `", stringify!(#req_idents), "` is missing."))?,)*
                #(#opt_idents: builder.#opt_idents,)*
            })
        }
    };

    let output = quote! {
        #builder_constructor_impl

        #builder_definition

        impl #builder_ident {
            #(#setters)*

            #build_fn
        }
    };

    output.into()
}

fn get_path(ty: &Type) -> Option<&Path> {
    match ty {
        Type::Path(TypePath { path, .. }) => Some(path),
        _ => None,
    }
}

fn generic_inner_type(ty: &Type) -> Option<&Punctuated<syn::GenericArgument, syn::token::Comma>> {
    match &get_path(ty)?.segments[0].arguments {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => Some(args),
        _ => unimplemented!(),
    }
}

fn name_of_type(ty: &Type) -> String {
    let Some(path) = get_path(ty) else { unimplemented!() };
    path.segments[0].ident.to_string()
}
