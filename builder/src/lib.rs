use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, AngleBracketedGenericArguments, DeriveInput, Ident, Type, TypePath};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    let ident = input.ident;
    let builder_ident = Ident::new(&format!("{}Builder", ident), ident.span());
    let syn::Data::Struct(s) = input.data else { unimplemented!() };
    let (optional_fields, non_optional_fields) =
        s.fields.iter().partition::<Vec<_>, _>(|f| match &f.ty {
            Type::Path(TypePath { path, .. }) => {
                let ident = &path.segments.first().unwrap().ident;
                *ident == Ident::new("Option", Span::call_site().into())
            }
            _ => false,
        });
    let _non_opt_idents = non_optional_fields.iter().map(|f| &f.ident);
    let _non_opt_types = non_optional_fields.iter().map(|f| &f.ty);
    let _opt_idents = optional_fields.iter().map(|f| &f.ident);
    let _opt_inner_types = optional_fields.iter().map(|f| {
        let path = match &f.ty {
            Type::Path(TypePath { path, .. }) => path,
            _ => unreachable!(),
        };
        let args = &path.segments.first().unwrap().arguments;
        match args {
            syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                args.first().unwrap()
            }
            syn::PathArguments::None => todo!(),
            syn::PathArguments::Parenthesized(_) => todo!(),
        }
    });

    let opt_idents = _opt_idents.clone();
    let opt_inner_types = _opt_inner_types.clone();
    let idents = _non_opt_idents.clone();
    let types = _non_opt_types.clone();

    let builder_definition = quote! {
        #[derive(Clone)]
        pub struct #builder_ident {
            #(#opt_idents: Option<#opt_inner_types>,)*
            #(#idents: Option<#types>,)*
        }
    };

    let opt_idents = _opt_idents.clone();
    let opt_inner_types = _opt_inner_types.clone();
    let idents = _non_opt_idents.clone();
    let types = _non_opt_types.clone();

    let builder_setter = quote! {
        impl #builder_ident {
            #(
            fn #idents(&mut self, #idents: #types) -> &mut Self {
                self.#idents = Some(#idents);
                self
            }
            )*
            #(
            fn #opt_idents(&mut self, #opt_idents: #opt_inner_types) -> &mut Self {
                self.#opt_idents = Some(#opt_idents);
                self
            }
            )*
        }
    };

    let builder_build = quote! {
        impl #builder_ident {
            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                let builder = self.clone();
                Ok(#ident {
                    #(#_non_opt_idents : builder.#_non_opt_idents.ok_or_else(|| concat!("field `", stringify!(#_non_opt_idents), "` is missing."))?,)*
                    #(#_opt_idents: builder.#_opt_idents,)*
                })
            }
        }
    };

    let idents = s.fields.iter().map(|f| &f.ident);
    let output = quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#idents: None,)*
                }
            }
        }

        #builder_definition

        #builder_setter

        #builder_build
    };

    output.into()
}
