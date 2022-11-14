use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    let ident = input.ident;
    let builder_ident = Ident::new(&format!("{}Builder", ident), ident.span());
    let syn::Data::Struct(s) = input.data else { unimplemented!() };
    let field_idents = s.fields.iter().map(|f| &f.ident);
    let field_types = s.fields.iter().map(|f| &f.ty);

    let f_idents = field_idents.clone();
    let f_types = field_types.clone();

    let builder_definition = quote! {
        #[derive(Clone)]
        pub struct #builder_ident {
            #(#f_idents: Option<#f_types>,)*
        }
    };

    let f_idents = field_idents.clone();
    let f_types = field_types.clone();
    let builder_impl = quote! {
        impl #builder_ident {
            #(
            fn #f_idents(&mut self, #f_idents: #f_types) -> &mut Self {
                self.#f_idents = Some(#f_idents);
                self
            }
            )*
        }
    };

    let f_idents = field_idents.clone();
    let builder_build = quote! {
        impl #builder_ident {
            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                let builder = self.clone();
                Ok(#ident {
                    #(#f_idents : builder.#f_idents.ok_or_else(|| concat!("field `", stringify!(#f_idents), "` is missing."))?,)*
                })
            }
        }
    };

    let output = quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_idents: None,)*
                }
            }
        }

        #builder_definition

        #builder_impl

        #builder_build
    };

    output.into()
}
