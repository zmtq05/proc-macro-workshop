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
    let field_idents2 = field_idents.clone();
    let field_types = s.fields.iter().map(|f| &f.ty);

    let output = quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_idents: None,)*
                }
            }
        }

        pub struct #builder_ident {
            #(#field_idents2: Option<#field_types>,)*
        }
    };

    output.into()
}
