use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DeriveInput, Field, Fields, Ident, Path,
    PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder)]
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

    let (opt_fields, req_fields) = split_optional_fields(&struct_data.fields);

    let req_idents_ = req_fields.iter().map(|f| &f.ident);
    let req_types_ = req_fields.iter().map(|f| &f.ty);
    let opt_idents_ = opt_fields.iter().map(|f| &f.ident);
    let opt_types_ = opt_fields.iter().map(|f| &f.ty);

    let req_idents = req_idents_.clone();
    let req_types = req_types_.clone();
    let opt_idents = opt_idents_.clone();
    let opt_types = opt_types_.clone();

    let builder_definition = quote! {
        #[derive(Clone, Default)]
        pub struct #builder_ident {
            #(#opt_idents: #opt_types,)*
            #(#req_idents: Option<#req_types>,)*
        }
    };

    let opt_idents = opt_idents_.clone();
    let opt_inner_types =
        opt_types_
            .filter_map(get_path)
            .map(|path| match &path.segments[0].arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => args,
                _ => unreachable!(),
            });
    let req_idents = req_idents_.clone();
    let req_types = req_types_.clone();

    let setters = quote! {
        #(
        fn #req_idents(&mut self, #req_idents: #req_types) -> &mut Self {
            self.#req_idents = Some(#req_idents);
            self
        }
        )*

        #(
        fn #opt_idents(&mut self, #opt_idents: #opt_inner_types) -> &mut Self {
            self.#opt_idents = Some(#opt_idents);
            self
        }
        )*
    };

    let req_idents = req_idents_.clone();
    let opt_idents = opt_idents_.clone();
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
            #setters

            #build_fn
        }
    };

    output.into()
}

fn split_optional_fields(fields: &Fields) -> (Vec<&Field>, Vec<&Field>) {
    fields.into_iter().partition(|field| {
        let Some(path) = get_path(&field.ty) else { return false };
        path.segments[0].ident.to_string().as_str() == "Option"
    })
}

fn get_path(ty: &Type) -> Option<&Path> {
    match ty {
        Type::Path(TypePath { path, .. }) => Some(path),
        _ => None,
    }
}
