use proc_macro2;
use quote;
use syn;

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_builder = struct_builder(&input);
    let impl_builder = impl_builder(&input);

    let expanded = quote::quote! {
        #struct_builder
        #impl_builder
    };
    proc_macro::TokenStream::from(expanded)
}

fn struct_fields(data: &syn::Data) -> Vec<&syn::Field> {
    match data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => fields.named.iter().collect(),
            syn::Fields::Unnamed(_) | syn::Fields::Unit => unimplemented!(),
        },
        syn::Data::Enum(_) | syn::Data::Union(_) => unimplemented!(),
    }
}

fn struct_builder(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let struct_name = quote::format_ident!("{}Builder", input.ident);
    let visibility = &input.vis;
    let struct_fields = struct_fields(&input.data);
    let fields = struct_fields
        .iter()
        .map(|field| struct_builder_field(field));
    quote::quote! {
        #visibility struct #struct_name {
            #(#fields),*
        }
    }
}

fn struct_builder_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let ty = &field.ty;
    quote::quote! {
        #name: Option<#ty>
    }
}

fn impl_builder(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &input.ident;
    let builder_name = quote::format_ident!("{}Builder", input.ident);
    let struct_fields = struct_fields(&input.data);
    let builder_initialize_fields = struct_fields
        .iter()
        .map(|field| impl_builder_initialize_field(field));
    quote::quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_initialize_fields),*
                }
            }
        }
    }
}

fn impl_builder_initialize_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    quote::quote! {
        #name: None
    }
}
