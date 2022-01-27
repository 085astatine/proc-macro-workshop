use proc_macro2;
use quote;
use syn;

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_builder = struct_builder(&input);
    let impl_struct = impl_struct(&input);
    let impl_builder = impl_builder(&input);

    let expanded = quote::quote! {
        #struct_builder
        #impl_struct
        #impl_builder
    };
    proc_macro::TokenStream::from(expanded)
}

fn builder_name(data: &syn::DeriveInput) -> syn::Ident {
    quote::format_ident!("{}Builder", data.ident)
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

fn last_path_segment(ty: &syn::Type) -> Option<&syn::PathSegment> {
    if let syn::Type::Path(ref ty) = ty {
        ty.path.segments.last()
    } else {
        None
    }
}

fn is_option_type(ty: &syn::Type) -> bool {
    if let Some(last_segment) = last_path_segment(ty) {
        last_segment.ident == "Option"
    } else {
        false
    }
}

fn option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let Some(last_segment) = last_path_segment(ty) {
        if last_segment.ident == "Option" {
            if let syn::PathArguments::AngleBracketed(arguments) = &last_segment.arguments {
                for arg in &arguments.args {
                    if let syn::GenericArgument::Type(generic_type) = arg {
                        return Some(&generic_type);
                    }
                }
            }
        }
    }
    None
}

fn struct_builder(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let struct_name = builder_name(input);
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
    if is_option_type(ty) {
        quote::quote! {
            #name: #ty
        }
    } else {
        quote::quote! {
            #name: Option<#ty>
        }
    }
}

fn impl_struct(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &input.ident;
    let builder_name = builder_name(input);
    let struct_fields = struct_fields(&input.data);
    let builder_initialize_fields = struct_fields
        .iter()
        .map(|field| impl_struct_initialize_builder_field(field));
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

fn impl_struct_initialize_builder_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    quote::quote! {
        #name: None
    }
}

fn impl_builder(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let builder_name = builder_name(input);
    let struct_fields = struct_fields(&input.data);
    let setters = struct_fields.iter().map(|field| impl_builder_setter(field));
    let build = impl_builder_build(input);
    quote::quote! {
        impl #builder_name {
            #(#setters)*
            #build
        }
    }
}

fn impl_builder_setter(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let ty = if is_option_type(&field.ty) {
        option_inner_type(&field.ty).unwrap()
    } else {
        &field.ty
    };
    quote::quote! {
        pub fn #name(&mut self, #name: #ty) -> &mut Self {
            self.#name = Some(#name);
            self
        }
    }
}

fn impl_builder_build(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let return_type = &input.ident;
    let struct_fields = struct_fields(&input.data);
    let args = struct_fields.iter().map(|field| {
        let name = &field.ident;
        if is_option_type(&field.ty) {
            quote::quote! {
                #name: self.#name.clone()
            }
        } else {
            quote::quote! {
                #name: self.#name.clone().unwrap()
            }
        }
    });
    quote::quote! {
        pub fn build(&mut self) -> Result<#return_type, Box<dyn std::error::Error>> {
            Ok(#return_type {
                #(#args),*
            })
        }
    }
}
