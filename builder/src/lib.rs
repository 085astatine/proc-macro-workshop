use proc_macro2;
use quote;
use syn;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match derive_impl(&input) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.into_compile_error().into(),
    }
}

fn derive_impl(input: &syn::DeriveInput) -> Result<proc_macro2::TokenStream, syn::Error> {
    let struct_builder = struct_builder(&input);
    let impl_struct = impl_struct(&input);
    let impl_builder = impl_builder(&input)?;

    Ok(quote::quote! {
        #struct_builder
        #impl_struct
        #impl_builder
    })
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

fn vector_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let Some(last_segment) = last_path_segment(ty) {
        if last_segment.ident == "Vec" {
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
    if is_option_type(ty) || is_repeated_field(field) {
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
    if is_repeated_field(field) {
        quote::quote! {
            #name: Vec::new()
        }
    } else {
        quote::quote! {
            #name: None
        }
    }
}

fn impl_builder(input: &syn::DeriveInput) -> Result<proc_macro2::TokenStream, syn::Error> {
    let builder_name = builder_name(input);
    let struct_fields = struct_fields(&input.data);
    let mut setters = Vec::new();
    for field in &struct_fields {
        setters.push(impl_builder_setter(field)?);
    }
    let build = impl_builder_build(input);
    Ok(quote::quote! {
        impl #builder_name {
            #(#setters)*
            #build
        }
    })
}

fn impl_builder_setter(field: &syn::Field) -> Result<proc_macro2::TokenStream, syn::Error> {
    let name = field.ident.as_ref().unwrap();
    match repeat_setter_name(field)? {
        Some(ref setter_name) => builder_repeat_setter(&name, setter_name, &field.ty),
        None => Ok(builder_simple_setter(&name, &field.ty)),
    }
}

fn builder_simple_setter(name: &syn::Ident, ty: &syn::Type) -> proc_macro2::TokenStream {
    let arg_ty = if is_option_type(ty) {
        option_inner_type(&ty).unwrap()
    } else {
        ty
    };
    quote::quote! {
        pub fn #name(&mut self, #name: #arg_ty) -> &mut Self {
            self.#name = Some(#name);
            self
        }
    }
}

fn builder_attribute(field: &syn::Field) -> Result<Option<syn::MetaList>, syn::Error> {
    let mut value = None;
    for attr in &field.attrs {
        if let Ok(meta) = attr.parse_meta() {
            if let Some(ident) = meta.path().get_ident() {
                if ident == "builder" {
                    if value.is_some() {
                        return Err(syn::Error::new_spanned(
                            attr,
                            "duplicated attribute \"builder\"",
                        ));
                    }
                    if let syn::Meta::List(meta_list) = meta {
                        value = Some(meta_list);
                    }
                }
            }
        }
    }
    Ok(value)
}

fn is_repeated_field(field: &syn::Field) -> bool {
    builder_attribute(field).ok().unwrap().is_some()
}

fn repeat_setter_name(field: &syn::Field) -> Result<Option<syn::Ident>, syn::Error> {
    use syn::spanned::Spanned;

    let error_message = "expected `builder(each = \"...\")`";
    if let Some(meta) = builder_attribute(field)? {
        let meta_span = meta.span();
        let mut value = None;
        for nestead in &meta.nested {
            if let syn::NestedMeta::Meta(nestead_meta) = nestead {
                if let syn::Meta::NameValue(name_value) = nestead_meta {
                    if let Some(ident) = name_value.path.get_ident() {
                        if ident == "each" {
                            if let syn::Lit::Str(name) = &name_value.lit {
                                if value.is_some() {
                                    return Err(syn::Error::new(meta_span, error_message));
                                }
                                value = Some(syn::Ident::new(&name.value(), name.span()));
                            }
                        } else {
                            return Err(syn::Error::new(meta_span, error_message));
                        }
                    }
                }
            }
        }
        if value.is_none() {
            return Err(syn::Error::new(meta_span, error_message));
        }
        return Ok(value);
    }
    Ok(None)
}

fn builder_repeat_setter(
    name: &syn::Ident,
    setter: &syn::Ident,
    ty: &syn::Type,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let inner_type = vector_inner_type(ty).unwrap();
    Ok(quote::quote! {
        pub fn #setter(& mut self, #setter: #inner_type) -> &mut Self {
            self.#name.push(#setter);
            self
        }
    })
}

fn impl_builder_build(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let return_type = &input.ident;
    let struct_fields = struct_fields(&input.data);
    let args = struct_fields.iter().map(|field| {
        let name = &field.ident;
        if is_option_type(&field.ty) || is_repeated_field(field) {
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
