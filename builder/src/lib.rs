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
    let builder = quote::format_ident!("{}Builder", input.ident);
    let fields = builder_fields(&input.data)?;

    let struct_builder = generate_struct_builder(&builder, &fields, &input.vis);
    let impl_struct = generate_builder(&input.ident, &builder, &fields);
    let impl_builder = generate_impl_builder(&input.ident, &builder, &fields);

    Ok(quote::quote! {
        #struct_builder
        #impl_struct
        #impl_builder
    })
}

struct ValueField<'a> {
    field: &'a syn::Field,
}

struct OptionField<'a> {
    field: &'a syn::Field,
    inner_type: &'a syn::Type,
}

struct RepeatedField<'a> {
    field: &'a syn::Field,
    setter: syn::Ident,
    inner_type: &'a syn::Type,
}

enum BuilderField<'a> {
    Value(ValueField<'a>),
    Opt(OptionField<'a>),
    Repeated(RepeatedField<'a>),
}

impl<'a> BuilderField<'a> {
    fn new(field: &syn::Field) -> Result<BuilderField, syn::Error> {
        if let Some(setter) = repeated_field_setter(field)? {
            if let Some(inner_type) = vector_inner_type(&field.ty) {
                return Ok(BuilderField::Repeated(RepeatedField {
                    field,
                    setter,
                    inner_type,
                }));
            }
        }
        if is_option_type(&field.ty) {
            if let Some(inner_type) = option_inner_type(&field.ty) {
                return Ok(BuilderField::Opt(OptionField { field, inner_type }));
            }
        }
        Ok(BuilderField::Value(ValueField { field }))
    }

    fn generate_struct_field(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Value(value) => {
                let name = &value.field.ident;
                let ty = &value.field.ty;
                quote::quote! {
                    #name: Option<#ty>
                }
            }
            Self::Opt(opt) => {
                let name = &opt.field.ident;
                let ty = &opt.inner_type;
                quote::quote! {
                    #name: Option<#ty>
                }
            }
            Self::Repeated(repeated) => {
                let name = &repeated.field.ident;
                let ty = &repeated.inner_type;
                quote::quote! {
                    #name: Vec<#ty>
                }
            }
        }
    }

    fn generate_initialize_field(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Value(value) => {
                let name = &value.field.ident;
                quote::quote! {
                    #name: None
                }
            }
            Self::Opt(opt) => {
                let name = &opt.field.ident;
                quote::quote! {
                    #name: None
                }
            }
            Self::Repeated(repeated) => {
                let name = &repeated.field.ident;
                quote::quote! {
                    #name: Vec::new()
                }
            }
        }
    }

    fn generate_setter(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Value(value) => {
                let name = &value.field.ident;
                let ty = &value.field.ty;
                quote::quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
            Self::Opt(opt) => {
                let name = &opt.field.ident;
                let ty = &opt.inner_type;
                quote::quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
            Self::Repeated(repeated) => {
                let name = &repeated.field.ident;
                let setter = &repeated.setter;
                let ty = &repeated.inner_type;
                quote::quote! {
                    pub fn #setter(&mut self, #setter: #ty) -> &mut Self {
                        self.#name.push(#setter);
                        self
                    }
                }
            }
        }
    }

    fn generate_build_arg(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Value(value) => {
                let name = &value.field.ident;
                quote::quote! {
                    #name: self.#name.clone().unwrap()
                }
            }
            Self::Opt(opt) => {
                let name = &opt.field.ident;
                quote::quote! {
                    #name: self.#name.clone()
                }
            }
            Self::Repeated(repeated) => {
                let name = &repeated.field.ident;
                quote::quote! {
                    #name: self.#name.clone()
                }
            }
        }
    }
}

fn builder_fields<'a>(data: &'a syn::Data) -> Result<Vec<BuilderField<'a>>, syn::Error> {
    match data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|field| BuilderField::new(field))
                .collect(),
            syn::Fields::Unnamed(_) | syn::Fields::Unit => unimplemented!(),
        },
        syn::Data::Enum(_) | syn::Data::Union(_) => unimplemented!(),
    }
}

fn generate_struct_builder<'a>(
    builder: &'a syn::Ident,
    fields: &'a Vec<BuilderField<'a>>,
    visibility: &'a syn::Visibility,
) -> proc_macro2::TokenStream {
    let fields = fields.iter().map(|field| field.generate_struct_field());
    quote::quote! {
        #visibility struct #builder{
            #(#fields),*
        }
    }
}

fn generate_impl_builder<'a>(
    name: &'a syn::Ident,
    builder: &'a syn::Ident,
    fields: &'a Vec<BuilderField<'a>>,
) -> proc_macro2::TokenStream {
    let setters = fields.iter().map(|field| field.generate_setter());
    let build = {
        let args = fields.iter().map(|field| field.generate_build_arg());
        quote::quote! {
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#args),*
                })
            }
        }
    };
    quote::quote! {
        impl #builder {
            #(#setters)*
            #build
        }
    }
}

fn generate_builder<'a>(
    name: &'a syn::Ident,
    builder: &'a syn::Ident,
    fields: &'a Vec<BuilderField<'a>>,
) -> proc_macro2::TokenStream {
    let args = fields.iter().map(|field| field.generate_initialize_field());
    quote::quote! {
        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#args),*
                }
            }
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

fn repeated_field_setter(field: &syn::Field) -> Result<Option<syn::Ident>, syn::Error> {
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
