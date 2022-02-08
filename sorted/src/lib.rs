#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // compile error: if args is not empty
    if !args.is_empty() {
        return syn::Error::new_spanned(proc_macro2::TokenStream::from(args), "unknown args")
            .into_compile_error()
            .into();
    }
    // parse input
    let sorted_enum = syn::parse_macro_input!(input as SortedEnum);
    sorted_enum.expand().into()
}

#[derive(Debug)]
struct SortedEnum {
    item: syn::Item,
}

impl syn::parse::Parse for SortedEnum {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let item = input.parse::<syn::Item>()?;
        Ok(Self { item })
    }
}

impl SortedEnum {
    fn check(&self) -> Option<syn::Error> {
        // check item is enum
        let item = match &self.item {
            syn::Item::Enum(item) => item,
            _ => {
                return Some(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    "expected enum or match expression",
                ));
            }
        };
        is_sorted_enum(item)
    }

    fn expand(&self) -> proc_macro2::TokenStream {
        use quote::ToTokens;
        let mut stream = self.item.to_token_stream();
        if let Some(error) = self.check() {
            stream.extend(error.into_compile_error());
        }
        stream
    }
}

fn is_sorted_enum(item: &syn::ItemEnum) -> Option<syn::Error> {
    let idents: Vec<&syn::Ident> = item.variants.iter().map(|variant| &variant.ident).collect();
    if let Some(unsorted) = idents
        .windows(2)
        .find(|pair| pair[0] > pair[1])
        .map(|pair| pair[1])
    {
        let before = idents.iter().find(|ident| ident > &&unsorted).unwrap();
        return Some(syn::Error::new(
            unsorted.span(),
            &format!(
                "{} should sort before {}",
                unsorted.to_string(),
                before.to_string()
            ),
        ));
    }
    None
}
