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

#[proc_macro_attribute]
pub fn check(
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
    let mut sorted_match = syn::parse_macro_input!(input as SortedMatch);
    sorted_match.expand().into()
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

#[derive(Debug)]
struct SortedMatch {
    item: syn::ItemFn,
    errors: Vec<syn::Error>,
}

impl syn::parse::Parse for SortedMatch {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let item = input.parse::<syn::Item>()?;
        if let syn::Item::Fn(item) = item {
            Ok(Self {
                item,
                errors: Vec::new(),
            })
        } else {
            Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "expected function expression",
            ))
        }
    }
}

impl syn::visit_mut::VisitMut for SortedMatch {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        // attributes
        let sorted_attribute_indexes: Vec<usize> = node
            .attrs
            .iter()
            .enumerate()
            .filter_map(|(i, attr)| {
                if is_sorted_attribute(attr) {
                    Some(i)
                } else {
                    None
                }
            })
            .collect();
        let is_target = !sorted_attribute_indexes.is_empty();
        // remove attribute #[sorted]
        for index in sorted_attribute_indexes.into_iter().rev() {
            node.attrs.remove(index);
        }
        // check match
        if is_target {
            if let Some(error) = is_sorted_match(node) {
                self.errors.push(error);
            }
        }
        // nest
        syn::visit_mut::visit_expr_match_mut(self, node);
    }
}

impl SortedMatch {
    fn check(&mut self) {
        // visit
        use syn::visit_mut::VisitMut;
        let mut item = self.item.clone();
        self.visit_item_fn_mut(&mut item);
        self.item = item;
    }

    fn error(&self) -> Option<syn::Error> {
        if let Some(first) = self.errors.first() {
            let mut merged = first.clone();
            for error in &self.errors[1..] {
                merged.combine(error.clone());
            }
            return Some(merged);
        }
        None
    }

    fn expand(&mut self) -> proc_macro2::TokenStream {
        use quote::ToTokens;
        self.check();
        let mut stream = self.item.to_token_stream();
        if let Some(error) = self.error() {
            stream.extend(error.into_compile_error());
        }
        stream
    }
}

fn is_sorted_attribute(attr: &syn::Attribute) -> bool {
    if let Some(ident) = attr.path.get_ident() {
        return ident == "sorted";
    }
    false
}

fn is_sorted_match(expr: &syn::ExprMatch) -> Option<syn::Error> {
    // extract path
    let arm_paths = match expr
        .arms
        .iter()
        .map(|arm| match_arm_path(arm))
        .collect::<syn::Result<Vec<&syn::Path>>>()
    {
        Ok(paths) => paths,
        Err(error) => {
            return Some(error);
        }
    };
    // check if path is sorted
    if let Some(unsorted) = arm_paths
        .windows(2)
        .find(|pair| compare_match_path(pair[0], pair[1]) == std::cmp::Ordering::Greater)
        .map(|pair| pair[1])
    {
        use syn::spanned::Spanned;
        let before = arm_paths
            .iter()
            .find(|path| compare_match_path(path, unsorted) == std::cmp::Ordering::Greater)
            .unwrap();
        return Some(syn::Error::new(
            unsorted.span(),
            &format!(
                "{} should sort before {}",
                unsorted
                    .segments
                    .iter()
                    .map(|path| path.ident.to_string())
                    .collect::<Vec<_>>()
                    .join("::"),
                before
                    .segments
                    .iter()
                    .map(|path| path.ident.to_string())
                    .collect::<Vec<_>>()
                    .join("::"),
            ),
        ));
    }
    None
}

fn match_arm_path(arm: &syn::Arm) -> syn::Result<&syn::Path> {
    match arm.pat {
        syn::Pat::Path(ref pat) => Ok(&pat.path),
        syn::Pat::Struct(ref pat) => Ok(&pat.path),
        syn::Pat::TupleStruct(ref pat) => Ok(&pat.path),
        _ => Err(syn::Error::new_spanned(
            arm.pat.clone(),
            "unsupported by #[sorted]",
        )),
    }
}

fn compare_match_path(lhs: &syn::Path, rhs: &syn::Path) -> std::cmp::Ordering {
    // lhs: '_'
    if is_underscore_path(lhs) {
        return if is_underscore_path(rhs) {
            std::cmp::Ordering::Equal
        } else {
            std::cmp::Ordering::Less
        };
    }
    // rhs: "_"
    if is_underscore_path(rhs) {
        return std::cmp::Ordering::Greater;
    }
    // segments
    let mut segments = lhs.segments.iter().zip(rhs.segments.iter());
    while let Some((lhs, rhs)) = segments.next() {
        if let Some(order) = lhs.ident.to_string().partial_cmp(&rhs.ident.to_string()) {
            if order != std::cmp::Ordering::Equal {
                return order;
            }
        }
    }
    lhs.segments.len().cmp(&rhs.segments.len())
}

fn is_underscore_path(path: &syn::Path) -> bool {
    if let Some(ident) = path.get_ident() {
        return ident == "_";
    }
    false
}
