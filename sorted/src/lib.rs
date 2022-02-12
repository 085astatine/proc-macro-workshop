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
        .map(|arm| MatchArmPat::new(arm))
        .collect::<syn::Result<Vec<MatchArmPat>>>()
    {
        Ok(paths) => paths,
        Err(error) => {
            return Some(error);
        }
    };
    // check if path is sorted
    if let Some(unsorted) = arm_paths
        .windows(2)
        .find(|pair| pair[0] > pair[1])
        .map(|pair| &pair[1])
    {
        let before = arm_paths.iter().find(|path| path > &unsorted).unwrap();
        return Some(syn::Error::new(
            *unsorted.span(),
            &format!(
                "{} should sort before {}",
                unsorted.to_string(),
                before.to_string(),
            ),
        ));
    }
    None
}

#[derive(Debug)]
struct MatchArmPatIdents {
    idents: Vec<String>,
    span: proc_macro2::Span,
}

impl PartialEq for MatchArmPatIdents {
    fn eq(&self, other: &Self) -> bool {
        self.idents.eq(&other.idents)
    }
}

impl PartialOrd for MatchArmPatIdents {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.idents.partial_cmp(&other.idents)
    }
}

impl ToString for MatchArmPatIdents {
    fn to_string(&self) -> String {
        self.idents.join("::")
    }
}

#[derive(Debug)]
struct MatchArmPatWild {
    span: proc_macro2::Span,
}

impl PartialEq for MatchArmPatWild {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl PartialOrd for MatchArmPatWild {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}

impl ToString for MatchArmPatWild {
    fn to_string(&self) -> String {
        "_".to_string()
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum MatchArmPat {
    Idents(MatchArmPatIdents),
    Wild(MatchArmPatWild),
}

impl MatchArmPat {
    fn new(arm: &syn::Arm) -> syn::Result<Self> {
        match arm.pat {
            syn::Pat::Ident(ref pat) => Ok(MatchArmPat::from(&pat.ident)),
            syn::Pat::Path(ref pat) => Ok(MatchArmPat::from(&pat.path)),
            syn::Pat::Struct(ref pat) => Ok(MatchArmPat::from(&pat.path)),
            syn::Pat::TupleStruct(ref pat) => Ok(MatchArmPat::from(&pat.path)),
            syn::Pat::Wild(ref pat) => Ok(MatchArmPat::from(pat)),
            _ => Err(syn::Error::new_spanned(
                arm.pat.clone(),
                "unsupported by #[sorted]",
            )),
        }
    }

    fn span(&self) -> &proc_macro2::Span {
        match self {
            Self::Idents(idents) => &idents.span,
            Self::Wild(wild) => &wild.span,
        }
    }
}

impl From<&syn::Ident> for MatchArmPat {
    fn from(item: &syn::Ident) -> Self {
        Self::Idents(MatchArmPatIdents {
            idents: vec![item.to_string()],
            span: item.span(),
        })
    }
}

impl From<&syn::Path> for MatchArmPat {
    fn from(item: &syn::Path) -> Self {
        use syn::spanned::Spanned;
        Self::Idents(MatchArmPatIdents {
            idents: item
                .segments
                .iter()
                .map(|segment| segment.ident.to_string())
                .collect(),
            span: item.span(),
        })
    }
}

impl From<&syn::PatWild> for MatchArmPat {
    fn from(item: &syn::PatWild) -> Self {
        use syn::spanned::Spanned;
        Self::Wild(MatchArmPatWild { span: item.span() })
    }
}

impl ToString for MatchArmPat {
    fn to_string(&self) -> String {
        match self {
            Self::Idents(idents) => idents.to_string(),
            Self::Wild(wild) => wild.to_string(),
        }
    }
}
