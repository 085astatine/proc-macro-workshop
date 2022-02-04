#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);

    match seq2(seq) {
        Ok(token_stream) => token_stream.into(),
        Err(error) => error.into_compile_error().into(),
    }
}

fn seq2(seq: Seq) -> syn::Result<proc_macro2::TokenStream> {
    let mut streams = Vec::new();
    for i in seq.range::<i64>()? {
        streams.push(expand_token_stream(&seq.token_stream(), &seq.target, &i));
    }
    Ok(quote::quote! {
        #(#streams)*
    })
}

#[derive(Debug)]
struct Seq {
    target: syn::Ident,
    _in_token: syn::Token![in],
    begin: syn::LitInt,
    _dot2_token: syn::Token![..],
    end: syn::LitInt,
    token_tree: proc_macro2::TokenTree,
}

impl Seq {
    fn range<N>(&self) -> syn::Result<std::ops::Range<N>>
    where
        N: std::str::FromStr,
        N::Err: std::fmt::Display,
    {
        let begin = self.begin.base10_parse::<N>()?;
        let end = self.end.base10_parse::<N>()?;
        Ok(begin..end)
    }

    fn token_stream(&self) -> proc_macro2::TokenStream {
        if let proc_macro2::TokenTree::Group(group) = &self.token_tree {
            group.stream()
        } else {
            proc_macro2::TokenStream::new()
        }
    }
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // N in X..Y
        let target = input.parse::<syn::Ident>()?;
        let _in_token = input.parse::<syn::Token![in]>()?;
        let begin = input.parse::<syn::LitInt>()?;
        let _dot2_token = input.parse::<syn::Token![..]>()?;
        let end = input.parse::<syn::LitInt>()?;
        // {...}
        let token_tree = input.parse::<proc_macro2::TokenTree>()?;
        Ok(Seq {
            target,
            _in_token,
            begin,
            _dot2_token,
            end,
            token_tree,
        })
    }
}

fn expand_token_tree<T: ToLiteral + std::fmt::Display>(
    tree: &proc_macro2::TokenTree,
    target: &syn::Ident,
    index: &T,
) -> proc_macro2::TokenTree {
    match tree {
        proc_macro2::TokenTree::Ident(ref ident) if ident == target => {
            proc_macro2::TokenTree::Literal(index.to_literal(ident))
        }
        proc_macro2::TokenTree::Group(ref group) => {
            let mut expanded = proc_macro2::Group::new(
                group.delimiter(),
                expand_token_stream(&group.stream(), target, index),
            );
            expanded.set_span(group.span());
            proc_macro2::TokenTree::Group(expanded)
        }
        _ => tree.clone(),
    }
}

fn expand_token_stream<T>(
    input: &proc_macro2::TokenStream,
    target: &syn::Ident,
    index: &T,
) -> proc_macro2::TokenStream
where
    T: ToLiteral + std::fmt::Display,
{
    let mut expanded = Vec::new();
    let mut iter = input.clone().into_iter().peekable();
    while let Some(tree) = iter.next() {
        match tree {
            proc_macro2::TokenTree::Punct(ref tilde) if tilde.as_char() == '~' => {
                match (expanded.last(), iter.peek()) {
                    (
                        Some(proc_macro2::TokenTree::Ident(prev)),
                        Some(proc_macro2::TokenTree::Ident(next)),
                    ) if next == target => {
                        // paste: ident ~ target
                        let pasted =
                            proc_macro2::Ident::new(&format!("{}{}", prev, index), prev.span());
                        // move cursor
                        expanded.pop();
                        iter.next();
                        // push
                        expanded.push(proc_macro2::TokenTree::Ident(pasted))
                    }
                    _ => expanded.push(expand_token_tree(&tree, target, index)),
                }
            }
            _ => expanded.push(expand_token_tree(&tree, target, index)),
        }
    }
    let mut stream = proc_macro2::TokenStream::new();
    stream.extend(expanded);
    stream
}

trait ToLiteral {
    fn to_literal(&self, ident: &proc_macro2::Ident) -> proc_macro2::Literal;
}

macro_rules! impl_to_literal_for {
    ($ty:ty) => {
        impl ToLiteral for $ty {
            fn to_literal(&self, ident: &proc_macro2::Ident) -> proc_macro2::Literal {
                paste::paste! {
                    let mut literal = proc_macro2::Literal::[< $ty _unsuffixed>] (*self);
                }
                literal.set_span(ident.span());
                literal
            }
        }
    };
}

impl_to_literal_for!(i8);
impl_to_literal_for!(i16);
impl_to_literal_for!(i32);
impl_to_literal_for!(i64);
impl_to_literal_for!(u8);
impl_to_literal_for!(u16);
impl_to_literal_for!(u32);
impl_to_literal_for!(u64);
impl_to_literal_for!(isize);
impl_to_literal_for!(usize);
