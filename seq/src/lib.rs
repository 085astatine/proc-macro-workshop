#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);
    println!("{:#?}", seq);

    proc_macro::TokenStream::new()
}

#[derive(Debug)]
struct Seq {
    target: syn::Ident,
    begin: u64,
    end: u64,
    block: syn::ExprBlock,
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, syn::parse::Error> {
        // N in X..Y
        let target = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![in]>()?;
        let begin = input.parse::<syn::LitInt>()?.base10_parse()?;
        input.parse::<syn::Token![..]>()?;
        let end = input.parse::<syn::LitInt>()?.base10_parse()?;
        // {...}
        let block = input.parse::<syn::ExprBlock>()?;
        Ok(Seq {
            target,
            begin,
            end,
            block,
        })
    }
}
