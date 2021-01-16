use proc_macro::TokenStream as StdTokenStream;
use proc_macro2::Span;
use proc_macro2::TokenTree;
use quote::quote;
use quote::quote_spanned;

#[derive(Debug)]
pub(crate) enum Error {
    UnexpectedTokens(Vec<TokenTree>, &'static str),
    Message(&'static str),
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

impl Into<StdTokenStream> for Error {
    fn into(self) -> StdTokenStream {
        match self {
            Error::UnexpectedTokens(tokens, msg) => {
                let span = tokens[0].span();
                error_at(msg, span)
            }
            Error::Message(msg) => {
                let q = quote! {
                    compile_error!(#msg)
                };
                q.into()
            }
        }
    }
}

fn error_at(msg: &str, span: Span) -> StdTokenStream {
    // Use proc_macro::Diagnostic once stabilized
    let span = span.into();
    let q = quote_spanned! {
        span => { compile_error!(#msg); }
    };
    let q = quote! {
        const _ERROR: () = { #q; };
    };
    q.into()
}
