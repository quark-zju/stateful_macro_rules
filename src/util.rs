use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Literal;
#[cfg(any(test, feature = "debug"))]
use proc_macro2::Spacing;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;

pub(crate) fn split_tokens<'a>(stream: TokenStream, sep: char) -> TokenSplit {
    let tokens: Vec<_> = stream.into_iter().collect();
    TokenSplit {
        tokens,
        start: 0,
        cur: 0,
        sep,
    }
}

// Similar to Vec::<TokenTree>::split, but owns the Vec.
pub(crate) struct TokenSplit {
    tokens: Vec<TokenTree>,
    start: usize,
    cur: usize,
    sep: char,
}

impl Iterator for TokenSplit {
    type Item = Vec<TokenTree>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.tokens.get(self.cur) {
                None => {
                    let start = self.start;
                    if start < self.tokens.len() {
                        self.start = self.cur + 1;
                        return Some(self.tokens[start..].to_vec());
                    }
                    return None;
                }
                Some(TokenTree::Punct(p)) if p.as_char() == self.sep => {
                    // start .. cur - 1;
                    let start = self.start;
                    let cur = self.cur;
                    self.cur += 1;
                    self.start = cur + 1;
                    if cur > start {
                        return Some(self.tokens[start..cur].to_vec());
                    }
                }
                _ => {
                    self.cur += 1;
                }
            }
        }
    }
}

pub(crate) enum Describe<'a> {
    G(char, &'a Group),
    I(&'a Ident),
    P(char),
    L(&'a Literal),
}

/// Describe the structure of tokens.
pub(crate) fn describe_tokens<'a>(tokens: &'a [TokenTree]) -> Vec<Describe<'a>> {
    tokens
        .iter()
        .map(|t| match t {
            TokenTree::Group(g) => Describe::G(g.delimiter_left(), g),
            TokenTree::Ident(i) => Describe::I(i),
            TokenTree::Punct(p) => Describe::P(p.as_char()),
            TokenTree::Literal(l) => Describe::L(l),
        })
        .collect()
}

/// Split `#[meta = ...] others` into `(meta, others)`.
pub(crate) fn split_meta(tokens: &[TokenTree]) -> (&[TokenTree], &[TokenTree]) {
    use Describe::{G, P};
    // Collect metas (ex. #[doc = r"foo"]).
    let meta_count = {
        let mut i = 0;
        loop {
            if i + 2 > tokens.len() {
                break i;
            }
            match describe_tokens(&tokens[i..i + 2])[..] {
                [P('#'), G('[', _g)] => {
                    i += 2;
                }
                _ => break i,
            }
        }
    };
    (&tokens[..meta_count], &tokens[meta_count..])
}

trait GroupExt {
    fn delimiter_left(&self) -> char;
    fn delimiter_right(&self) -> char;
}

impl GroupExt for Group {
    fn delimiter_left(&self) -> char {
        match self.delimiter() {
            Delimiter::Parenthesis => '(',
            Delimiter::Brace => '{',
            Delimiter::Bracket => '[',
            _ => ' ',
        }
    }

    fn delimiter_right(&self) -> char {
        match self.delimiter() {
            Delimiter::Parenthesis => ')',
            Delimiter::Brace => '}',
            Delimiter::Bracket => ']',
            _ => ' ',
        }
    }
}

// Repalce '...' with `to`.
fn replace_3dots_impl(
    tokens: TokenStream,
    to: &TokenStream,
    append_if_missing: &mut bool,
    level: usize,
) -> TokenStream {
    let tokens: Vec<_> = tokens.into_iter().collect();
    let mut new_tokens = TokenStream::new();
    let mut i = 0;
    loop {
        match (tokens.get(i), tokens.get(i + 1), tokens.get(i + 2)) {
            (
                Some(TokenTree::Punct(p1)),
                Some(TokenTree::Punct(p2)),
                Some(TokenTree::Punct(p3)),
            ) if p1.as_char() == '.' && p2.as_char() == '.' && p3.as_char() == '.' => {
                new_tokens.extend(to.clone());
                *append_if_missing = false;
                i += 3;
            }
            (Some(TokenTree::Group(g)), _, _) => {
                let g = Group::new(
                    g.delimiter(),
                    replace_3dots_impl(g.stream(), to, append_if_missing, level + 1),
                );
                new_tokens.extend(std::iter::once(TokenTree::Group(g)));
                i += 1;
            }
            (Some(token), _, _) => {
                new_tokens.extend(std::iter::once(token.clone()));
                i += 1;
            }
            (None, _, _) => {
                if *append_if_missing && level == 0 {
                    new_tokens.extend(to.clone());
                }
                return new_tokens;
            }
        }
    }
}

pub(crate) fn replace_or_append_3dots(tokens: TokenStream, to: &TokenStream) -> TokenStream {
    let mut append = true;
    replace_3dots_impl(tokens, to, &mut append, 0)
}

pub(crate) fn replace_3dots(tokens: TokenStream, to: &TokenStream) -> TokenStream {
    let mut append = false;
    replace_3dots_impl(tokens, to, &mut append, 0)
}

// Repalce '....' with `to`.
pub(crate) fn replace_4dots(tokens: TokenStream, to: &TokenStream) -> TokenStream {
    let tokens: Vec<_> = tokens.into_iter().collect();
    let mut new_tokens = TokenStream::new();
    let mut i = 0;
    loop {
        match (
            tokens.get(i),
            tokens.get(i + 1),
            tokens.get(i + 2),
            tokens.get(i + 3),
        ) {
            (
                Some(TokenTree::Punct(p1)),
                Some(TokenTree::Punct(p2)),
                Some(TokenTree::Punct(p3)),
                Some(TokenTree::Punct(p4)),
            ) if p1.as_char() == '.'
                && p2.as_char() == '.'
                && p3.as_char() == '.'
                && p4.as_char() == '.' =>
            {
                new_tokens.extend(to.clone());
                i += 4;
            }
            (Some(TokenTree::Group(g)), _, _, _) => {
                let g = Group::new(g.delimiter(), replace_4dots(g.stream(), to));
                new_tokens.extend(std::iter::once(TokenTree::Group(g)));
                i += 1;
            }
            (Some(token), _, _, _) => {
                new_tokens.extend(std::iter::once(token.clone()));
                i += 1;
            }
            (None, _, _, _) => return new_tokens,
        }
    }
}

// Remove `:ty` from macro pattern captures.
pub(crate) fn remove_pattern_capture_types(tokens: TokenStream) -> TokenStream {
    let tokens: Vec<_> = tokens.into_iter().collect();
    let mut new_tokens = TokenStream::new();
    let mut i = 0;
    loop {
        match (tokens.get(i), tokens.get(i + 1)) {
            (Some(TokenTree::Punct(p)), Some(TokenTree::Ident(id)))
                if p.as_char() == ':' && MACRO_PAT_TYPES.contains(&id.to_string().as_str()) =>
            {
                i += 2;
            }
            (Some(TokenTree::Group(g)), _) => {
                let g = Group::new(g.delimiter(), remove_pattern_capture_types(g.stream()));
                new_tokens.extend(std::iter::once(TokenTree::Group(g)));
                i += 1;
            }
            (Some(token), _) => {
                new_tokens.extend(std::iter::once(token.clone()));
                i += 1;
            }
            (None, _) => return new_tokens,
        }
    }
}

static MACRO_PAT_TYPES: &[&str] = &[
    "item", "block", "stmt", "pat", "expr", "ty", "ident", "path", "meta", "tt",
];

#[cfg(any(test, feature = "debug"))]
pub(crate) fn to_string(tokens: TokenStream, width: usize) -> String {
    format!("\n{}", to_string_vec(tokens, width).concat().trim_end())
}

#[cfg(any(test, feature = "debug"))]
fn to_string_vec(tokens: TokenStream, width: usize) -> Vec<String> {
    let mut result: Vec<String> = Vec::new();
    let new_line = |result: &mut Vec<String>| match result.last_mut() {
        None => {}
        Some(s) if s.ends_with('\n') || s.is_empty() => {}
        Some(s) => {
            *s = s.trim_end().to_string();
            s.push('\n')
        }
    };
    let push_str = |result: &mut Vec<String>, s: &str| {
        match result.last_mut() {
            None => {
                result.push(s.to_string());
                return;
            }
            Some(s) if s.ends_with('\n') => { /* break */ }
            Some(l) => {
                l.push_str(s);
                return;
            }
        }
        result.push(s.to_string());
    };
    for t in tokens {
        match &t {
            TokenTree::Group(g) => {
                let s = t.to_string();
                let last_len = match result.last() {
                    None => 0,
                    Some(l) if l.ends_with('\n') => 0,
                    Some(l) => l.len(),
                };
                if s.len() + last_len < width {
                    push_str(&mut result, &s);
                    push_str(&mut result, " ");
                } else {
                    let v = to_string_vec(g.stream(), width);
                    let l = g.delimiter_left();
                    let r = g.delimiter_right();
                    new_line(&mut result);
                    result.push(l.to_string());
                    new_line(&mut result);
                    for line in v {
                        result.push(format!("  {}", line));
                        new_line(&mut result);
                    }
                    result.push(r.to_string());
                    new_line(&mut result);
                }
            }
            TokenTree::Punct(p) => {
                push_str(&mut result, &t.to_string());
                if p.spacing() == Spacing::Alone {
                    push_str(&mut result, " ");
                }
            }
            _ => push_str(&mut result, &format!("{} ", &t)),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn test_replace_3dots() {
        let q = quote! { $($a:ty, ... $b:ty) };
        let to = quote! { $i:tt };
        let q = replace_3dots(q, &to);
        assert_eq!(q.to_string(), "$ ($ a : ty , $ i : tt $ b : ty)");
    }

    #[test]
    fn test_remove_pattern_capture_types() {
        let q = quote! { $($a:ty, $b:ident)* $c:expr };
        let q = remove_pattern_capture_types(q);
        assert_eq!(q.to_string(), "$ ($ a , $ b) * $ c");
    }
}
