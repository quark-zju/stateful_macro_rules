use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Punct;
use proc_macro2::TokenStream;
use proc_macro2::{Spacing, TokenTree};
use quote::format_ident;
use quote::quote;
use std::collections::BTreeMap;

use crate::error::Error;
use crate::error::Result;
use crate::util::describe_tokens;
use crate::util::remove_pattern_capture_types;
use crate::util::replace_3dots;
use crate::util::replace_4dots;
use crate::util::replace_or_append_3dots;
use crate::util::split_tokens;
use crate::util::Describe::{G, I, P};

#[derive(Clone)]
struct State {
    pat: TokenStream,
    default: TokenStream,
}

#[derive(Clone)]
struct StateOp {
    name: String,
    action: Action,
}

#[derive(Clone)]
enum Action {
    Append(TokenStream),
    Replace(TokenStream),
}

struct Rule {
    input_pat: TokenStream,
    state_conditions: BTreeMap<String, TokenStream>,
    ops: Vec<StateOp>,
}

struct Conclude {
    body: TokenStream,
    state_conditions: BTreeMap<String, TokenStream>,
}

#[derive(Default)]
pub(crate) struct StatefulMacroRule {
    attributes: TokenStream,
    name: Option<Ident>,
    states: BTreeMap<String, State>,
    rules: Vec<Rule>,
    conclude: Option<Conclude>,
}

impl StatefulMacroRule {
    pub fn set_attributes(&mut self, tokens: &[TokenTree]) -> Result<()> {
        self.attributes.extend(tokens.iter().cloned());
        Ok(())
    }

    pub fn set_name(&mut self, name: Ident) -> Result<()> {
        if self.name.is_some() {
            return Err(Error::UnexpectedTokens(
                vec![TokenTree::Ident(name)],
                "cannot re-assign name",
            ));
        }
        self.name = Some(name);
        Ok(())
    }

    pub fn set_state(&mut self, state: &Group) -> Result<()> {
        for tokens in split_tokens(state.stream(), ',') {
            // name: (ty) = (value),
            match describe_tokens(&tokens)[..] {
                // name: (ty),
                [I(i), P(':'), G('(', g)] => {
                    let state = State {
                        pat: g.stream(),
                        default: Default::default(),
                    };
                    self.states.insert(i.to_string(), state);
                }

                // name: (ty) = (init),
                [I(i), P(':'), G('(', g), P('='), G('(', v)] => {
                    let state = State {
                        pat: g.stream(),
                        default: v.stream(),
                    };
                    self.states.insert(i.to_string(), state);
                }

                _ => {
                    return Err(Error::UnexpectedTokens(
                        tokens,
                        "expect 'name: (ty),' or 'name: (pat) = (default),' in 'state { ... }'",
                    ))
                }
            };
        }
        Ok(())
    }

    pub fn append_rule(
        &mut self,
        input_pat: TokenStream,
        when: Option<TokenStream>,
        body: TokenStream,
    ) -> Result<()> {
        // Turn body into actions.
        let mut ops = Vec::new();
        for tokens in split_tokens(body, ';') {
            match describe_tokens(&tokens)[..] {
                [I(name), P('.'), I(op), G('(', pat)] if op.to_string() == "set" && self.states.contains_key(&name.to_string()) => {
                    ops.push(StateOp {
                        name: name.to_string(),
                        action: Action::Replace(pat.stream()),
                    });
                }
                [I(name), P('.'), I(op), G('(', pat)] if op.to_string() == "append" && self.states.contains_key(&name.to_string()) => {
                    ops.push(StateOp {
                        name: name.to_string(),
                        action: Action::Append(pat.stream()),
                    });
                }
                _ => {
                    return Err(Error::UnexpectedTokens(
                        tokens,
                        "expect 'state_name.set(pat);' or 'state_name.append(pat);' in '(pat) => { HERE }'",
                    ))
                }
            }
        }
        // Turn state into conditions.
        let state_conditions = self.parse_when_clause(when.unwrap_or_default())?;
        self.rules.push(Rule {
            input_pat,
            state_conditions,
            ops,
        });
        Ok(())
    }

    /// Parse "when" condition clause like `x: (true), y: ($y:tt)`.
    fn parse_when_clause(&self, tokens: TokenStream) -> Result<BTreeMap<String, TokenStream>> {
        let mut conditions = BTreeMap::new();
        for tokens in split_tokens(tokens, ',') {
            match describe_tokens(&tokens)[..] {
                // name: (pat)
                [I(name), P(':'), G('(', pat)] if self.states.contains_key(&name.to_string()) => {
                    conditions.insert(name.to_string(), pat.stream());
                }
                _ => {
                    return Err(Error::UnexpectedTokens(
                        tokens,
                        "expect 'state_name: (pat),' in \"when\" block",
                    ))
                }
            }
        }
        Ok(conditions)
    }

    pub fn set_return(&mut self, when: Option<TokenStream>, body: &Group) -> Result<()> {
        if self.conclude.is_some() {
            return Err(Error::UnexpectedTokens(
                vec![TokenTree::Group(body.clone())],
                "cannot re-assign return block",
            ));
        }
        let conclude = Conclude {
            body: body.stream(),
            state_conditions: self.parse_when_clause(when.unwrap_or_default())?,
        };
        self.conclude = Some(conclude);
        Ok(())
    }

    pub fn generate_code(&self) -> Result<TokenStream> {
        let name = match &self.name {
            Some(n) => n,
            None => return Err(Error::Message("missing macro name")),
        };
        let conclude = match &self.conclude {
            Some(c) => c,
            None => return Err(Error::Message("missing return block")),
        };

        let private_name = format_ident!("__{}_state", name);
        let dollar = dollar();
        let tt = quote!(#dollar tt);
        let input_state = self.map_flatten_state(|n, s| match conclude.state_conditions.get(n) {
            Some(p) => p.clone(),
            None => s.pat.clone(),
        });
        let initial_state = self.map_flatten_state(|_, s| s.default.clone());
        let call_self = quote! { #dollar crate::#private_name ! };
        let rules = self.generate_rule_code(&call_self)?;
        let attributes = &self.attributes;
        let return_block = &conclude.body;
        let q = quote! {
            #[macro_export]
            #attributes
            macro_rules! #name {
                { #dollar( #tt:tt )* } => {
                    #call_self ([ #dollar( #tt )* ] { #initial_state })
                };
            }

            #[doc(hidden)]
            #[macro_export]
            macro_rules! #private_name {
                ([] {#input_state}) => { #return_block };
                #rules
            }
        };

        Ok(q)
    }

    fn generate_rule_code(&self, call_self: &TokenStream) -> Result<TokenStream> {
        let d = dollar();
        let dotdotdot_pat = quote! { #d(#d _ddd:tt)* };
        let dotdotdot = remove_pattern_capture_types(dotdotdot_pat.clone());
        let mut rules = Vec::new();
        for rule in &self.rules {
            let input_pat = replace_or_append_3dots(rule.input_pat.clone(), &dotdotdot_pat);
            let input_state = map(&self.states, |n, s| match rule.state_conditions.get(n) {
                Some(p) => p.clone(),
                None => s.pat.clone(),
            });
            let mut new_state: BTreeMap<String, TokenStream> =
                map(&input_state, |_, v| remove_pattern_capture_types(v.clone()));
            for op in &rule.ops {
                // op.name was checked by append_rule.
                new_state
                    .entry(op.name.clone())
                    .and_modify(|t| match &op.action {
                        Action::Append(pat) => t.extend(pat.clone()),
                        Action::Replace(pat) => *t = pat.clone(),
                    });
            }
            let output_state = flatten(new_state.into_iter().map(|(k, v)| {
                // Replace "...." (4 dots) with input without "...".
                let v = replace_4dots(
                    v,
                    &remove_pattern_capture_types(replace_3dots(
                        rule.input_pat.clone(),
                        &TokenStream::new(),
                    )),
                );
                // Replace "..." (3 dots) with dotdotdot.
                let v = replace_3dots(v, &dotdotdot);
                (k, v)
            }));
            let input_state = flatten(input_state.into_iter());
            rules.push(quote! {
                ([#input_pat] {#input_state}) => {
                    #call_self ([#dotdotdot] {#output_state})
                };
            })
        }
        Ok(quote! { #(#rules)* })
    }

    fn map_flatten_state(&self, f: impl Fn(&str, &State) -> TokenStream) -> TokenStream {
        flatten(map(&self.states, f).into_iter())
    }
}

pub(crate) fn dollar() -> Punct {
    Punct::new('$', Spacing::Alone)
}

fn map<S>(
    states: &BTreeMap<String, S>,
    f: impl Fn(&str, &S) -> TokenStream,
) -> BTreeMap<String, TokenStream> {
    states.iter().map(|(k, v)| (k.clone(), f(k, v))).collect()
}

fn flatten(tokens: impl Iterator<Item = (String, TokenStream)>) -> TokenStream {
    let mut result = Vec::new();
    for (name, pat) in tokens {
        // name [ pat ]
        let name = format_ident!("{}", name);
        result.push(quote! { #name [ #pat ] });
    }
    quote! { #(#result)* }
}
