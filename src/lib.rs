#![deny(missing_docs)]

//! # stateful_macro_rules
//!
//! `stateful_macro_rules` makes it easier to write `macro_rules` with states.
//! It is especially useful where the `macro_rule` needs to take a list of
//! inputs with various patterns.
//!
//! Refer to [`stateful_macro_rules`](macro.stateful_macro_rules.html) for
//! the documentation of the main macro.

use proc_macro::TokenStream as StdTokenStream;
use proc_macro2::TokenStream;

mod error;
mod state;
mod util;

use error::Error;
use error::Result;
use state::StatefulMacroRule;
use util::describe_tokens;
use util::split_meta;
use util::split_tokens;
use util::Describe::{G, I, P};

/// Generate `macro_rules!` macros that have states.
///
/// ## Basic: Macro name and body
///
/// To specify the generated macro name and its final expanded content, use
/// `name() { body }`.
///
/// For example, the code below generates a macro called `foo!()` and it
/// expands to `"foo"`.
///
/// ```
/// # use stateful_macro_rules::stateful_macro_rules;
/// stateful_macro_rules! {
///     foo() { "foo" };
/// }
/// ```
///
/// ## States
///
/// To define states, add them to the `()` after the macro name. A state can
/// be defined as `state_name: (pattern) = (default_value))`. Multiple
/// states are separated by `,`. States can be referred by their pattern
/// name.
///
/// For example, the code below defines a `plus!()` macro with `x` and `y`
/// states (but there is no real way to use this macro):
///
/// ```
/// # use stateful_macro_rules::stateful_macro_rules;
/// stateful_macro_rules! {
///     pos(x: ($x:expr) = (0), y: ($y:expr) = (0)) { ($x, $y) };
/// }
/// ```
///
/// ## Rules
///
/// To make the macro useful, macro rules like `(pattern) => { body }` are
/// needed. Unlike rules in classic `macro_rules!`, the `pattern` matches
/// incomplete tokens (use `...` to mark the incomplete portion), and `body`
/// can only contain changes to states like `state_name.set(tokens)`, or
/// `state_name.append(tokens)`.
///
/// ```
/// # #![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]
/// # use stateful_macro_rules::stateful_macro_rules;
/// stateful_macro_rules! {
///     pos(x: ($x:expr) = (0), y: ($y:expr) = (0)) { ($x, $y) };
///     (incx($i:expr) ...) => { x.set($x + $i); };
///     (incx ...) => { x.set($x + 1); };
///     (incy($i:expr) ...) => { y.set($y + $i); };
///     (incy ...) => { y.set($y + 1); };
///
///     // `...` is implicitly at the end
///     (reset) => { x.set(0); y.set(0); };  
///
///     // `...` can be in the middle
///     (eval( ... )) => { };
/// }
/// assert_eq!(pos!(incx(3) reset incy incy(10) incx), (1, 11));
/// assert_eq!(pos!(eval(incx(10) incy(20))), (10, 20));
/// ```
///
/// # Conditional Rules
///
/// Rules can be disabled by states. This is done by the `when` block.
///
/// ```
/// # #![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]
/// # use stateful_macro_rules::stateful_macro_rules;
/// stateful_macro_rules! {
///     toggle(result: ($v:tt) = (0)) { $v };
///     (T) when { result: (0) } => { result.set(1); };
///     (T) when { result: (1) } => { result.set(0); };
/// }
/// assert_eq!(toggle![T T T], 1);
/// ```
///
/// # Debugging
///
/// If the `debug` feature is on, `debug;` can be used to print the
/// generated code to stderr. The code can be used as a drop-in macro
/// replacement to help debugging.
#[proc_macro]
pub fn stateful_macro_rules(input: StdTokenStream) -> StdTokenStream {
    match stateful_macro_rules_fallible(input.into()) {
        Ok(t) => t.into(),
        Err(e) => e.into(),
    }
}

pub(crate) fn stateful_macro_rules_fallible(input: TokenStream) -> Result<TokenStream> {
    let mut rule = StatefulMacroRule::default();
    #[cfg(feature = "debug")]
    let mut debug = false;
    for tokens in split_tokens(input, ';') {
        let (meta, tokens) = split_meta(&tokens);
        match describe_tokens(tokens)[..] {
            // #[doc = r"foo"]
            // name ( k: ty = v, k: ty = v, ) { result }
            [I(i), G('(', s), G('{', r)] => {
                rule.set_attributes(meta)?;
                rule.set_name(i.clone())?;
                rule.set_state(s)?;
                rule.set_return(None, r)?;
            }

            // #[doc = r"foo"]
            // name ( k: ty = v, k: ty = v, ) { result }
            [I(i), G('(', s), I(iw), G('{', w), G('{', r)] if iw.to_string() == "when" => {
                rule.set_attributes(meta)?;
                rule.set_name(i.clone())?;
                rule.set_state(s)?;
                rule.set_return(Some(w.stream()), r)?;
            }

            // (pat) => { ... }
            [G('(', pat), P('='), P('>'), G('{', body)] => {
                rule.append_rule(pat.stream(), None, body.stream())?;
            }

            // (pat) when { state_name: (pat), ... } => { ... }
            [G('(', pat), I(w), G('{', state), P('='), P('>'), G('{', body)]
                if w.to_string() == "when" =>
            {
                rule.append_rule(pat.stream(), Some(state.stream()), body.stream())?;
            }

            // debug; write expanded macro to stderr for debugging purpose.
            #[cfg(feature = "debug")]
            [I(i)] if i.to_string() == "debug" => {
                debug = true;
            }

            _ => {
                return Err(Error::UnexpectedTokens(
                    tokens.to_vec(),
                    concat!(
                        "expect 'macro_name(state_name: (ty) = (default), ...) { ... };',",
                        " or '(...) => { ... };'",
                        " or '(...) when { state: (pat), ... } => { ... };'",
                        " in stateful_macro_rule!"
                    ),
                ))
            }
        }
    }

    let code = rule.generate_code()?;
    #[cfg(feature = "debug")]
    if debug {
        eprintln!("{}", util::to_string(code.clone(), 100));
    }
    Ok(code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::dollar;
    use quote::quote;

    fn to_string(t: TokenStream) -> String {
        crate::util::to_string(t, 80)
    }

    #[test]
    fn test_minimal_example() {
        let q = stateful_macro_rules_fallible(quote! {
            minimal() { "foo" }
        })
        .unwrap();

        assert_eq!(
            to_string(q),
            r#"
# [macro_export] macro_rules ! minimal
{
  { $ ($ tt : tt) * } => { $ crate :: __minimal_state ! ([$ ($ tt) *] { }) } ;
}
# [doc (hidden)] # [macro_export] macro_rules ! __minimal_state
{
  ([] { }) => { "foo" } ;
}"#
        );
    }

    #[test]
    fn test_attributes() {
        let q = stateful_macro_rules_fallible(quote! {
            #[cfg(feature = "bar")]
            /// Some comment.
            /// Foo bar.
            attriute_test() { 1 }
        })
        .unwrap();

        assert_eq!(
            to_string(q),
            r#"
# [macro_export] # [cfg (feature = "bar")] # [doc = r" Some comment."] #
[
  doc = r" Foo bar."
]
macro_rules ! attriute_test
{
  { $ ($ tt : tt) * } =>
  {
    $ crate :: __attriute_test_state ! ([$ ($ tt) *] { })
  }
  ;
}
# [doc (hidden)] # [macro_export] macro_rules ! __attriute_test_state
{
  ([] { }) => { 1 } ;
}"#
        );
    }

    #[test]
    fn test_when_clause() {
        let d = dollar();
        let q = stateful_macro_rules_fallible(quote! {
            w(b: (#d b:tt) = (false)) when { b: (true) } { "ok" };
            (t) when { b: (false) } => { b.set(true) };
        })
        .unwrap();

        assert_eq!(
            to_string(q),
            r#"
# [macro_export] macro_rules ! w
{
  { $ ($ tt : tt) * } => { $ crate :: __w_state ! ([$ ($ tt) *] { b [false] }) } ;
}
# [doc (hidden)] # [macro_export] macro_rules ! __w_state
{
  ([] { b [true] }) => { "ok" } ; ([t $ ($ _ddd : tt) *] { b [false] }) =>
  {
    $ crate :: __w_state ! ([$ ($ _ddd) *] { b [true] })
  }
  ;
}"#
        );
    }

    #[test]
    fn test_complex_example() {
        let d = dollar();
        let q = stateful_macro_rules_fallible(quote! {
            #[allow(dead_code)]
            /// Foo bar
            foo(
                x: (#d (#d i:expr)*) = (1 2),
                y: (#d (#d j:ident)*),
                z: (#d (#d t:tt)* ) = (x),
            ) {{
                let v1 = vec![#d (#d i),*];
                let v2 = vec![#d (stringify!(#d j)),*];
                format!("{:?} {:?}", v1, v2)
            }};

            // Implicit "..."
            (y += #d t:ident) => {
                y.append(#d t);
            };

            // Matching state (z).
            (y = #d t:ident ...) when { z: (x) } => {
                y.set(#d t);
                z.append(y);
            };

            (e(#d d:ident, #d e:expr) ...) => {
                x.append(#d e);
                y.append(#d d);
                // 4 dots: the entire input without "...".
                z.append(....);
            }
        })
        .unwrap();
        assert_eq!(
            to_string(q),
            r#"
# [macro_export] # [allow (dead_code)] # [doc = r" Foo bar"] macro_rules ! foo
{
  { $ ($ tt : tt) * } =>
  {
    $ crate :: __foo_state ! ([$ ($ tt) *] { x [1 2] y [] z [x] })
  }
  ;
}
# [doc (hidden)] # [macro_export] macro_rules ! __foo_state
{
  ([] { x [$ ($ i : expr) *] y [$ ($ j : ident) *] z [$ ($ t : tt) *] }) =>
  {
    {
      let v1 = vec ! [$ ($ i) , *] ; let v2 = vec ! [$ (stringify ! ($ j)) , *] ; format !
      (
        "{:?} {:?}" , v1 , v2
      )
    }
  }
  ;
  (
    [y += $ t : ident $ ($ _ddd : tt) *]
    {
      x [$ ($ i : expr) *] y [$ ($ j : ident) *] z [$ ($ t : tt) *]
    }
  )
  =>
  {
    $ crate :: __foo_state !
    (
      [$ ($ _ddd) *] { x [$ ($ i) *] y [$ ($ j) * $ t] z [$ ($ t) *] }
    )
  }
  ;
  (
    [y = $ t : ident $ ($ _ddd : tt) *]
    {
      x [$ ($ i : expr) *] y [$ ($ j : ident) *] z [x]
    }
  )
  =>
  {
    $ crate :: __foo_state ! ([$ ($ _ddd) *] { x [$ ($ i) *] y [$ t] z [x y] })
  }
  ;
  (
    [e ($ d : ident , $ e : expr) $ ($ _ddd : tt) *]
    {
      x [$ ($ i : expr) *] y [$ ($ j : ident) *] z [$ ($ t : tt) *]
    }
  )
  =>
  {
    $ crate :: __foo_state !
    (
      [$ ($ _ddd) *]
      {
        x [$ ($ i) * $ e] y [$ ($ j) * $ d] z [$ ($ t) * e ($ d , $ e)]
      }
    )
  }
  ;
}"#
        );
    }
}
