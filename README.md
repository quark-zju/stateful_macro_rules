# stateful_macro_rules

[![Documentation](https://docs.rs/stateful_macro_rules/badge.svg)](https://docs.rs/stateful_macro_rules)

Rust `macro_rules!` with states.

## Example

Define a stateful macro:

```rust
stateful_macro_rules! {
    /// Auto-incremental i32 constants from 0.
    constants(
        next: ($next:expr) = (0),
        body: ($($body:tt)*),
    ) {
        $($body)*
    };

    ($i:ident = $v:expr, ...) => {
        body.append(const $i: i32 = $v;);
        next.set($v + 1);
    };

    ($i:ident, ...) => {
        body.append(const $i: i32 = $next;);
        next.set($next + 1);
    };
}
```

Use the macro:

```rust
constants! { A, B, C, D = 10, E, F, }

assert_eq!(A, 0);
assert_eq!(C, 2);
assert_eq!(F, 12);
```

Refer to [the documentation](https://docs.rs/stateful_macro_rules) and [macro_examples.rs](test_examples/macro_examples/macro_examples.rs) for more examples.