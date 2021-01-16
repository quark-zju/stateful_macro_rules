use stateful_macro_rules::stateful_macro_rules;

stateful_macro_rules! {
    /// Simple macro without states.
    foo() { "foo" }
}

stateful_macro_rules! {
    /// Sum numbers, (expressions), and literal "forty two".
    sum(result: ($r:expr) = (0)) { $r };
    (forty two ...) => { result.set($r + 42) };
    ($e:literal ...) => { result.set($r + $e) };
    (($e:expr) ...) => { result.set($r + $e) };
}

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

stateful_macro_rules! {
    /// Walk in a 3x3 grid starting from the center.
    walk(
        ypos: ($y:expr) = (1),
        xpos: ($x:expr) = (1),
        step: ($s:expr) = (1),
        path: ($($p:tt)*),
    ) {{
        let mut out = [[0,0,0], [0,0,0], [0,0,0]];
        for (y, x, s) in vec![$($p),*] {
            out[y][x] = s;
        }
        out
    }};

    (up) => {
        path.append(($y - 1, $x, $s));
        step.set($s + 1);
        ypos.set($y - 1);
    };

    (down) => {
        path.append(($y + 1, $x, $s));
        step.set($s + 1);
        ypos.set($y + 1);
    };

    (left) => {
        path.append(($y, $x - 1, $s));
        step.set($s + 1);
        xpos.set($x - 1);
    };

    (right) => {
        path.append(($y, $x + 1, $s));
        step.set($s + 1);
        xpos.set($x + 1);
    };
}

stateful_macro_rules! {
    /// Provide a "doc()" method for documented fields.
    doc_fields(
        struct_name: ($n:ident) = (_x),
        field_doc: ($(($f:tt $d:tt))*),
        body: ($($b:tt)*),
        has_struct: ($i:tt) = (false),
    ) when { has_struct: (true) } {
        $($b)*

        impl $n {
            /// Get a hashmap from field name to docstring.
            pub fn doc() -> ::std::collections::BTreeMap<&'static str, &'static str> {
                #[allow(unused_mut)]
                let mut result: ::std::collections::BTreeMap<&'static str, &'static str> = Default::default();
                $( result.insert(stringify!($f), $d); )*
                result
            }
        }
    };

    // capture the whole block into 'body'.
    (...) when { body: () } => { body.set(...); };

    // struct attributes
    (#[$($m:tt)*] ...) when { has_struct: (false) } => { };

    // struct body
    (struct $sname:ident { ... }) when { has_struct: (false) } => {
        struct_name.set($sname);
        has_struct.set(true);
    };

    // undocumented fields
    ($v:vis $fn:ident : $ft:ty , ...) when { has_struct: (true) } => { };

    // documented fields
    (#[doc=$doc:literal] $v:vis $fname:ident : $ft:ty , ...) when { has_struct: (true) } => {
        field_doc.append(($fname $doc));
    };

    // for debugging purpose (requires "debug" feature)
    // debug;
}
