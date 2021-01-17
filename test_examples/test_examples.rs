#[cfg(test)]
#[macro_use]
extern crate macro_examples;

#[test]
fn test_foo() {
    assert_eq!(foo!(), "foo");
}

#[test]
fn test_sum() {
    let v = sum![10 forty two 20 (10 * 20)];
    assert_eq!(v, 272);
}

#[test]
fn test_define_constants() {
    constants! { A, B, C, D = 10, E, F, }

    assert_eq!(A, 0);
    assert_eq!(C, 2);
    assert_eq!(F, 12);
}

#[test]
fn test_draw() {
    let d = walk!(up right down down left left up up);
    assert_eq!(d, [[8, 1, 2], [7, 0, 3], [6, 5, 4]]);
}

#[test]
fn test_doc_fields() {
    doc_fields! {
        #[allow(dead_code)]
        #[derive(Default)]
        struct Pos {
            /// Line index, starting from 1.
            pub line: usize,
            /// Colum index, starting from 0.
            pub col: usize,

            undocumented_field: Option<u8>,
        }
    }

    let _p = Pos::default();
    let d = Pos::doc();
    assert_eq!(
        d.into_iter().collect::<Vec<_>>(),
        [
            ("col", " Colum index, starting from 0."),
            ("line", " Line index, starting from 1.")
        ]
    );
}
