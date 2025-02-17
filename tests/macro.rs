use flat_repr::{Flat, Flattenable};

#[derive(Flattenable)]
struct Test {
    // The default flat representation is itself, and passes by value
    a: u32,

    // Explicitly specified flat representation, same as the default.
    #[flat_repr = "self(by_value)"]
    b: u32,

    // An option where the Some variant field is stored in the DST tail, allowing smaller size when None.
    #[flat_repr = "option(u128, by_value, small_none)"]
    e: Option<u128>,

    // Inline string, stored in the DST tail.
    #[flat_repr = "string(inline)"]
    c: String,

    // Inline list, stored in the DST tail.
    #[flat_repr = "list(u8, inline)"]
    d: Vec<u8>,
}

#[test]
fn test_01() {
    let mut plain = Test {
        a: 1,
        b: 2,
        c: "hello".to_string(),
        d: vec![1, 2, 3],
        e: None,
    };

    let flat1 = Flat::from_plain(&plain);
    assert_eq!(flat1.dst_size(), 28);
    assert_eq!(flat1.a(), 1);
    assert_eq!(flat1.b(), 2);
    assert_eq!(flat1.c(), "hello");
    assert_eq!(flat1.d(), vec![1, 2, 3]);
    assert_eq!(flat1.e(), None);

    plain.e = Some(123);
    let flat2 = Flat::from_plain(&plain);
    assert_eq!(flat2.dst_size(), 56);
    assert_eq!(flat2.e(), Some(123));
}
