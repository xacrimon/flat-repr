use better_repr::{BetterRepr, Flat};

#[derive(BetterRepr)]
struct Test {
    a: u32,
    b: u32,

    #[better_repr(outlined_copy_option, <u128>)]
    e: Option<u128>,

    #[better_repr(inline_string)]
    c: String,

    #[better_repr(inline_list, <u8>)]
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

    plain.e = Some(123);
    let flat2 = Flat::from_plain(&plain);
    assert_eq!(flat2.dst_size(), 28);
}
