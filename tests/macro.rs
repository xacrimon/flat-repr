use flat_repr::{Flattenable, Flat};

#[derive(Flattenable)]
struct Test {
    a: u32,
    b: u32,
    #[flat_repr(outlined_copy_option, <u128>)]
    e: Option<u128>,
    #[flat_repr(inline_string)]
    c: String,
    #[flat_repr(inline_list, <u8>)]
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
    assert_eq!(flat2.e(), Some(&123));
}
