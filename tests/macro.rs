use better_repr::{BetterRepr, Flat};

#[test]
fn test_01() {
    #[derive(BetterRepr)]
    struct Test {
        #[better_repr(copy)]
        a: u32,
        #[better_repr(copy)]
        b: u32,
        #[better_repr(inline_string)]
        c: String,
        #[better_repr(inline_list, <u8>)]
        d: Vec<u8>,
    }

    let plain = Test {
        a: 1,
        b: 2,
        c: "hello".to_string(),
        d: vec![1, 2, 3],
    };

    let flat = Flat::from_plain(&plain);

    // 4 a + 4 b + 4 offset+len c + 4 offset+len d + 5 string bytes c + 3 list bytes d
    assert_eq!(flat.size(), 4 + 8 + 4 + 5 + 3);

    assert_eq!(flat.a(), 1);
    assert_eq!(flat.b(), 2);
    assert_eq!(flat.c(), "hello");
    assert_eq!(flat.d(), vec![1, 2, 3]);
}
