#![allow(clippy::ignored_unit_patterns)]

#[cfg(feature = "serde_impl")]
mod serde;

mod impls;

#[cfg(not(target_arch = "wasm32"))]
use crate::{Deserializer, tape::Node};
#[cfg(not(target_arch = "wasm32"))]
use value_trait::prelude::*;

#[test]
fn test_send_sync() {
    struct TestStruct<T: Sync + Send>(T);
    #[allow(let_underscore_drop)] // test
    let _: TestStruct<_> = TestStruct(super::AlignedBuf::with_capacity(0));
}

#[test]
fn test_tape_object_simple() {
    let mut d = String::from("a:\n  b:\n    c: Hamza\n  d: Dadda");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("");
    println!("{:?}", simd.tape);
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 1, count: 8 },
            Node::String("a"),
            Node::Object { len: 2, count: 6 },
            Node::String("b"),
            Node::Object { len: 1, count: 2 },
            Node::String("c"),
            Node::String("Hamza"),
            Node::String("d"),
            Node::String("Dadda")
        ]
    );
}

#[test]
fn playground() {
    let mut d = String::from("tags[3]:                   a,b,c");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("");
    println!("{:?}", simd.tape);
}

#[test]
fn test_tape_inline_string_array() {
    let mut d = String::from("tags[3]: rust,parser,simd");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 1, count: 5 },
            Node::String("tags"),
            Node::Array { len: 3, count: 3 },
            Node::String("rust"),
            Node::String("parser"),
            Node::String("simd"),
        ]
    );
}

#[test]
fn test_tape_inline_number_array() {
    let mut d = String::from("numbers[3]: 1,2,3");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    println!("{:?}", simd.tape);
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 1, count: 5 },
            Node::String("numbers"),
            Node::Array { len: 3, count: 3 },
            Node::Static(StaticNode::U64(1)),
            Node::Static(StaticNode::U64(2)),
            Node::Static(StaticNode::U64(3)),
        ]
    );
}

#[test]
fn test_tape_inline_bool_array() {
    let mut d = String::from("flags[3]: true,false,true");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 1, count: 5 },
            Node::String("flags"),
            Node::Array { len: 3, count: 3 },
            Node::Static(StaticNode::Bool(true)),
            Node::Static(StaticNode::Bool(false)),
            Node::Static(StaticNode::Bool(true)),
        ]
    );
}

#[test]
fn test_tape_empty_array() {
    let mut d = String::from("empty[0]:\nother: val");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 2, count: 4 },
            Node::String("empty"),
            Node::Array { len: 0, count: 0 },
            Node::String("other"),
            Node::String("val"),
        ]
    );
}

#[test]
fn test_tape_array_with_sibling_key() {
    // Array followed by another key-value pair
    let mut d = String::from("tags[3]: rust,parser,simd\nver: 1");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 2, count: 7 },
            Node::String("tags"),
            Node::Array { len: 3, count: 3 },
            Node::String("rust"),
            Node::String("parser"),
            Node::String("simd"),
            Node::String("ver"),
            Node::Static(StaticNode::U64(1)),
        ]
    );
}

#[test]
fn test_tape_tabular_string_array() {
    // users[2]{id,name}:\n  1,Alice\n  2,Bob
    let mut d = String::from("users[2]{id,name}:\n  1,Alice\n  2,Bob");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    println!("{:?}", simd.tape);
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 1, count: 12 },
            Node::String("users"),
            Node::Array { len: 2, count: 10 },
            Node::Object { len: 2, count: 4 },
            Node::String("id"),
            Node::Static(StaticNode::U64(1)),
            Node::String("name"),
            Node::String("Alice"),
            Node::Object { len: 2, count: 4 },
            Node::String("id"),
            Node::Static(StaticNode::U64(2)),
            Node::String("name"),
            Node::String("Bob"),
        ]
    );
}

#[test]
fn test_tape_tabular_mixed_types() {
    // items[2]{sku,qty,price}:\n  A1,2,9.99\n  B2,1,14.5
    let mut d = String::from("items[2]{sku,qty,price}:\n  A1,2,9.99\n  B2,1,14.5");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    println!("{:?}", simd.tape);
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 1, count: 16 },
            Node::String("items"),
            Node::Array { len: 2, count: 14 },
            Node::Object { len: 3, count: 6 },
            Node::String("sku"),
            Node::String("A1"),
            Node::String("qty"),
            Node::Static(StaticNode::U64(2)),
            Node::String("price"),
            Node::Static(StaticNode::F64(9.99)),
            Node::Object { len: 3, count: 6 },
            Node::String("sku"),
            Node::String("B2"),
            Node::String("qty"),
            Node::Static(StaticNode::U64(1)),
            Node::String("price"),
            Node::Static(StaticNode::F64(14.5)),
        ]
    );
}

#[test]
fn test_tape_tabular_with_sibling_key() {
    // Tabular array followed by another key-value pair at the same level
    let mut d = String::from("users[2]{id,name}:\n  1,Alice\n  2,Bob\nver: 2");
    let d = unsafe { d.as_bytes_mut() };
    let simd = Deserializer::from_slice(d).expect("failed to parse");
    println!("{:?}", simd.tape);
    assert_eq!(
        simd.tape,
        [
            Node::Object { len: 2, count: 14 },
            Node::String("users"),
            Node::Array { len: 2, count: 10 },
            Node::Object { len: 2, count: 4 },
            Node::String("id"),
            Node::Static(StaticNode::U64(1)),
            Node::String("name"),
            Node::String("Alice"),
            Node::Object { len: 2, count: 4 },
            Node::String("id"),
            Node::Static(StaticNode::U64(2)),
            Node::String("name"),
            Node::String("Bob"),
            Node::String("ver"),
            Node::Static(StaticNode::U64(2)),
        ]
    );
}
