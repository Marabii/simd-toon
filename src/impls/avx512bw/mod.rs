#![allow(unused_imports, dead_code)]
mod deser;
mod stage1;
mod classify_bytes;

pub(crate) use classify_bytes::{BasicTypes, classify_bytes_avx512};
pub(crate) use deser::parse_str;
pub(crate) use stage1::SimdInput;
