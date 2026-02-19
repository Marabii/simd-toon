use std::arch::x86_64::{
    __m512i, _MM_CMPINT_EQ, _MM_CMPINT_LT, _MM_CMPINT_NLE, _mm512_cmp_epu8_mask,
    _mm512_loadu_si512, _mm512_mask_loadu_epi8, _mm512_set1_epi8,
};

use crate::charutils::is_not_structural_or_whitespace;

#[derive(Debug, PartialEq)]
pub enum BasicTypes {
    Null,
    Number,
    String,
    Boolean(bool),
}

#[target_feature(enable = "avx512bw")]
#[allow(
    clippy::if_not_else,
    clippy::cast_possible_wrap,
    clippy::too_many_lines
)]
#[cfg_attr(not(feature = "no-inline"), inline)]
pub unsafe fn classify_bytes_avx512(input: &[u8]) -> BasicTypes {
    match input {
        b"true" => return BasicTypes::Boolean(true),
        b"false" => return BasicTypes::Boolean(false),
        _ => {}
    }

    unsafe {
        let mut ptr = input.as_ptr();
        let mut len = input.len();

        if len == 0 {
            return BasicTypes::Null;
        }

        // Handle negative numbers: skip the first byte if it's a '-'
        if *ptr == b'-' {
            ptr = ptr.add(1);
            len -= 1;

            // A single "-" or "-." is a string
            if len == 0 || (len == 1 && *ptr == b'.') {
                return BasicTypes::String;
            }
        } else if len == 1 && *ptr == b'.' {
            // A single "." is a string
            return BasicTypes::String;
        }

        let zero = _mm512_set1_epi8(b'0' as i8);
        let nine = _mm512_set1_epi8(b'9' as i8);
        let dot = _mm512_set1_epi8(b'.' as i8);

        let mut dot_count = 0;

        // Process full 64-byte chunks
        while len >= 64 {
            let chunk = _mm512_loadu_si512(ptr.cast::<__m512i>());

            let less_than_zero = _mm512_cmp_epu8_mask(chunk, zero, _MM_CMPINT_LT);
            let greater_than_nine = _mm512_cmp_epu8_mask(chunk, nine, _MM_CMPINT_NLE);
            let is_dot = _mm512_cmp_epu8_mask(chunk, dot, _MM_CMPINT_EQ);

            let invalid_mask = (less_than_zero & !is_dot) | greater_than_nine;

            if invalid_mask != 0 {
                return BasicTypes::String;
            }

            dot_count += is_dot.count_ones();
            if dot_count > 1 {
                return BasicTypes::String;
            }

            ptr = ptr.add(64);
            len -= 64;
        }

        // Process the remainder
        if len > 0 {
            // Create a mask with `len` number of 1s.
            // Note: Since we are here, `len` is strictly between 1 and 63.
            // Bit-shifting 1 by 64 would overflow, but it's mathematically impossible to reach here with len >= 64.
            let load_mask = (1_u64 << len) - 1;

            // _mm512_mask_loadu_epi8 dynamically loads `len` bytes from memory,
            // and fills the remaining bytes in the 512-bit register with `zero` (ASCII '0').
            let chunk = _mm512_mask_loadu_epi8(zero, load_mask, ptr.cast::<i8>());

            let less_than_zero = _mm512_cmp_epu8_mask(chunk, zero, _MM_CMPINT_LT);
            let greater_than_nine = _mm512_cmp_epu8_mask(chunk, nine, _MM_CMPINT_NLE);
            let is_dot = _mm512_cmp_epu8_mask(chunk, dot, _MM_CMPINT_EQ);

            let invalid_mask = (less_than_zero & !is_dot) | greater_than_nine;

            if invalid_mask != 0 {
                return BasicTypes::String;
            }

            dot_count += is_dot.count_ones();
            if dot_count > 1 {
                return BasicTypes::String;
            }
        }
    }

    BasicTypes::Number
}
