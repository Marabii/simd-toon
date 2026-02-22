#![allow(dead_code)]
use crate::{
    Stage1Parse,
    macros::{static_cast_i32, static_cast_i64, static_cast_u32},
};
#[cfg(target_arch = "x86")]
use std::arch::x86 as arch;

use arch::{
    __m256i, _mm_clmulepi64_si128, _mm_set_epi64x, _mm_set1_epi8, _mm256_add_epi32,
    _mm256_and_si256, _mm256_cmpeq_epi8, _mm256_loadu_si256, _mm256_max_epu8, _mm256_movemask_epi8,
    _mm256_set_epi32, _mm256_set1_epi8, _mm256_setr_epi8, _mm256_setzero_si256,
    _mm256_shuffle_epi8, _mm256_srli_epi32, _mm256_storeu_si256,
};
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64 as arch;
use std::arch::x86_64::{
    __m512i, _MM_CMPINT_NE, _mm256_set1_epi32, _mm512_and_si512, _mm512_cmp_epi8_mask,
    _mm512_cmpeq_epi8_mask, _mm512_cmple_epu8_mask, _mm512_loadu_si512, _mm512_set1_epi8,
    _mm512_set1_epi32, _mm512_shuffle_epi8,
};
use std::arch::x86_64::{
    _mm_setr_epi8, _mm512_broadcast_i32x4, _mm512_srli_epi32, _mm512_storeu_si512,
};

macro_rules! low_nibble_mask128 {
    () => {
        _mm_setr_epi8(32, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 2, 4, 72, 0, 0)
    };
}

macro_rules! high_nibble_mask128 {
    () => {
        _mm_setr_epi8(81, 0, 44, 1, 0, 10, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0)
    };
}

fn print_m512i_binary(reg: __m512i) {
    unsafe {
        // 1. Create a buffer to hold the 64 bytes (256 bits)
        let mut bytes = [0u8; 64];

        // 2. Store the register contents into the array (unaligned)
        _mm512_storeu_si512(bytes.as_mut_ptr() as *mut __m512i, reg);

        // 3. Print bytes (reversed to show most significant bit on the left)
        for byte in bytes.iter().rev() {
            print!("{:08b} ", byte);
        }
        println!();
    }
}

#[derive(Debug)]
pub(crate) struct SimdInput {
    v0: __m512i,
}

impl Stage1Parse for SimdInput {
    type Utf8Validator = simdutf8::basic::imp::x86::avx2::ChunkedUtf8ValidatorImp;
    type SimdRepresentation = __m512i;
    #[cfg_attr(not(feature = "no-inline"), inline)]
    // _mm256_loadu_si256 does not need alignment
    #[allow(clippy::cast_ptr_alignment)]
    #[target_feature(enable = "avx512bw")]
    unsafe fn new(ptr: &[u8]) -> Self {
        unsafe {
            Self {
                v0: _mm512_loadu_si512(ptr.as_ptr().cast::<__m512i>()),
            }
        }
    }

    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(clippy::cast_sign_loss)]
    #[target_feature(enable = "avx512bw")]
    #[cfg(target_arch = "x86_64")]
    unsafe fn compute_quote_mask(quote_bits: u64) -> u64 {
        unsafe {
            std::arch::x86_64::_mm_cvtsi128_si64(_mm_clmulepi64_si128(
                _mm_set_epi64x(0, static_cast_i64!(quote_bits)),
                _mm_set1_epi8(-1_i8 /* 0xFF */),
                0,
            )) as u64
        }
    }

    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(clippy::cast_sign_loss)]
    #[target_feature(enable = "avx2")]
    #[cfg(target_arch = "x86")]
    unsafe fn compute_quote_mask(quote_bits: u64) -> u64 {
        let mut quote_mask: u64 = quote_bits ^ (quote_bits << 1);
        quote_mask = quote_mask ^ (quote_mask << 2);
        quote_mask = quote_mask ^ (quote_mask << 4);
        quote_mask = quote_mask ^ (quote_mask << 8);
        quote_mask = quote_mask ^ (quote_mask << 16);
        quote_mask = quote_mask ^ (quote_mask << 32);
        quote_mask
    }

    /// Returns a mask where bit i = 1 means byte i of input matches m and vice versa.
    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
    #[target_feature(enable = "avx2")]
    unsafe fn cmp_mask_against_input(&self, m: u8) -> u64 {
        unsafe { _mm512_cmpeq_epi8_mask(self.v0, _mm512_set1_epi8(m as i8)) }
    }

    // find all values less than or equal than the content of maxval.
    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(clippy::cast_sign_loss)]
    #[target_feature(enable = "avx2")]
    unsafe fn unsigned_lteq_against_input(&self, maxval: __m512i) -> u64 {
        unsafe { _mm512_cmple_epu8_mask(self.v0, maxval) }
    }

    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(clippy::cast_sign_loss)]
    #[target_feature(enable = "avx2")]
    unsafe fn find_whitespace_and_structurals(&self, whitespace: &mut u64, structurals: &mut u64) {
        unsafe {
            // Bit 0 (1): : (0x3A), \n (0x0A) -> Structural
            // Bit 1 (2): [ (0x5B), { (0x7B) -> Structural
            // Bit 2 (4): , (0x2C), | (0x7C) -> Structural
            // Bit 3 (8): - (0x2D), ] (0x5D), } (0x7D) -> Structural
            // Bit 4 (16): \t (0x09) -> Structural
            // Bit 5 (32):       (0x20) -> Whitespace
            // Bit 6 (64): \r (0x0D) -> Whitespace

            let low_nibble_mask = _mm512_broadcast_i32x4(low_nibble_mask128!());
            let high_nibble_mask = _mm512_broadcast_i32x4(high_nibble_mask128!());

            // Bits 0, 1, 2, 3, 4 are structurals (1 + 2 + 4 + 8 + 16 = 31 or 0x1F)
            let structural_shufti_mask: __m512i = _mm512_set1_epi8(0x1F);
            // Bits 5, 6 are whitespace (32 + 64 = 96 or 0x60)
            let whitespace_shufti_mask: __m512i = _mm512_set1_epi8(0x60);

            // Remove the MSB cuz otherwise that would set the result byte to 0 when we use _mm512_shuffle_epi8.
            // Shuffle the low and high nibbles:
            let msb_mak = _mm512_set1_epi8(0x7f);
            let filtered_lower_4 = _mm512_and_si512(self.v0, msb_mak);
            let result_lower_4 = _mm512_shuffle_epi8(low_nibble_mask, filtered_lower_4);
            let filtered_high_4 = _mm512_and_si512(_mm512_srli_epi32::<4>(self.v0), msb_mak);
            let result_high_4 = _mm512_shuffle_epi8(high_nibble_mask, filtered_high_4);

            // And the low and high nibbles to get correct matches:
            let result = _mm512_and_si512(result_high_4, result_lower_4);

            *structurals = _mm512_cmp_epi8_mask(
                _mm512_and_si512(result, structural_shufti_mask),
                _mm512_set1_epi8(0),
                _MM_CMPINT_NE, // Not Equal to zero means it's a structural
            );

            *whitespace = _mm512_cmp_epi8_mask(
                _mm512_and_si512(result, whitespace_shufti_mask),
                _mm512_set1_epi8(0),
                _MM_CMPINT_NE, // Not Equal to zero means it's whitespace
            );
        }
    }

    // flatten out values in 'bits' assuming that they are are to have values of idx
    // plus their position in the bitvector, and store these indexes at
    // base_ptr[base] incrementing base as we go
    // will potentially store extra values beyond end of valid bits, so base_ptr
    // needs to be large enough to handle this
    //TODO: usize was u32 here does this matter?
    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(clippy::cast_possible_wrap, clippy::cast_ptr_alignment)]
    #[target_feature(enable = "avx2")]
    unsafe fn flatten_bits(base: &mut Vec<u32>, idx: u32, mut bits: u64) {
        unsafe {
            let cnt: usize = bits.count_ones() as usize;
            let mut l = base.len();
            let idx_minus_64 = idx.wrapping_sub(64);
            let idx_64_v = _mm256_set_epi32(
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
                static_cast_i32!(idx_minus_64),
            );

            // We're doing some trickery here.
            // We reserve 64 extra entries, because we've at most 64 bit to set
            // then we truncate the base to the next base (that we calculated above)
            // We later indiscriminatory write over the len we set but that's OK
            // since we ensure we reserve the needed space
            base.reserve(64);
            let final_len = l + cnt;

            while bits != 0 {
                let v0 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v1 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v2 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v3 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v4 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v5 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v6 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);
                let v7 = bits.trailing_zeros() as i32;
                bits &= bits.wrapping_sub(1);

                let v: __m256i = _mm256_set_epi32(v7, v6, v5, v4, v3, v2, v1, v0);
                let v: __m256i = _mm256_add_epi32(idx_64_v, v);
                _mm256_storeu_si256(base.as_mut_ptr().add(l).cast::<__m256i>(), v);
                l += 8;
            }
            // We have written all the data
            base.set_len(final_len);
        }
    }

    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[target_feature(enable = "avx2")]
    unsafe fn fill_s8(n: i8) -> __m512i {
        unsafe { _mm512_set1_epi8(n) }
    }
}
