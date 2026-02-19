#![allow(dead_code)]
use crate::charutils::is_not_structural_or_whitespace;
use crate::safer_unchecked::GetSaferUnchecked;
use crate::value::tape::Node;
use crate::{Deserializer, Error, ErrorType, InternalError, Result};
use value_trait::StaticNode;

macro_rules! get {
    ($a:expr_2021, $i:expr_2021) => {{ unsafe { $a.get_kinda_unchecked($i) } }};
}

#[cfg_attr(not(feature = "no-inline"), inline)]
pub fn is_valid_null_atom(loc: &[u8]) -> bool {
    //let nv: u64 = *(b"null   ".as_ptr() as *const u64);
    // this is the same:
    const NV: u64 = 0x00_00_00_00_6c_6c_75_6e;
    const MASK4: u64 = 0x00_00_00_00_ff_ff_ff_ff;

    debug_assert!(loc.len() >= 8, "loc too short for a u64 read");

    // TODO is this expensive?
    let mut error: u64;

    let locval: u64 = unsafe { loc.as_ptr().cast::<u64>().read_unaligned() };

    error = (locval & MASK4) ^ NV;
    error |= u64::from(is_not_structural_or_whitespace(*get!(loc, 4)));

    error == 0
}

#[derive(Debug)]
enum State {
    ObjectKey,
    ScopeEnd,
    MainArraySwitch,
}
#[derive(Debug)]
pub(crate) enum StackState {
    Start,
    Object { last_start: usize, cnt: usize },
    Array { last_start: usize, cnt: usize },
}

impl<'de> Deserializer<'de> {
    #[cfg_attr(not(feature = "no-inline"), inline)]
    #[allow(
        clippy::cognitive_complexity,
        clippy::too_many_lines,
        unused_unsafe,
        clippy::needless_continue
    )]
    pub(crate) fn build_tape(
        input: &'de mut [u8],
        input2: &[u8],
        buffer: &mut [u8],
        structural_indexes: &[u32],
        whitespace_indexes: &[u32],
        stack: &mut Vec<StackState>,
        res: &mut Vec<Node<'de>>,
    ) -> Result<()> {
        println!("structural indexes: {:?}", structural_indexes);
        println!("whitespace indexes: {:?}", whitespace_indexes);
        res.clear();
        res.reserve(structural_indexes.len());
        // While a valid json can have at max len/2 (`[[[]]]`)elements that are relevant
        // a invalid json might exceed this `[[[[[[` and we need to protect against that.
        stack.clear();
        stack.reserve(structural_indexes.len());

        let res_ptr = res.as_mut_ptr();
        let stack_ptr = stack.as_mut_ptr();

        let mut depth: usize = 0;
        let mut last_start;
        let mut cnt: usize;
        let mut r_i = 0;

        // let mut i: usize = 0; // index of the structural character (0,1,2,3...)
        // location of the structural character in the input (buf)
        let mut idx: usize = 0;
        // used to track the (structural) character we are looking at, updated
        // by UPDATE_CHAR macro
        let mut c: u8 = 0;
        // skip the zero index
        let mut i: usize = 0;
        let mut state;

        macro_rules! s2try {
            ($e:expr_2021) => {
                match $e {
                    ::std::result::Result::Ok(val) => val,
                    ::std::result::Result::Err(err) => {
                        // We need to ensure that rust doesn't
                        // try to free strings that we never
                        // allocated
                        unsafe {
                            res.set_len(r_i);
                        };
                        return ::std::result::Result::Err(err);
                    }
                }
            };
        }

        macro_rules! insert_res {
            ($t:expr_2021) => {
                unsafe {
                    res_ptr.add(r_i).write($t);
                    r_i += 1;
                }
            };
        }
        macro_rules! success {
            () => {
                unsafe {
                    res.set_len(r_i);
                }
                return Ok(());
            };
        }
        macro_rules! update_char {
            () => {
                if i < structural_indexes.len() {
                    idx = *get!(structural_indexes, i) as usize;
                    i += 1;
                    c = *get!(input2, idx);
                } else {
                    fail!(ErrorType::Syntax);
                }
            };
        }

        macro_rules! goto {
            ($state:expr_2021) => {{
                state = $state;
                #[allow(clippy::needless_continue)]
                continue;
            }};
        }

        macro_rules! insert_str {
            ($end:expr) => {
                insert_res!(Node::String(s2try!(Self::parse_str_(
                    input.as_mut_ptr(),
                    &input2,
                    buffer,
                    idx,
                    $end
                ))));
            };
        }

        // The continue cases are the most frequently called onces it's
        // worth pulling them out into a macro (aka inlining them)
        // Since we don't have a 'gogo' in rust.
        macro_rules! array_continue {
            () => {{
                update_char!();
                match c {
                    b',' => {
                        cnt += 1;
                        update_char!();
                        goto!(MainArraySwitch);
                    }
                    b']' => {
                        goto!(ScopeEnd);
                    }
                    _c => {
                        fail!(ErrorType::ExpectedArrayContent);
                    }
                }
            }};
        }

        macro_rules! object_continue {
            () => {{
                update_char!();
                match c {
                    b',' => {
                        cnt += 1;
                        update_char!();
                        if c == b'"' {
                            insert_str!(0);
                            goto!(ObjectKey);
                        }
                        fail!(ErrorType::ExpectedObjectKey);
                    }
                    b'}' => {
                        goto!(ScopeEnd);
                    }
                    _ => {
                        fail!(ErrorType::ExpectedObjectContent);
                    }
                }
            }};
        }

        macro_rules! array_begin {
            () => {
                update_char!();
                if c == b']' {
                    cnt = 0;
                    goto!(ScopeEnd);
                }
                goto!(MainArraySwitch);
            };
        }

        macro_rules! object_begin {
            () => {{
                update_char!();
                match c {
                    b'"' => {
                        insert_str!(0);
                        goto!(ObjectKey)
                    }
                    b'}' => {
                        cnt = 0;
                        goto!(ScopeEnd);
                    }
                    _c => {
                        fail!(ErrorType::ExpectedObjectContent);
                    }
                }
            }};
        }

        macro_rules! fail {
            () => {
                // We need to ensure that rust doesn't
                // try to free strings that we never
                // allocated
                unsafe {
                    res.set_len(r_i);
                };
                return Err(Error::new_c(
                    idx,
                    c as char,
                    ErrorType::InternalError(InternalError::TapeError),
                ));
            };
            ($t:expr_2021) => {
                // We need to ensure that rust doesn't
                // try to free strings that we never
                // allocated
                unsafe {
                    res.set_len(r_i);
                };
                return Err(Error::new_c(idx, c as char, $t));
            };
        }
        // top-level
        // structural_indexes[0]: opening `"` of the key token.
        update_char!();
        let key_start = idx; // saved here — idx will advance on the next call

        // structural_indexes[1]: reveals what follows the key.
        update_char!();
        match c {
            b'[' => {
                // Array-notation key: "tags"[3]: "a","b","c"
                // TODO: implement array-notation top-level parsing
                fail!(ErrorType::NoStructure);
            }
            b':' => {
                unsafe { stack_ptr.add(depth).write(StackState::Start) };

                // Object header placeholder; len/count back-filled by ScopeEnd.
                last_start = r_i;
                insert_res!(Node::Object { len: 0, count: 0 });
                depth += 1;
                cnt = 1; // proactively count the one key-value pair we are about to parse

                // Key: from key_start up to (not including) the `:` at idx.
                insert_res!(Node::String(s2try!(unsafe {
                    Self::parse_str_(input.as_mut_ptr(), input2, buffer, key_start, idx)
                })));

                // structural_indexes[2]: first byte of the value token.
                update_char!();

                if c == b'\n' {
                    // Value is on the next line → nested object scope.
                    // TODO: implement nested-object (indented) parsing
                    fail!(ErrorType::NoStructure);
                }

                // Value: from idx up to the next structural (must be `\n`),
                // or to the true document end if no further structural exists.
                let value_start = idx;
                let value_end = if i < structural_indexes.len() {
                    let next = *get!(structural_indexes, i) as usize;
                    if *get!(input2, next) != b'\n' {
                        fail!(ErrorType::NoStructure);
                    }
                    next
                } else {
                    // No trailing newline — value runs to the end of the document.
                    input.len()
                };

                let value_bytes_trimmed = &input2[value_start..value_end];

                // AVX-512BW SIMD classifier: Number / Null / String / Boolean.
                let basic_type =
                    unsafe { crate::impls::avx512bw::classify_bytes_avx512(value_bytes_trimmed) };

                match basic_type {
                    crate::impls::avx512bw::BasicTypes::Number => {
                        let is_negative = *get!(input2, value_start) == b'-';
                        insert_res!(Node::Static(s2try!(Self::parse_number(
                            value_start,
                            input2,
                            is_negative,
                        ))));
                    }
                    crate::impls::avx512bw::BasicTypes::Null => {
                        insert_res!(Node::Static(StaticNode::Null));
                    }
                    crate::impls::avx512bw::BasicTypes::String => {
                        insert_res!(Node::String(s2try!(unsafe {
                            Self::parse_str_(input.as_mut_ptr(), input2, buffer, idx, value_end)
                        })));
                    }
                    crate::impls::avx512bw::BasicTypes::Boolean(b) => {
                        insert_res!(Node::Static(StaticNode::Bool(b)));
                    }
                }

                // ScopeEnd will back-fill the Object header (len, count).
                state = State::ScopeEnd;
            }
            _ => {
                fail!();
            }
        }

        loop {
            use self::State::{MainArraySwitch, ObjectKey, ScopeEnd};
            match state {
                ////////////////////////////// OBJECT STATES /////////////////////////////
                ObjectKey => {
                    update_char!();
                    if unlikely!(c != b':') {
                        fail!(ErrorType::ExpectedObjectColon);
                    }
                    update_char!();
                    match c {
                        b'"' => {
                            insert_str!(0);
                            object_continue!();
                        }
                        b'n' => {
                            insert_res!(Node::Static(StaticNode::Null));
                            if !is_valid_null_atom(get!(input2, idx..)) {
                                fail!(ErrorType::ExpectedNull);
                            }
                            object_continue!();
                        }
                        b'-' => {
                            insert_res!(Node::Static(s2try!(Self::parse_number(
                                idx, input2, true
                            ))));

                            object_continue!();
                        }
                        b'0'..=b'9' => {
                            insert_res!(Node::Static(s2try!(Self::parse_number(
                                idx, input2, false
                            ))));

                            object_continue!();
                        }
                        b'{' => {
                            unsafe {
                                stack_ptr
                                    .add(depth)
                                    .write(StackState::Object { last_start, cnt });
                            }
                            last_start = r_i;
                            insert_res!(Node::Object { len: 0, count: 0 });
                            depth += 1;
                            cnt = 1;
                            object_begin!();
                        }
                        b'[' => {
                            unsafe {
                                stack_ptr
                                    .add(depth)
                                    .write(StackState::Object { last_start, cnt });
                            }
                            last_start = r_i;
                            insert_res!(Node::Array { len: 0, count: 0 });
                            depth += 1;
                            cnt = 1;
                            array_begin!();
                        }
                        _c => {
                            fail!();
                        }
                    }
                }
                ////////////////////////////// COMMON STATE /////////////////////////////
                ScopeEnd => {
                    if depth == 0 {
                        fail!(ErrorType::Syntax);
                    }
                    depth -= 1;
                    unsafe {
                        match *res_ptr.add(last_start) {
                            Node::Array {
                                ref mut len,
                                count: ref mut end,
                            }
                            | Node::Object {
                                ref mut len,
                                count: ref mut end,
                            } => {
                                *len = cnt;
                                *end = r_i - last_start - 1;
                            }
                            _ => unreachable!("scop end needs to be an array or object"),
                        }
                    }
                    unsafe {
                        match *stack_ptr.add(depth) {
                            StackState::Object {
                                last_start: l,
                                cnt: c,
                            } => {
                                last_start = l;
                                cnt = c;
                                object_continue!();
                            }
                            StackState::Array {
                                last_start: l,
                                cnt: c,
                            } => {
                                last_start = l;
                                cnt = c;
                                array_continue!();
                            }
                            StackState::Start => {
                                if i == structural_indexes.len() {
                                    success!();
                                }
                                fail!();
                            }
                        };
                    }
                }

                ////////////////////////////// ARRAY STATES /////////////////////////////
                MainArraySwitch => {
                    // we call update char on all paths in, so we can peek at c on the
                    // on paths that can accept a close square brace (post-, and at start)
                    match c {
                        b'"' => {
                            insert_str!(0);
                            array_continue!();
                        }
                        b'n' => {
                            insert_res!(Node::Static(StaticNode::Null));
                            if !is_valid_null_atom(get!(input2, idx..)) {
                                fail!(ErrorType::ExpectedNull);
                            }
                            array_continue!();
                        }
                        b'-' => {
                            insert_res!(Node::Static(s2try!(Self::parse_number(
                                idx, input2, true
                            ))));

                            array_continue!();
                        }
                        b'0'..=b'9' => {
                            insert_res!(Node::Static(s2try!(Self::parse_number(
                                idx, input2, false
                            ))));

                            array_continue!();
                        }
                        b'{' => {
                            unsafe {
                                stack_ptr
                                    .add(depth)
                                    .write(StackState::Array { last_start, cnt });
                            }
                            last_start = r_i;
                            insert_res!(Node::Object { len: 0, count: 0 });
                            depth += 1;
                            cnt = 1;
                            object_begin!();
                        }
                        b'[' => {
                            unsafe {
                                stack_ptr
                                    .add(depth)
                                    .write(StackState::Array { last_start, cnt });
                            }
                            last_start = r_i;
                            insert_res!(Node::Array { len: 0, count: 0 });
                            depth += 1;
                            cnt = 1;
                            array_begin!();
                        }
                        _c => {
                            fail!();
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::SIMDJSON_PADDING;

    use super::*;

    #[test]
    fn null_atom() {
        assert!(is_valid_null_atom(b"null    "));
        assert!(!is_valid_null_atom(b"nul     "));
        assert!(!is_valid_null_atom(b" ull    "));
    }

    #[cfg(feature = "serde_impl")]
    #[test]
    fn parsing_errors() {
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut b"time".to_vec()),
            Err(Error::new_c(0, 't', ErrorType::ExpectedTrue))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut b"falsy".to_vec()),
            Err(Error::new_c(0, 'f', ErrorType::ExpectedFalse))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut b"new".to_vec()),
            Err(Error::new_c(0, 'n', ErrorType::ExpectedNull))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut b"[true, time]".to_vec()),
            Err(Error::new_c(7, 't', ErrorType::ExpectedTrue))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut b"[true, falsy]".to_vec()),
            Err(Error::new_c(7, 'f', ErrorType::ExpectedFalse))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut b"[null, new]".to_vec()),
            Err(Error::new_c(7, 'n', ErrorType::ExpectedNull))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut br#"{"1":time}"#.to_vec()),
            Err(Error::new_c(5, 't', ErrorType::ExpectedTrue))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut br#"{"0":falsy}"#.to_vec()),
            Err(Error::new_c(5, 'f', ErrorType::ExpectedFalse))
        );
        assert_eq!(
            crate::serde::from_slice::<bool>(&mut br#"{"0":new}"#.to_vec()),
            Err(Error::new_c(5, 'n', ErrorType::ExpectedNull))
        );
    }

    #[test]
    fn parse_string() -> Result<()> {
        let mut input = Vec::from(&br#""{\"arg\":\"test\"}""#[..]);
        let mut input2 = input.clone();
        input2.append(vec![0; SIMDJSON_PADDING * 2].as_mut());
        let mut buffer = vec![0; 1024];

        let s = unsafe {
            Deserializer::parse_str_(input.as_mut_ptr(), &input2, buffer.as_mut_slice(), 0, 0)?
        };
        assert_eq!(r#"{"arg":"test"}"#, s);
        Ok(())
    }
}
