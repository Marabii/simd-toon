#![allow(dead_code)]
use crate::safer_unchecked::GetSaferUnchecked;
use crate::value::tape::Node;
use crate::{Deserializer, Error, ErrorType, InternalError, Result};
use value_trait::StaticNode;

macro_rules! get {
    ($a:expr_2021, $i:expr_2021) => {{ unsafe { $a.get_kinda_unchecked($i) } }};
}

#[derive(Debug)]
enum State {
    ObjectKey,
    ScopeEnd,
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
        _whitespace_indexes: &[u32],
        stack: &mut Vec<StackState>,
        res: &mut Vec<Node<'de>>,
    ) -> Result<()> {
        res.clear();
        res.reserve(structural_indexes.len());
        stack.clear();
        stack.reserve(structural_indexes.len());

        let res_ptr = res.as_mut_ptr();
        let stack_ptr = stack.as_mut_ptr();

        let mut depth: usize = 0;
        let mut last_start: usize;
        let mut cnt: usize;
        let mut r_i: usize = 0;
        let mut idx: usize = 0;
        let mut c: u8 = 0;
        let mut i: usize = 0;
        let mut state: State;

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
            ($start:expr, $end:expr) => {
                insert_res!(Node::String(s2try!(Self::parse_str_(
                    input.as_mut_ptr(),
                    input2,
                    buffer,
                    $start,
                    $end
                ))));
            };

            ($end:expr) => {
                insert_res!(Node::String(s2try!(Self::parse_str_(
                    input.as_mut_ptr(),
                    input2,
                    buffer,
                    idx,
                    $end
                ))));
            };
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

        if structural_indexes.len() < 2 {
            fail!(ErrorType::NoStructure);
        }

        // This is for peeking not for consumption.
        idx = *get!(structural_indexes, 0) as usize;
        c = *get!(input2, idx);

        if c == b'\n' {
            fail!(ErrorType::ExpectedObjectKey);
        }

        let second_idx = *get!(structural_indexes, 1) as usize;
        idx = second_idx;
        c = *get!(input2, second_idx);

        match c {
            b'[' => {
                // Array-notation key: tags[3]: a,b,c
                // TODO: implement array-notation top-level parsing
                fail!(ErrorType::NoStructure);
            }
            b':' => {
                unsafe { stack_ptr.add(depth).write(StackState::Start) };
                last_start = r_i;
                insert_res!(Node::Object { len: 0, count: 0 });
                depth += 1;
                cnt = 0;
                state = State::ObjectKey;
            }
            _ => {
                fail!();
            }
        }

        loop {
            match state {
                State::ObjectKey => {
                    update_char!();

                    println!("chat is: {}", c as char);

                    if c == b'\n' || c == b':' {
                        fail!(ErrorType::ExpectedObjectKey);
                    }
                    if c == b'[' {
                        fail!(ErrorType::NoStructure);
                    }

                    let key_start = idx;

                    update_char!();
                    if c != b':' {
                        fail!(ErrorType::ExpectedObjectColon);
                    }

                    cnt += 1;
                    insert_str!(key_start, idx);

                    update_char!();

                    if c == b'\n' {
                        let newline_idx = idx;
                        if i >= structural_indexes.len() {
                            insert_res!(Node::Static(StaticNode::Null));
                            goto!(State::ScopeEnd);
                        }

                        let next_idx = *get!(structural_indexes, i) as usize;
                        let next_c = *get!(input2, next_idx);
                        if next_c == b'\n' {
                            fail!(ErrorType::Syntax);
                        }

                        let actual_ws = next_idx - newline_idx - 1;
                        let sibling_ws = (depth - 1) * 2;
                        let child_ws = depth * 2;

                        if actual_ws == sibling_ws {
                            insert_res!(Node::Static(StaticNode::Null));
                            goto!(State::ObjectKey);
                        }

                        if actual_ws == child_ws {
                            unsafe {
                                stack_ptr
                                    .add(depth)
                                    .write(StackState::Object { last_start, cnt });
                            }
                            last_start = r_i;
                            insert_res!(Node::Object { len: 0, count: 0 });
                            depth += 1;
                            cnt = 0;
                            goto!(State::ObjectKey);
                        }

                        if actual_ws < sibling_ws && actual_ws.is_multiple_of(2) {
                            insert_res!(Node::Static(StaticNode::Null));
                            goto!(State::ScopeEnd);
                        }

                        fail!(ErrorType::InvalidIndentation);
                    }

                    let value_start = idx;
                    let value_end = if i < structural_indexes.len() {
                        let next = *get!(structural_indexes, i) as usize;
                        if *get!(input2, next) != b'\n' {
                            fail!(ErrorType::NoStructure);
                        }
                        next
                    } else {
                        input.len()
                    };

                    let value_bytes = &input2[value_start..value_end];
                    let basic_type =
                        unsafe { crate::impls::avx512bw::classify_bytes_avx512(value_bytes) };

                    match basic_type {
                        crate::impls::avx512bw::BasicTypes::Number => {
                            let is_negative = *get!(input2, value_start) == b'-';
                            insert_res!(Node::Static(s2try!(Self::parse_number(
                                value_start,
                                input2,
                                is_negative
                            ))));
                        }
                        crate::impls::avx512bw::BasicTypes::String => {
                            insert_str!(value_start, value_end);
                        }
                        crate::impls::avx512bw::BasicTypes::Boolean(b) => {
                            insert_res!(Node::Static(StaticNode::Bool(b)));
                        }
                    }

                    if i >= structural_indexes.len() {
                        goto!(State::ScopeEnd);
                    }

                    let newline_idx = *get!(structural_indexes, i) as usize;
                    if *get!(input2, newline_idx) != b'\n' {
                        fail!(ErrorType::NoStructure);
                    }
                    i += 1;

                    if i >= structural_indexes.len() {
                        goto!(State::ScopeEnd);
                    }

                    let next_idx = *get!(structural_indexes, i) as usize;
                    let next_c = *get!(input2, next_idx);
                    if next_c == b'\n' {
                        fail!(ErrorType::Syntax);
                    }

                    let actual_ws = next_idx - newline_idx - 1;
                    let sibling_ws = (depth - 1) * 2;

                    if actual_ws == sibling_ws {
                        goto!(State::ObjectKey);
                    }
                    if actual_ws < sibling_ws && actual_ws.is_multiple_of(2) {
                        goto!(State::ScopeEnd);
                    }

                    fail!(ErrorType::InvalidIndentation);
                }

                State::ScopeEnd => {
                    if depth == 0 {
                        fail!(ErrorType::Syntax);
                    }

                    depth -= 1;

                    unsafe {
                        match *res_ptr.add(last_start) {
                            Node::Object {
                                ref mut len,
                                count: ref mut end,
                            } => {
                                *len = cnt;
                                *end = r_i - last_start - 1;
                            }
                            _ => unreachable!("scope end expects an object"),
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

                                if i >= structural_indexes.len() {
                                    goto!(State::ScopeEnd);
                                }

                                let next_idx = *get!(structural_indexes, i) as usize;
                                if *get!(input2, next_idx) == b'\n' {
                                    fail!(ErrorType::Syntax);
                                }

                                if i == 0 {
                                    fail!(ErrorType::Syntax);
                                }

                                let prev_idx = *get!(structural_indexes, i - 1) as usize;
                                if *get!(input2, prev_idx) != b'\n' {
                                    fail!(ErrorType::Syntax);
                                }

                                let actual_ws = next_idx - prev_idx - 1;
                                let expected_ws = (depth - 1) * 2;

                                if actual_ws == expected_ws {
                                    goto!(State::ObjectKey);
                                }
                                if actual_ws < expected_ws && actual_ws.is_multiple_of(2) {
                                    goto!(State::ScopeEnd);
                                }

                                fail!(ErrorType::InvalidIndentation);
                            }

                            StackState::Start => {
                                // Skip any trailing `\n` structurals (line terminators).
                                while i < structural_indexes.len()
                                    && *get!(input2, *get!(structural_indexes, i) as usize) == b'\n'
                                {
                                    i += 1;
                                }
                                if i == structural_indexes.len() {
                                    success!();
                                }
                                fail!();
                            }
                            StackState::Array { .. } => {
                                fail!(ErrorType::NoStructure);
                            }
                        };
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
