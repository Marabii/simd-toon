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

        // Current nesting level of arrays/objects.
        // Example: parsing the equivalent of {"a":[1]} in TOON goes depth 0 -> 1 (object) -> 2 (array).
        let mut depth: usize = 0;
        // Tape slot where the current container (Node::Object / Node::Array) started.
        // Example: if '{' starts at tape index 7, last_start = 7 until the matching '}'.
        let mut last_start;
        // Number of entries seen in the current container.
        // Example: for array[3]: 10,20,30 cnt becomes 3.
        let mut cnt: usize;
        // Write cursor into `res` (the tape under construction).
        // Example: after writing three nodes, r_i == 3.
        let mut r_i = 0;

        // Byte offset in the input buffer for the current structural token.
        // Example: in name: Hamza idx can point to 'n' ':' 'H' ('n' and 'H' because they are the first character of the token)
        let mut idx: usize = 0;
        // Structural byte currently being handled (read from input2[idx]).
        // Example: c == b'{' when entering an object, c == b',' between values.
        let mut c: u8 = 0;
        // Cursor into `structural_indexes`.
        // Example: i == 5 means the next update_char!() reads structural_indexes[5].
        let mut i: usize = 0;
        // Current state of the stage-2 state machine.
        // Example: State::ObjectKey means the parser currently expects an object key.
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

        macro_rules! get_value_end {
            ($start:expr, $delim:expr) => {{
                // Find the hard boundary
                let mut end = if c == b':' {
                    idx
                } else {
                    if i < structural_indexes.len() {
                        let next = *get!(structural_indexes, i) as usize;
                        let next_c = *get!(input2, next);
                        if next_c != b'\n' && next_c != $delim {
                            fail!(ErrorType::NoStructure);
                        }
                        next
                    } else {
                        input.len()
                    }
                };

                // Right-to-Left Trim
                while end > $start {
                    let prev_char = *get!(input2, end - 1);
                    if prev_char == b' ' {
                        end -= 1;
                    } else {
                        break;
                    }
                }

                end
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
            b'[' | b':' => {
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

                    if c == b'\n' || c == b':' {
                        fail!(ErrorType::ExpectedObjectKey);
                    }
                    if c == b'[' {
                        fail!(ErrorType::NoStructure);
                    }

                    let key_start = idx;
                    // Trim trailing spaces from key name (key ends where '[' begins)
                    let key_end = get_value_end!(key_start, b'[');

                    update_char!();
                    if c == b'[' {
                        // Array key: key[N]: val1,val2,...

                        // Parse [N]
                        update_char!();
                        let digit_start = idx;
                        update_char!();
                        if c != b']' {
                            fail!(ErrorType::InvalidArrayHeader);
                        }
                        let digit_end = idx;
                        let declared_len =
                            s2try!(Self::parse_array_len(input2, digit_start, digit_end));

                        // Expect ':'
                        update_char!();
                        if c != b':' {
                            fail!(ErrorType::InvalidArrayHeader);
                        }

                        cnt += 1;
                        insert_str!(key_start, key_end);

                        // Empty array: emit immediately
                        if declared_len == 0 {
                            insert_res!(Node::Array { len: 0, count: 0 });
                            if i >= structural_indexes.len() {
                                goto!(State::ScopeEnd);
                            }
                            update_char!();
                            if c != b'\n' {
                                fail!(ErrorType::Syntax);
                            }
                            let newline_idx = idx;
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

                        // Peek ahead to distinguish inline from block
                        if i >= structural_indexes.len() {
                            fail!(ErrorType::ExpectedArrayContent);
                        }
                        let peek_c = *get!(input2, *get!(structural_indexes, i) as usize);
                        if peek_c == b'\n' {
                            // Block arrays not yet implemented
                            fail!(ErrorType::NoStructure);
                        }

                        // Inline array: comma-separated values on this line
                        let array_tape_start = r_i;
                        insert_res!(Node::Array {
                            len: declared_len,
                            count: 0
                        });

                        let mut elem_count: usize = 0;
                        loop {
                            update_char!();
                            let value_start = idx;
                            let value_end = get_value_end!(value_start, b',');

                            let value_bytes = &input2[value_start..value_end];
                            let basic_type = unsafe {
                                crate::impls::avx512bw::classify_bytes_avx512(value_bytes)
                            };
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
                            elem_count += 1;

                            // EOF with no trailing newline — array ends here
                            if i >= structural_indexes.len() {
                                break;
                            }
                            update_char!();
                            if c == b',' {
                                // continue to next element
                            } else if c == b'\n' {
                                break;
                            } else {
                                fail!(ErrorType::Syntax);
                            }
                        }

                        // Backfill Array count
                        unsafe {
                            match *res_ptr.add(array_tape_start) {
                                Node::Array { ref mut count, .. } => {
                                    *count = r_i - array_tape_start - 1;
                                }
                                _ => unreachable!("array backfill expects an array node"),
                            }
                        }

                        if elem_count != declared_len {
                            fail!(ErrorType::ArrayCountMismatch);
                        }

                        // Scope continuation — idx now holds the '\n' position
                        let newline_idx = idx;
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
                    } else if c != b':' {
                        fail!(ErrorType::ExpectedObjectColon);
                    }

                    cnt += 1;
                    let key_end = get_value_end!(key_start, b'\n');
                    insert_str!(key_start, key_end);

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
                    let value_end = get_value_end!(value_start, b'\n');

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

    /// Parse an ASCII decimal number from `input2[start..end]`.
    /// Returns `Err(InvalidArrayHeader)` if the slice is empty or contains non-digit bytes.
    fn parse_array_len(input2: &[u8], start: usize, end: usize) -> Result<usize> {
        if start >= end {
            return Err(Error::generic(ErrorType::InvalidArrayHeader));
        }
        let mut n: usize = 0;
        for (pos, &b) in input2[start..end].iter().enumerate() {
            if !b.is_ascii_digit() {
                return Err(Error::new_c(
                    start + pos,
                    b as char,
                    ErrorType::InvalidArrayHeader,
                ));
            }
            n = n * 10 + (b - b'0') as usize;
        }
        Ok(n)
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
