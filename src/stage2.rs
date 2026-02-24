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
    SingleValue,

    ArrayRouter(usize),

    // tags[3]: a,b,c
    ParseInlineArray(usize),
    // key[N]{f1,f2,...}:\n rows
    ParseTabularArray(usize),
    // Block Array: key[N]:\n  - item
    ParseBlockArray(usize),
    ParseBlockArrayItem,

    CheckIndentation(usize),
}
#[derive(Debug)]
pub(crate) enum StackState {
    Start,
    Object {
        last_start: usize,
        cnt: usize,
    },
    Array {
        parent_last_start: usize,
        parent_cnt: usize,
        tape_start: usize,
        declared_len: usize,
        elem_cnt: usize,
        compact_indent: bool,
        is_root: bool,
    },
    ArrayItem,
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
        let mut last_start: usize = 0;
        // Number of entries seen in the current container.
        // Example: for array[3]: 10,20,30 cnt becomes 3.
        let mut cnt: usize = 0;
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

        // Accumulator for tracking visual indentation shifts (Compact Arrays / Root Arrays)
        let mut indent_modifier: isize = 0;

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

        macro_rules! peek_value_end {
            ($start:expr, $err:expr, $($expected_delim:expr),+) => {{
                let mut hard_end = input.len();

                // Advance through structural indexes looking for delimiters or newlines
                while i < structural_indexes.len() {
                    let pos = *get!(structural_indexes, i) as usize;
                    let c = *get!(input2, pos);

                    // If we find ANY of the expected delimiters, we found our boundary
                    if $(c == $expected_delim)||* {
                        hard_end = pos;
                        break;
                    }

                    // If we encounter a newline before a delimiter, it's a failure
                    if c == b'\n' {
                        fail!($err);
                    }

                    // Keep searching forward
                    i += 1;
                }

                trim_trailing_spaces!($start, hard_end)
            }};
        }

        macro_rules! trim_trailing_spaces {
            ($start:expr, $hard_end:expr) => {{
                let mut end = $hard_end;
                while end > $start {
                    let prev_char = *get!(input2, end - 1);
                    if prev_char == b' ' || prev_char == b'\r' || prev_char == b'\t' {
                        end -= 1;
                    } else {
                        break;
                    }
                }
                end
            }};
        }

        // Currently simd-toon only works on CPUs supporting AVX512BW, I'll work on other simd srchitectures in the future.
        macro_rules! parse_and_insert_value {
            ($start:expr, $end:expr) => {
                let value_bytes = &input2[$start..$end];
                let basic_type =
                    unsafe { crate::impls::avx512bw::classify_bytes_avx512(value_bytes) };
                match basic_type {
                    crate::impls::avx512bw::BasicTypes::Number => {
                        let is_negative = *get!(input2, $start) == b'-';
                        insert_res!(Node::Static(s2try!(Self::parse_number(
                            $start,
                            input2,
                            is_negative,
                        ))));
                    }
                    crate::impls::avx512bw::BasicTypes::String => {
                        insert_str!($start, $end);
                    }
                    crate::impls::avx512bw::BasicTypes::Boolean(b) => {
                        insert_res!(Node::Static(StaticNode::Bool(b)));
                    }
                    crate::impls::avx512bw::BasicTypes::Null => {
                        insert_res!(Node::Static(StaticNode::Null));
                    }
                }
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

        macro_rules! skip_newline_chars {
            () => {{
                for &idx in &structural_indexes[i..] {
                    let trailing_idx = idx as usize;
                    unsafe {
                        if *input2.get_unchecked(trailing_idx) == b'\n' {
                            i += 1;
                        } else {
                            fail!(ErrorType::Syntax);
                        }
                    }
                }
                let _ = i;
                success!();
            }};
        }

        if unlikely!(structural_indexes.len() < 2) {
            fail!(ErrorType::NoStructure);
        }

        // This is for peeking not for consumption.
        idx = *get!(structural_indexes, 0) as usize;
        c = *get!(input2, idx);

        if unlikely!(c == b'\n') {
            fail!(ErrorType::ExpectedObjectKey);
        }

        if c == b'[' {
            i = 1;

            update_char!();
            let digit_start = idx;
            update_char!();
            if unlikely!(c != b']') {
                fail!(ErrorType::InvalidArrayHeader);
            }
            let digit_end = idx;
            let declared_len = s2try!(Self::parse_array_len(input2, digit_start, digit_end));

            update_char!();
            state = State::ArrayRouter(declared_len);
        } else {
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
        }

        loop {
            match state {
                State::ObjectKey => {
                    update_char!();

                    if c == b'\n' || c == b':' || c == b'[' {
                        fail!(ErrorType::ExpectedObjectKey);
                    }

                    let key_start = idx;
                    update_char!();

                    if c == b':' {
                        // Standard Key 'name: Hamza'
                        cnt += 1;
                        let key_end = trim_trailing_spaces!(key_start, idx);
                        insert_str!(key_start, key_end);

                        goto!(State::SingleValue);
                    } else if c == b'[' {
                        // Array key: key[N]: val1,val2,...
                        let key_end = trim_trailing_spaces!(key_start, idx);

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

                        // After ']': expect '{' (tabular) or ':' (inline/block)
                        update_char!();
                        cnt += 1;
                        insert_str!(key_start, key_end);

                        goto!(State::ArrayRouter(declared_len));
                    } else {
                        fail!(ErrorType::Syntax);
                    }
                }

                State::ArrayRouter(declared_len) => {
                    if c == b'{' {
                        goto!(State::ParseTabularArray(declared_len));
                    } else if c == b':' {
                        if declared_len == 0 {
                            insert_res!(Node::Array { len: 0, count: 0 });

                            if i >= structural_indexes.len() {
                                if depth == 0 {
                                    success!();
                                }
                                goto!(State::ScopeEnd);
                            }

                            update_char!();
                            if c != b'\n' {
                                fail!(ErrorType::Syntax);
                            }

                            if depth == 0 {
                                skip_newline_chars!();
                            }

                            // update_char! consumed the newline. Back up!
                            i -= 1;
                            goto!(State::CheckIndentation(idx));
                        } else {
                            if i >= structural_indexes.len() {
                                fail!(ErrorType::ExpectedArrayContent);
                            }
                            let next_idx = *get!(structural_indexes, i) as usize;
                            let next_c = *get!(input2, next_idx);

                            if next_c == b'\n' {
                                goto!(State::ParseBlockArray(declared_len));
                            } else {
                                goto!(State::ParseInlineArray(declared_len));
                            }
                        }
                    } else {
                        fail!(ErrorType::InvalidArrayHeader);
                    }
                }

                State::ParseBlockArray(declared_len) => {
                    update_char!();
                    if c != b'\n' {
                        fail!(ErrorType::Syntax);
                    }

                    let array_tape_start = r_i;
                    insert_res!(Node::Array {
                        len: declared_len,
                        count: 0,
                    });

                    let compact_indent = if depth == 0 {
                        false
                    } else {
                        unsafe {
                            matches!(*stack_ptr.add(depth - 1), StackState::ArrayItem) && cnt == 1
                        }
                    };

                    // shift visual indent expectations back 2 spaces
                    if compact_indent {
                        indent_modifier -= 2;
                    }

                    unsafe {
                        stack_ptr.add(depth).write(StackState::Array {
                            parent_last_start: last_start,
                            parent_cnt: cnt,
                            tape_start: array_tape_start,
                            declared_len,
                            elem_cnt: 0,
                            compact_indent,
                            is_root: depth == 0,
                        });
                    }
                    depth += 1;

                    goto!(State::ParseBlockArrayItem);
                }

                State::ParseBlockArrayItem => {
                    if i >= structural_indexes.len() {
                        fail!(ErrorType::ExpectedArrayContent);
                    }

                    update_char!();
                    if c != b'-' {
                        fail!(ErrorType::InvalidListMarker);
                    }

                    if i >= structural_indexes.len() {
                        fail!(ErrorType::ExpectedArrayContent);
                    }

                    update_char!();
                    let value_start = idx;

                    let is_object_item = if i < structural_indexes.len() {
                        let next_idx = *get!(structural_indexes, i) as usize;
                        let next_c = *get!(input2, next_idx);
                        next_c == b':' || next_c == b'['
                    } else {
                        false
                    };

                    let is_root_array = unsafe {
                        match *stack_ptr.add(depth - 1) {
                            StackState::Array { is_root, .. } => is_root,
                            _ => false,
                        }
                    };

                    if is_object_item {
                        // Rewind so ObjectKey can consume the first key token naturally.
                        i -= 1;
                        unsafe { stack_ptr.add(depth).write(StackState::ArrayItem) };

                        // Root Array objects are visually indented 2 extra spaces
                        if is_root_array {
                            indent_modifier += 2;
                        }

                        last_start = r_i;
                        insert_res!(Node::Object { len: 0, count: 0 });
                        depth += 1;
                        cnt = 0;
                        goto!(State::ObjectKey);
                    }

                    let value_end = peek_value_end!(value_start, ErrorType::Syntax, b'\n');
                    parse_and_insert_value!(value_start, value_end);

                    let (
                        parent_last_start,
                        parent_cnt,
                        tape_start,
                        declared_len,
                        mut elem_cnt,
                        compact_indent,
                        is_root,
                    ) = unsafe {
                        match *stack_ptr.add(depth - 1) {
                            StackState::Array {
                                parent_last_start,
                                parent_cnt,
                                tape_start,
                                declared_len,
                                elem_cnt,
                                compact_indent,
                                is_root,
                            } => (
                                parent_last_start,
                                parent_cnt,
                                tape_start,
                                declared_len,
                                elem_cnt,
                                compact_indent,
                                is_root,
                            ),
                            _ => {
                                fail!(ErrorType::NoStructure);
                            }
                        }
                    };

                    elem_cnt += 1;
                    unsafe {
                        stack_ptr.add(depth - 1).write(StackState::Array {
                            parent_last_start,
                            parent_cnt,
                            tape_start,
                            declared_len,
                            elem_cnt,
                            compact_indent,
                            is_root,
                        });
                    }

                    if elem_cnt > declared_len {
                        fail!(ErrorType::ArrayCountMismatch);
                    }

                    if elem_cnt == declared_len {
                        unsafe {
                            match *res_ptr.add(tape_start) {
                                Node::Array { ref mut count, .. } => {
                                    *count = r_i - tape_start - 1;
                                }
                                _ => unreachable!("array backfill expects Array node"),
                            }
                        }

                        depth -= 1;
                        last_start = parent_last_start;
                        cnt = parent_cnt;

                        // Restore array reduction upon exit
                        if compact_indent {
                            indent_modifier += 2;
                        }

                        if i >= structural_indexes.len() {
                            if depth == 0 {
                                success!();
                            }
                            goto!(State::ScopeEnd);
                        }

                        if depth == 0 {
                            skip_newline_chars!();
                        }

                        let newline_idx = *get!(structural_indexes, i) as usize;
                        goto!(State::CheckIndentation(newline_idx));
                    }

                    if i >= structural_indexes.len() {
                        fail!(ErrorType::ArrayCountMismatch);
                    }

                    update_char!();
                    if c != b'\n' {
                        fail!(ErrorType::Syntax);
                    }
                    let newline_idx = idx;

                    if i >= structural_indexes.len() {
                        fail!(ErrorType::ArrayCountMismatch);
                    }

                    let next_idx = *get!(structural_indexes, i) as usize;
                    let next_c = *get!(input2, next_idx);
                    if next_c == b'\n' {
                        fail!(ErrorType::BlankLineInBlock);
                    }

                    let actual_ws = next_idx - newline_idx - 1;

                    let base_marker = if depth == 1 { 2 } else { (depth - 1) * 2 } as isize;
                    let marker_ws = (base_marker + indent_modifier) as usize;

                    if actual_ws < marker_ws && actual_ws.is_multiple_of(2) {
                        fail!(ErrorType::ArrayCountMismatch);
                    }
                    if actual_ws != marker_ws {
                        fail!(ErrorType::InvalidIndentation);
                    }
                    if next_c != b'-' {
                        fail!(ErrorType::InvalidListMarker);
                    }

                    goto!(State::ParseBlockArrayItem);
                }

                State::ParseTabularArray(declared_len) => {
                    // Parse field names from {f1,f2,...}
                    let mut fields: Vec<(usize, usize)> = Vec::new();
                    loop {
                        update_char!();
                        if c == b'}' {
                            break;
                        }

                        let field_start = idx;
                        // A field must end with a comma or a closing brace
                        let field_end =
                            peek_value_end!(field_start, ErrorType::InvalidArrayHeader, b',', b'}');
                        fields.push((field_start, field_end));

                        // Consume the ',' or '}' we just peeked at
                        update_char!();
                        if c == b'}' {
                            break;
                        }
                        // else c == b',' → continue
                    }

                    // Expect ':'
                    update_char!();
                    if c != b':' {
                        fail!(ErrorType::InvalidArrayHeader);
                    }

                    let num_fields = fields.len();

                    // Empty tabular array
                    if declared_len == 0 {
                        insert_res!(Node::Array { len: 0, count: 0 });
                        if i >= structural_indexes.len() {
                            goto!(State::ScopeEnd);
                        }

                        // Expect '\n' after the tabular header's ':'
                        update_char!();
                        if c != b'\n' {
                            fail!(ErrorType::Syntax);
                        }

                        // update_char! consumed the newline. Back up!
                        i -= 1;
                        goto!(State::CheckIndentation(idx));
                    }

                    // Expect '\n' before rows
                    update_char!();
                    if c != b'\n' {
                        fail!(ErrorType::Syntax);
                    }

                    let array_tape_start = r_i;
                    insert_res!(Node::Array {
                        len: declared_len,
                        count: 0
                    });

                    for _row_idx in 0..declared_len {
                        update_char!();

                        let row_tape_start = r_i;
                        insert_res!(Node::Object {
                            len: num_fields,
                            count: 0
                        });

                        for (fi, &(fname_start, fname_end)) in fields.iter().enumerate() {
                            // Emit field name
                            insert_str!(fname_start, fname_end);

                            let value_start = idx;
                            // A tabular value must end with a comma, or a newline (end of row)
                            let value_end =
                                peek_value_end!(value_start, ErrorType::Syntax, b',', b'\n');

                            parse_and_insert_value!(value_start, value_end);

                            if fi < num_fields - 1 {
                                // Consume ',' then advance to next value start
                                update_char!();
                                if c != b',' {
                                    fail!(ErrorType::Syntax);
                                }
                                update_char!();
                            } else if i < structural_indexes.len() {
                                update_char!();
                                if c != b'\n' {
                                    fail!(ErrorType::Syntax);
                                }
                            }
                        }

                        // Backfill row Object count
                        unsafe {
                            match *res_ptr.add(row_tape_start) {
                                Node::Object { ref mut count, .. } => {
                                    *count = r_i - row_tape_start - 1;
                                }
                                _ => unreachable!("tabular row backfill expects Object node"),
                            }
                        }
                    }

                    // Backfill Array count
                    unsafe {
                        match *res_ptr.add(array_tape_start) {
                            Node::Array { ref mut count, .. } => {
                                *count = r_i - array_tape_start - 1;
                            }
                            _ => unreachable!("array backfill expects Array node"),
                        }
                    }

                    // Scope continuation
                    if i >= structural_indexes.len() {
                        if depth == 0 {
                            success!();
                        }
                        goto!(State::ScopeEnd);
                    }

                    if depth == 0 {
                        // Root tabular array: accept only trailing newlines.
                        if unlikely!(i == 0) {
                            fail!(ErrorType::Syntax);
                        }
                        i -= 1;
                        skip_newline_chars!();
                    }

                    // The inner loop consumed the final '\n' of the last row. Back up!
                    i -= 1;
                    let newline_idx = *get!(structural_indexes, i) as usize;
                    goto!(State::CheckIndentation(newline_idx));
                }

                State::ParseInlineArray(declared_len) => {
                    let array_tape_start = r_i;
                    insert_res!(Node::Array {
                        len: declared_len,
                        count: 0,
                    });

                    let mut elem_count: usize = 0;
                    loop {
                        update_char!();
                        let value_start = idx;
                        let value_end =
                            peek_value_end!(value_start, ErrorType::Syntax, b',', b'\n');

                        parse_and_insert_value!(value_start, value_end);
                        elem_count += 1;

                        if i >= structural_indexes.len() {
                            break;
                        }
                        update_char!();
                        if c == b',' {
                            // continue
                        } else if c == b'\n' {
                            // We consumed the newline. Back up!
                            i -= 1;
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

                    if unlikely!(elem_count != declared_len) {
                        fail!(ErrorType::ArrayCountMismatch);
                    }
                    if i >= structural_indexes.len() {
                        if depth == 0 {
                            success!();
                        }
                        goto!(State::ScopeEnd);
                    }

                    if depth == 0 {
                        skip_newline_chars!();
                    }

                    let newline_idx = *get!(structural_indexes, i) as usize;
                    goto!(State::CheckIndentation(newline_idx));
                }

                State::SingleValue => {
                    update_char!();

                    if c == b'\n' {
                        // nested object or null
                        let newline_idx = idx;

                        if i >= structural_indexes.len() {
                            insert_res!(Node::Object { len: 0, count: 0 });
                            goto!(State::ScopeEnd);
                        }

                        let next_idx = *get!(structural_indexes, i) as usize;
                        if *get!(input2, next_idx) == b'\n' {
                            fail!(ErrorType::Syntax);
                        }

                        let actual_ws = next_idx - newline_idx - 1;

                        // Clean math replacing the `object_expected_ws!` macro
                        let child_ws = ((depth * 2) as isize + indent_modifier) as usize;

                        if actual_ws == child_ws {
                            // Push scope
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
                        } else {
                            // It wasn't a child object, just a null value
                            insert_res!(Node::Static(StaticNode::Null));

                            // pushed `i` past the newline. Back up!
                            i -= 1;
                            goto!(State::CheckIndentation(newline_idx));
                        }
                    } else {
                        // Primitive value
                        let value_start = idx;
                        let value_end = peek_value_end!(value_start, ErrorType::Syntax, b'\n');

                        parse_and_insert_value!(value_start, value_end);

                        if i >= structural_indexes.len() {
                            goto!(State::ScopeEnd);
                        }

                        // Here, get_value_end! only peeked, so `i` is correctly pointing at the newline.
                        let newline_idx = *get!(structural_indexes, i) as usize;
                        goto!(State::CheckIndentation(newline_idx));
                    }
                }

                State::CheckIndentation(newline_idx) => {
                    if depth == 0 {
                        fail!(ErrorType::Syntax);
                    }

                    if i >= structural_indexes.len() {
                        goto!(State::ScopeEnd);
                    }

                    if *get!(input2, newline_idx) != b'\n' {
                        fail!(ErrorType::NoStructure);
                    }

                    i += 1;

                    if i >= structural_indexes.len() {
                        goto!(State::ScopeEnd);
                    }

                    let next_idx = *get!(structural_indexes, i) as usize;
                    if *get!(input2, next_idx) == b'\n' {
                        fail!(ErrorType::Syntax);
                    }

                    let actual_ws = next_idx - newline_idx - 1;

                    // Clean math replacing the `object_expected_ws!` macro
                    let sibling_ws = (((depth - 1) * 2) as isize + indent_modifier) as usize;

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

                                // We dropped into ScopeEnd because CheckIndentation found
                                // that the indentation dropped. CheckIndentation already
                                // consumed the newline (i += 1). We need to back up so we
                                // can re-evaluate that exact same newline against the new depth
                                i -= 1;
                                let newline_idx = *get!(structural_indexes, i) as usize;
                                goto!(State::CheckIndentation(newline_idx));
                            }
                            StackState::Start => {
                                // Skip any trailing `\n` structurals (EOF terminators).
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
                            StackState::ArrayItem => {
                                let (
                                    parent_last_start,
                                    parent_cnt,
                                    tape_start,
                                    declared_len,
                                    mut elem_cnt,
                                    compact_indent,
                                    is_root,
                                ) = match *stack_ptr.add(depth - 1) {
                                    StackState::Array {
                                        parent_last_start,
                                        parent_cnt,
                                        tape_start,
                                        declared_len,
                                        elem_cnt,
                                        compact_indent,
                                        is_root,
                                    } => (
                                        parent_last_start,
                                        parent_cnt,
                                        tape_start,
                                        declared_len,
                                        elem_cnt,
                                        compact_indent,
                                        is_root,
                                    ),
                                    _ => {
                                        fail!(ErrorType::NoStructure);
                                    }
                                };

                                // O(1) Accumulator: Remove the root bias since we just dropped the item object
                                if is_root {
                                    indent_modifier -= 2;
                                }

                                elem_cnt += 1;
                                unsafe {
                                    stack_ptr.add(depth - 1).write(StackState::Array {
                                        parent_last_start,
                                        parent_cnt,
                                        tape_start,
                                        declared_len,
                                        elem_cnt,
                                        compact_indent,
                                        is_root,
                                    });
                                }

                                if elem_cnt > declared_len {
                                    fail!(ErrorType::ArrayCountMismatch);
                                }

                                if elem_cnt < declared_len {
                                    if i >= structural_indexes.len() {
                                        fail!(ErrorType::ArrayCountMismatch);
                                    }

                                    let next_idx = *get!(structural_indexes, i) as usize;
                                    let next_c = *get!(input2, next_idx);
                                    if next_c == b'\n' {
                                        fail!(ErrorType::BlankLineInBlock);
                                    }

                                    if i == 0 {
                                        fail!(ErrorType::Syntax);
                                    }
                                    let newline_idx = *get!(structural_indexes, i - 1) as usize;
                                    if *get!(input2, newline_idx) != b'\n' {
                                        fail!(ErrorType::Syntax);
                                    }

                                    let actual_ws = next_idx - newline_idx - 1;
                                    let base_marker =
                                        if depth == 1 { 2 } else { (depth - 1) * 2 } as isize;
                                    let marker_ws = (base_marker + indent_modifier) as usize;

                                    if actual_ws < marker_ws && actual_ws.is_multiple_of(2) {
                                        fail!(ErrorType::ArrayCountMismatch);
                                    }
                                    if actual_ws != marker_ws {
                                        fail!(ErrorType::InvalidIndentation);
                                    }
                                    if next_c != b'-' {
                                        fail!(ErrorType::InvalidListMarker);
                                    }

                                    goto!(State::ParseBlockArrayItem);
                                }

                                unsafe {
                                    match *res_ptr.add(tape_start) {
                                        Node::Array { ref mut count, .. } => {
                                            *count = r_i - tape_start - 1;
                                        }
                                        _ => unreachable!("array backfill expects Array node"),
                                    }
                                }

                                depth -= 1;
                                last_start = parent_last_start;
                                cnt = parent_cnt;

                                if compact_indent {
                                    indent_modifier += 2;
                                }

                                if i >= structural_indexes.len() {
                                    if depth == 0 {
                                        success!();
                                    }
                                    goto!(State::ScopeEnd);
                                }

                                if depth == 0 {
                                    i -= 1;
                                    skip_newline_chars!();
                                }

                                i -= 1;
                                let newline_idx = *get!(structural_indexes, i) as usize;
                                goto!(State::CheckIndentation(newline_idx));
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
