use std::path::Path;

fn assert_exists(path: &str) {
    assert!(Path::new(path).exists(), "missing fixture: {path}");
}

#[test]
fn toon_pass_and_fail_directories_exist() {
    assert!(Path::new("data/toon/pass").is_dir());
    assert!(Path::new("data/toon/fail").is_dir());
}

#[test]
fn toon_pass_fixtures_exist() {
    let expected = [
        "data/toon/pass/01_basics.toon",
        "data/toon/pass/02_hierarchy.toon",
        "data/toon/pass/03_arrays_standard.toon",
        "data/toon/pass/04_arrays_tabular_comma.toon",
        "data/toon/pass/05_inline_structures.toon",
        "data/toon/pass/06_tricky_types.toon",
        "data/toon/pass/07_nulls_empty.toon",
        "data/toon/pass/08_mixed_array.toon",
        "data/toon/pass/09_key_folding.toon",
        "data/toon/pass/10_tabular_pipe_delimiter.toon",
        "data/toon/pass/11_indent_edge_valid.toon",
        "data/toon/pass/12_tabular_tab_delimiter.toon",
    ];

    for path in expected {
        assert_exists(path);
    }
}

#[test]
fn toon_fail_fixtures_exist() {
    let expected = [
        "data/toon/fail/01_invalid_indentation_tab.toon",
        "data/toon/fail/02_invalid_indentation_non_multiple.toon",
        "data/toon/fail/03_invalid_array_header.toon",
        "data/toon/fail/04_array_count_mismatch.toon",
        "data/toon/fail/05_tabular_row_field_mismatch.toon",
        "data/toon/fail/06_invalid_list_marker.toon",
        "data/toon/fail/07_blank_line_in_block.toon",
        "data/toon/fail/08_path_expansion_conflict.toon",
        "data/toon/fail/09_invalid_delimiter.toon",
        "data/toon/fail/EXPECTED_ERRORS.md",
    ];

    for path in expected {
        assert_exists(path);
    }
}
