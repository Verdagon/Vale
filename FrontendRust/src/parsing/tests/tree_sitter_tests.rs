//! Tree-sitter grammar tests: parse every Vale sample file and assert no ERROR nodes.
//!
//! These tests require the `tree-sitter-tests` feature:
//!   cargo test --features tree-sitter-tests
//!
//! Before running, ensure the tree-sitter parser has been generated:
//!   cd syntax-highlighting/tree-sitter-vale && npm install && npx tree-sitter generate

#![cfg(feature = "tree-sitter-tests")]

use std::fs;
use std::path::PathBuf;

fn load_sample(path: &str) -> String {
    let full_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../Frontend/Tests/test/main/resources")
        .join(path);
    fs::read_to_string(&full_path)
        .unwrap_or_else(|e| panic!("Failed to load sample '{}': {} ({:?})", path, e, full_path))
}

fn collect_errors(node: tree_sitter::Node, source: &str, errors: &mut Vec<String>) {
    if node.is_error() || node.is_missing() {
        let start = node.start_position();
        let end = node.end_position();
        let byte_range = node.byte_range();
        let text = &source[byte_range.start..byte_range.end.min(source.len())];
        let snippet: String = text.chars().take(60).collect();
        errors.push(format!(
            "  {}:{}-{}:{}: {} {:?}",
            start.row + 1,
            start.column,
            end.row + 1,
            end.column,
            if node.is_error() { "ERROR" } else { "MISSING" },
            snippet,
        ));
    }
    for i in 0..node.child_count() {
        collect_errors(node.child(i).unwrap(), source, errors);
    }
}

fn assert_no_errors(path: &str, source: &str) {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_vale::language())
        .expect("Failed to load Vale language");
    let tree = parser.parse(source, None).expect("Parser returned None");

    let mut errors = Vec::new();
    collect_errors(tree.root_node(), source, &mut errors);

    assert!(
        errors.is_empty(),
        "Tree-sitter parse errors in {}:\n{}",
        path,
        errors.join("\n"),
    );
}

macro_rules! tree_sitter_sample_test {
    ($name:ident, $path:literal) => {
        #[test]
        fn $name() {
            let source = load_sample($path);
            assert_no_errors($path, &source);
        }
    };
}

use super::sample_paths::for_each_sample_path;
for_each_sample_path!(tree_sitter_sample_test);
