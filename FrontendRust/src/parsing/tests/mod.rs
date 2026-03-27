pub mod after_regions_tests;
pub mod expression_tests;
pub mod functions;
pub mod if_tests;
pub mod impl_tests;
pub mod load_tests;
pub mod parse_samples_tests;
pub mod parser_test_compilation;
pub mod patterns;
pub mod rules;
pub mod sample_paths;
pub mod statement_tests;
pub mod struct_tests;
pub mod top_level_tests;
pub mod traverse;
pub mod utils;
pub mod while_tests;

#[cfg(feature = "tree-sitter-tests")]
pub mod tree_sitter_tests;

#[cfg(test)]
pub mod textmate_tests;
