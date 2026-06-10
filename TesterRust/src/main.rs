// Rust port of Tester/src/main.vale.
//
// Mirrors Tester's CLI, semantics, and output format as closely as possible. Adds three
// rust-interop CLI flags (--vale_ruster_path, --divination_path, --rust_cargo_toml) and a
// `// expected_exit:` / `// expects_build_fail:` header-driven directory-scan test path so
// the rust-interop tests under tests/rust-interop/ can be driven by the same runner as the
// canonical Vale corpus.
//
// The hardcoded test catalog from Tester/src/main.vale is ported in catalog.rs.

mod cli;
mod suite;
mod builder;
mod runner;
mod catalog;
mod rust_interop;

use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    let opts = match cli::parse(&args) {
        Ok(o) => o,
        Err(msg) => {
            eprintln!("{}", msg);
            return ExitCode::from(2);
        }
    };

    if opts.verbose {
        println!("Tester processing flags");
    }

    let mut suite = suite::TestSuite::new(opts);

    if suite.verbose {
        println!("Parsed command line inputs...");
    }

    // Register the canonical Vale-corpus tests.
    catalog::register(&mut suite);

    // Register the rust-interop tests (directory-scanned + header-driven).
    if suite.opts.vale_ruster_path.is_some() {
        rust_interop::register(&mut suite);
    }

    // Drain any still-running build subprocesses.
    suite.finish_tests(0);

    let successes = suite.num_successes;
    let failures = suite.num_failures;
    println!("Done! Passed {}/{}", successes, successes + failures);

    if failures == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
