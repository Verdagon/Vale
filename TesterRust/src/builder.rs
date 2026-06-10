// Mirrors `func StartBuild` in Tester/src/main.vale (lines 525-585). Constructs the
// `valec build` argv for one test and spawns it as a child process whose handle we hold
// onto in the TestInstance until `finish_tests` drains it.

use crate::suite::TestSuite;
use std::path::Path;
use std::process::{Child, Command, Stdio};

/// `test_kind` distinguishes "ordinary Vale corpus test" from "rust-interop test" — the
/// latter needs the three extra CLI flags appended to the build command. Other test kinds
/// can be added as new variants without touching the per-test registration code.
pub enum TestKind {
    /// A regular Vale corpus test (the `samples_path./("programs/...")` cases in Tester).
    /// `include_stdlib` mirrors Tester's bool argument with the same name.
    Vale { include_stdlib: bool },
    /// A rust-interop test under tests/rust-interop/. Forces resilient-v3, skips the
    /// testbuiltins.c injection (the rust-interop tests are self-contained), and appends
    /// the rust-interop CLI flags. `include_stdlib` mirrors the Vale corpus arm — opt-in
    /// for tests that use `println` / `castI64Str` / etc.
    RustInterop { include_stdlib: bool },
}

/// Spawn a valec build subprocess for one test. Mirrors `func StartBuild`.
///
/// Per Tester: `vtest=<vale_input>` is the source dir/file, `vtest=<testbuiltins.c>` is
/// the C helper Tester injects into every build, `--output_dir` and `--region_override`
/// are per-test, and `--llvm_ir true --opt_level O0` keep the build cheap to iterate on.
pub fn start_build(
    suite: &TestSuite,
    test_name: &str,
    vale_input: &Path,
    extra_build_flags: &[String],
    test_build_dir: &Path,
    region: &str,
    kind: &TestKind,
) -> std::io::Result<Child> {
    println!("Starting {}, region {}...", test_name, region);

    let mut args = suite.common_build_args();

    args.push(format!("vtest={}", vale_input.to_string_lossy()));

    // Tester injects testbuiltins.c into every test (line 539 of main.vale). Rust-interop
    // tests are self-contained against `--no_std true` and the rust binding so we skip the
    // injection there — adding it just slows the build for no observable change.
    if !matches!(kind, TestKind::RustInterop { .. }) {
        let testbuiltins = suite.opts.backend_tests_dir.join("testbuiltins.c");
        args.push(format!("vtest={}", testbuiltins.to_string_lossy()));
    }

    args.push("--output_dir".to_string());
    args.push(test_build_dir.to_string_lossy().into_owned());
    args.push("--region_override".to_string());
    args.push(region.to_string());
    args.push("--llvm_ir".to_string());
    args.push("true".to_string());
    args.push("--opt_level".to_string());
    args.push("O0".to_string());

    if suite.flares {
        args.push("--flares".to_string());
        args.push("true".to_string());
    }

    // Tester always passes --no_std true (line 560-561). The stdlib gets re-added below
    // when include_stdlib is true so tests use the HEAD stdlib, not the bootstrap one.
    args.push("--no_std".to_string());
    args.push("true".to_string());

    let include_stdlib_now = match kind {
        TestKind::Vale { include_stdlib } => *include_stdlib,
        TestKind::RustInterop { include_stdlib } => *include_stdlib,
    };
    if include_stdlib_now {
        args.push(format!(
            "stdlib={}",
            suite.opts.stdlib_dir.join("src").to_string_lossy()
        ));
    }

    if matches!(kind, TestKind::RustInterop { .. }) {
        // Append the three rust-interop flags so PassManager invokes ValeRuster and
        // Backend invokes Divination.
        for arg in suite.rust_interop_args() {
            args.push(arg);
        }
    }

    for flag in extra_build_flags {
        args.push(flag.clone());
    }

    if suite.verbose {
        println!("Starting subprocess...");
        println!(
            "Build command: {} {}",
            suite.opts.valec_path.to_string_lossy(),
            args.join(" ")
        );
    }

    Command::new(&suite.opts.valec_path)
        .args(&args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
}
