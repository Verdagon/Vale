// Scans `tests/rust-interop/*.vale`, reads the `// expected_exit: N` or
// `// expects_build_fail: true` header from each, and queues them as rust-interop tests
// on the suite. This is the new test path that the bash runner used to drive directly;
// pulling it into TesterRust lets the same runner cover both the canonical Vale corpus
// and the rust-interop tests.

use crate::runner::RustInteropOutcome;
use crate::suite::TestSuite;
use std::fs;

pub fn register(suite: &mut TestSuite) {
    let dir = suite.opts.rust_interop_tests_dir.clone();
    if !dir.is_dir() {
        eprintln!(
            "rust-interop tests dir {} doesn't exist; skipping the rust-interop suite.",
            dir.display()
        );
        return;
    }

    // Sort for deterministic order — bash runner sorted alphabetically too.
    let mut entries: Vec<_> = match fs::read_dir(&dir) {
        Ok(it) => it.filter_map(|e| e.ok()).collect(),
        Err(e) => {
            eprintln!("read_dir({}) failed: {}", dir.display(), e);
            return;
        }
    };
    entries.sort_by_key(|e| e.file_name());

    // The rust-interop pipeline today only supports resilient-v3 — both the bash runner
    // and valec's default produce resilient-v3 output. Future work could parameterize.
    let region = "resilient-v3";

    for entry in entries {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("vale") {
            continue;
        }
        let test_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string());
        let test_name = match test_name {
            Some(n) => n,
            None => continue,
        };

        let outcome = match parse_header(&path) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("Skipping {}: {}", path.display(), e);
                continue;
            }
        };

        // Each test program lives in its own dir so that valec's per-test build can use
        // the dir as a module root. We pass the FILE (not the dir) here — the bash runner
        // copied the file into a temp module dir per test; we let valec handle it by
        // passing the source file directly with `vtest=<path>` (see builder.rs).
        suite.start_rust_interop_test(&test_name, &path, outcome, region);
    }
}

/// Mirrors the bash runner's grep step: look for `// expected_exit: N` first, otherwise
/// `// expects_build_fail: true`. Either header is acceptable; missing both is an error.
fn parse_header(path: &std::path::Path) -> Result<RustInteropOutcome, String> {
    let contents = fs::read_to_string(path)
        .map_err(|e| format!("read {}: {}", path.display(), e))?;

    // We only scan the file header — the first ~20 lines — to mirror the bash runner's
    // implicit "headers are at the top" expectation and to avoid matching a comment in
    // body code by accident.
    for line in contents.lines().take(40) {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed.strip_prefix("// expected_exit:") {
            let n: i32 = rest
                .trim()
                .parse()
                .map_err(|e| format!("bad expected_exit value in {}: {}", path.display(), e))?;
            return Ok(RustInteropOutcome::ExpectedExit(n));
        }
        if let Some(rest) = trimmed.strip_prefix("// expects_build_fail:") {
            if rest.trim() == "true" {
                return Ok(RustInteropOutcome::BuildFail);
            }
        }
    }
    Err(format!(
        "{} has no '// expected_exit: N' or '// expects_build_fail: true' header",
        path.display()
    ))
}
