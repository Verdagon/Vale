// Scans `tests/rust-interop/*.vale`, reads the `// expected_exit: N` or
// `// expects_build_fail: true` header from each, and queues them as rust-interop tests
// on the suite. This is the new test path that the bash runner used to drive directly;
// pulling it into TesterRust lets the same runner cover both the canonical Vale corpus
// and the rust-interop tests.

use crate::runner::{RustInteropOpts, RustInteropOutcome};
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

        let (outcome, opts) = match parse_header(&path) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Skipping {}: {}", path.display(), e);
                continue;
            }
        };

        // Each test program lives in its own dir so that valec's per-test build can use
        // the dir as a module root. We pass the FILE (not the dir) here — the bash runner
        // copied the file into a temp module dir per test; we let valec handle it by
        // passing the source file directly with `vtest=<path>` (see builder.rs).
        suite.start_rust_interop_test(&test_name, &path, outcome, region, opts);
    }
}

/// Scans the first ~40 lines for these headers, in any order:
///   `// expected_exit: N`             (mutually exclusive with expects_build_fail)
///   `// expects_build_fail: true`     (mutually exclusive with expected_exit)
///   `// include_stdlib: true`         (opt-in, default false)
///   `// expected_stdout: "..."`       (opt-in, supports \n / \t / \" / \\ escapes)
/// Returns the outcome (build-fail vs exit) plus the optional opts. Missing both of the
/// mandatory headers is an error and the runner skips the file (Catter's vendored
/// main.vale is exempt — it gets registered directly with hard-coded outcome+opts).
fn parse_header(
    path: &std::path::Path,
) -> Result<(RustInteropOutcome, RustInteropOpts), String> {
    let contents = fs::read_to_string(path)
        .map_err(|e| format!("read {}: {}", path.display(), e))?;

    let mut outcome: Option<RustInteropOutcome> = None;
    let mut opts = RustInteropOpts::default();

    for line in contents.lines().take(40) {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed.strip_prefix("// expected_exit:") {
            let n: i32 = rest
                .trim()
                .parse()
                .map_err(|e| format!("bad expected_exit value in {}: {}", path.display(), e))?;
            outcome = Some(RustInteropOutcome::ExpectedExit(n));
        } else if let Some(rest) = trimmed.strip_prefix("// expects_build_fail:") {
            if rest.trim() == "true" {
                outcome = Some(RustInteropOutcome::BuildFail);
            }
        } else if let Some(rest) = trimmed.strip_prefix("// include_stdlib:") {
            if rest.trim() == "true" {
                opts.include_stdlib = true;
            }
        } else if let Some(rest) = trimmed.strip_prefix("// expected_stdout:") {
            opts.expected_stdout = Some(parse_quoted_string(rest.trim()).ok_or_else(|| {
                format!(
                    "bad expected_stdout in {}: expected a \"...\"-quoted string with optional \\n / \\t / \\\" escapes",
                    path.display()
                )
            })?);
        }
    }

    let outcome = outcome.ok_or_else(|| {
        format!(
            "{} has no '// expected_exit: N' or '// expects_build_fail: true' header",
            path.display()
        )
    })?;
    Ok((outcome, opts))
}

/// Parse a double-quoted string with backslash escapes (`\n`, `\t`, `\"`, `\\`). Returns
/// None if the value isn't wrapped in `"..."` or an unknown escape is encountered.
/// Deliberately minimal — covers what we need for `// expected_stdout: "length:\n42\n"`.
fn parse_quoted_string(raw: &str) -> Option<String> {
    let bytes = raw.as_bytes();
    if bytes.len() < 2 || bytes[0] != b'"' || bytes[bytes.len() - 1] != b'"' {
        return None;
    }
    let inner = &raw[1..raw.len() - 1];
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next()? {
                'n' => out.push('\n'),
                't' => out.push('\t'),
                'r' => out.push('\r'),
                '"' => out.push('"'),
                '\\' => out.push('\\'),
                _ => return None,
            }
        } else {
            out.push(c);
        }
    }
    Some(out)
}
