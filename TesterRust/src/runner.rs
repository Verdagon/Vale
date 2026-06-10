// Mirrors `func StartTest`, `func StartReplayTest`, `func FinishTests` from
// Tester/src/main.vale (lines 525-660). The "build queue, drain when full" pattern is
// the same — at most `max_concurrent_tests` builds run in parallel; each new test waits
// for a slot via finish_tests.

use crate::builder::{start_build, TestKind};
use crate::suite::{Step, TestInstance, TestSuite};
use std::io::Read;
use std::path::Path;

impl TestSuite {
    /// Mirrors `func StartTest` (main.vale lines 587-614). Queue a single-run test that
    /// expects one return code and (optionally) one stdout match.
    pub fn start_test(
        &mut self,
        expected_return_code: i32,
        test_name: &str,
        vale_input: &Path,
        extra_build_flags: &[String],
        region: &str,
        include_stdlib: bool,
        expect_stdout: Option<String>,
    ) {
        if self.verbose {
            println!("Considering test {}...", test_name);
        }
        if !self.matches_filters(test_name) {
            return;
        }

        let slots_when_full = self.opts.max_concurrent_tests.saturating_sub(1);
        self.finish_tests(slots_when_full);

        let test_build_dir = self
            .cwd
            .join(format!("testbuild/{}_{}", test_name, region));

        let kind = TestKind::Vale { include_stdlib };
        match start_build(
            self,
            test_name,
            vale_input,
            extra_build_flags,
            &test_build_dir,
            region,
            &kind,
        ) {
            Ok(process) => {
                self.test_instances.push(TestInstance {
                    test_name: test_name.to_string(),
                    region: region.to_string(),
                    test_build_dir,
                    process,
                    runs: vec![Step {
                        args: Vec::new(),
                        expected_return_code,
                    }],
                    expect_stdout,
                });
            }
            Err(e) => {
                eprintln!("Spawn failed for {}: {}", test_name, e);
                self.num_failures += 1;
            }
        }
    }

    /// Mirrors `func StartReplayTest` (main.vale lines 616-660). Replay tests AASETR-run
    /// the binary three times: plain, --vale_record, --vale_replay; only the first run's
    /// expected return code differs (the AASETR convention is "first" / "repeated"). Per
    /// main.vale line 637 we also add `--enable_replaying true` to the build flags.
    pub fn start_replay_test(
        &mut self,
        first_expected_return_code: i32,
        repeated_expected_return_code: i32,
        test_name: &str,
        vale_input: &Path,
        specific_extra_build_flags: &[String],
        region: &str,
        include_stdlib: bool,
        expect_stdout: Option<String>,
    ) {
        if self.verbose {
            println!("Considering test {}...", test_name);
        }
        if !self.matches_filters(test_name) {
            return;
        }

        let slots_when_full = self.opts.max_concurrent_tests.saturating_sub(1);
        self.finish_tests(slots_when_full);

        let mut extra_build_flags: Vec<String> = specific_extra_build_flags.to_vec();
        extra_build_flags.push("--enable_replaying".to_string());
        extra_build_flags.push("true".to_string());

        let test_build_dir = self
            .cwd
            .join(format!("testbuild/{}_{}", test_name, region));

        let kind = TestKind::Vale { include_stdlib };
        match start_build(
            self,
            test_name,
            vale_input,
            &extra_build_flags,
            &test_build_dir,
            region,
            &kind,
        ) {
            Ok(process) => {
                self.test_instances.push(TestInstance {
                    test_name: test_name.to_string(),
                    region: region.to_string(),
                    test_build_dir,
                    process,
                    runs: vec![
                        Step { args: Vec::new(), expected_return_code: first_expected_return_code },
                        Step {
                            args: vec!["--vale_record".to_string(), "recording.bin".to_string()],
                            expected_return_code: repeated_expected_return_code,
                        },
                        Step {
                            args: vec!["--vale_replay".to_string(), "recording.bin".to_string()],
                            expected_return_code: repeated_expected_return_code,
                        },
                    ],
                    expect_stdout,
                });
            }
            Err(e) => {
                eprintln!("Spawn failed for {}: {}", test_name, e);
                self.num_failures += 1;
            }
        }
    }

    /// Queue a rust-interop test. Two outcome modes:
    ///   - `Some(N)`: build must succeed, the produced binary must exit with N.
    ///   - `None`:    we EXPECT the build to fail (the `// expects_build_fail: true`
    ///                tests). The drain step marks success if the build's exit ≠ 0.
    /// `opts` carries the optional `// include_stdlib: true` / `// expected_stdout: "..."`
    /// behaviors — defaults for both reproduce the original ri_* shape (no stdlib, no
    /// stdout check).
    pub fn start_rust_interop_test(
        &mut self,
        test_name: &str,
        vale_dir: &Path,
        expected_exit_or_buildfail: RustInteropOutcome,
        region: &str,
        opts: RustInteropOpts,
    ) {
        if self.verbose {
            println!("Considering rust-interop test {}...", test_name);
        }
        if !self.matches_filters(test_name) {
            return;
        }

        let slots_when_full = self.opts.max_concurrent_tests.saturating_sub(1);
        self.finish_tests(slots_when_full);

        let test_build_dir = self
            .cwd
            .join(format!("testbuild/{}_{}", test_name, region));

        let kind = TestKind::RustInterop { include_stdlib: opts.include_stdlib };
        match start_build(
            self,
            test_name,
            vale_dir,
            &[],
            &test_build_dir,
            region,
            &kind,
        ) {
            Ok(process) => {
                let runs = match expected_exit_or_buildfail {
                    RustInteropOutcome::ExpectedExit(n) => vec![Step {
                        args: Vec::new(),
                        expected_return_code: n,
                    }],
                    // Empty runs ⇒ finish_tests sees no run steps and (per the build_fail
                    // branch below) treats build-fail as the expected outcome.
                    RustInteropOutcome::BuildFail => Vec::new(),
                };
                self.test_instances.push(TestInstance {
                    test_name: test_name.to_string(),
                    region: region.to_string(),
                    test_build_dir,
                    process,
                    runs,
                    expect_stdout: opts.expected_stdout,
                });
            }
            Err(e) => {
                eprintln!("Spawn failed for {}: {}", test_name, e);
                self.num_failures += 1;
            }
        }
    }

    /// Mirrors `func FinishTests` (main.vale lines 441-523). Drains test_instances from
    /// the front, waiting on each build and asserting return code + stdout per step. Stops
    /// once `until_this_many_left` are still queued — Tester uses this to throttle the
    /// pool without forcing a hard barrier between every test.
    pub fn finish_tests(&mut self, until_this_many_left: usize) {
        while self.test_instances.len() > until_this_many_left {
            // Take ownership of the queued instance so we can move the Child into
            // wait_with_output (which takes self by value).
            let TestInstance {
                test_name,
                region,
                test_build_dir,
                process,
                runs,
                expect_stdout,
            } = self.test_instances.remove(0);

            // capture_and_join equivalent: wait for the build, capture stdout/stderr.
            let build_result = match process.wait_with_output() {
                Ok(o) => o,
                Err(e) => {
                    eprintln!("wait_with_output failed for {}: {}", test_name, e);
                    self.num_failures += 1;
                    continue;
                }
            };

            let build_ok = build_result.status.success();
            // The empty-runs case (rust-interop expects_build_fail) inverts the polarity:
            // we PASS iff the build failed; we FAIL iff it built.
            let is_build_fail_test = runs.is_empty();

            if is_build_fail_test {
                if !build_ok {
                    println!(
                        "Test {} (region {}) succeeded! (build failed as expected)",
                        test_name, region
                    );
                    self.num_successes += 1;
                } else {
                    println!(
                        "Test {} (region {}) failed: expected build_fail, build succeeded.",
                        test_name, region
                    );
                    self.num_failures += 1;
                }
                continue;
            }

            if !build_ok {
                print_build_failure(
                    &build_result.status,
                    &build_result.stdout,
                    &build_result.stderr,
                    &test_name,
                    &region,
                );
                self.num_failures += 1;
                continue;
            }

            // Build succeeded — run the produced binary once per Step.
            let program_name = if cfg!(windows) { "main.exe" } else { "main" };
            let run_program = test_build_dir.join(program_name);

            let mut all_runs_succeeded = true;

            for step in &runs {
                let run = std::process::Command::new(&run_program)
                    .args(&step.args)
                    .current_dir(test_build_dir.as_path())
                    .stdout(std::process::Stdio::piped())
                    .stderr(std::process::Stdio::piped())
                    .spawn();

                let run = match run {
                    Ok(c) => c,
                    Err(e) => {
                        eprintln!(
                            "Spawn of {} failed for {}: {}",
                            run_program.display(),
                            test_name,
                            e
                        );
                        all_runs_succeeded = false;
                        break;
                    }
                };

                let run_result = match run.wait_with_output() {
                    Ok(o) => o,
                    Err(e) => {
                        eprintln!("wait_with_output (run) failed for {}: {}", test_name, e);
                        all_runs_succeeded = false;
                        break;
                    }
                };

                let actual = run_result.status.code().unwrap_or(-1);
                let mut this_run_succeeded = true;
                let mut print_stdout_stderr = self.verbose;

                if actual != step.expected_return_code {
                    println!(
                        "Invalid result for test {} (region {}). Expected return {} but got {}.",
                        test_name, region, step.expected_return_code, actual
                    );
                    print_stdout_stderr = true;
                    this_run_succeeded = false;
                }

                if let Some(expected) = expect_stdout.as_ref() {
                    let stdout_str = String::from_utf8_lossy(&run_result.stdout);
                    if stdout_str.as_ref() != expected.as_str() {
                        if !stdout_str.is_empty() {
                            println!(
                                "Invalid result for test {} (region {}), expected stdout:",
                                test_name, region
                            );
                            println!("{}", expected);
                        } else {
                            println!(
                                "Invalid result for test {} (region {}), expected empty stdout.",
                                test_name, region
                            );
                        }
                        print_stdout_stderr = true;
                        this_run_succeeded = false;
                    }
                }

                if print_stdout_stderr {
                    print_streams(&run_result.stdout, &run_result.stderr);
                }

                if !this_run_succeeded {
                    all_runs_succeeded = false;
                }
            }

            if all_runs_succeeded {
                println!("Test {} (region {}) succeeded!", test_name, region);
                self.num_successes += 1;
            } else {
                self.num_failures += 1;
            }
        }
    }
}

pub enum RustInteropOutcome {
    ExpectedExit(i32),
    BuildFail,
}

/// Optional behaviors a rust-interop test can opt into via headers:
/// `// include_stdlib: true` and `// expected_stdout: "..."`. Defaults match the
/// no-stdlib / no-stdout-check shape used by the original 28 ri_* tests.
pub struct RustInteropOpts {
    pub include_stdlib: bool,
    pub expected_stdout: Option<String>,
}

impl Default for RustInteropOpts {
    fn default() -> Self {
        RustInteropOpts { include_stdlib: false, expected_stdout: None }
    }
}

fn print_build_failure(
    status: &std::process::ExitStatus,
    stdout: &[u8],
    stderr: &[u8],
    test_name: &str,
    region: &str,
) {
    println!(
        "Error {} building test {} (region {}).",
        status.code().unwrap_or(-1),
        test_name,
        region
    );
    print_streams(stdout, stderr);
}

fn print_streams(stdout: &[u8], stderr: &[u8]) {
    if !stdout.is_empty() {
        println!("stdout:");
        let _ = std::io::stdout().write_all_then_newline(stdout);
    } else {
        println!("(no stdout)");
    }
    if !stderr.is_empty() {
        println!("stderr:");
        let _ = std::io::stdout().write_all_then_newline(stderr);
    } else {
        println!("(no stderr)");
    }
}

/// Tiny extension trait so the stream-printing call sites read like the Vale equivalents.
trait WriteAllThenNewline {
    fn write_all_then_newline(&mut self, buf: &[u8]) -> std::io::Result<()>;
}
impl WriteAllThenNewline for std::io::Stdout {
    fn write_all_then_newline(&mut self, buf: &[u8]) -> std::io::Result<()> {
        use std::io::Write;
        self.write_all(buf)?;
        if !buf.ends_with(b"\n") {
            self.write_all(b"\n")?;
        }
        Ok(())
    }
}

// Silence dead-code warning on Read import — kept for future use if we want to stream
// stdout during runs rather than capturing whole.
#[allow(dead_code)]
fn _force_read_use(r: &mut dyn Read) -> std::io::Result<()> {
    let mut buf = Vec::new();
    r.read_to_end(&mut buf)?;
    Ok(())
}
