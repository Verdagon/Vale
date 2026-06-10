// Mirrors Tester/src/main.vale's TestSuite, TestInstance, Step structs. The Vale
// originals are at:
//   TestSuite     — main.vale lines 7-21
//   TestInstance  — main.vale lines 23-30
//   Step          — main.vale lines 32-35

use crate::cli::Opts;
use std::path::PathBuf;
use std::process::Child;

/// One run of the produced binary: the args to pass and the return code we expect.
/// Mirrors `struct Step` in main.vale.
pub struct Step {
    pub args: Vec<String>,
    pub expected_return_code: i32,
}

/// One in-flight test (build subprocess running, runs queued for after it completes).
/// Mirrors `struct TestInstance` in main.vale.
pub struct TestInstance {
    pub test_name: String,
    pub region: String,
    pub test_build_dir: PathBuf,
    pub process: Child,
    pub runs: Vec<Step>,
    pub expect_stdout: Option<String>,
}

/// The whole suite's running state. Mirrors `struct TestSuite` in main.vale.
///
/// Tester's pre-computed `common_build_args` list is recomputed here on demand from the
/// parsed Opts — keeps Opts as the single source of truth and avoids the duplicated-string
/// state Vale had to manage manually.
pub struct TestSuite {
    pub opts: Opts,
    pub cwd: PathBuf,
    pub verbose: bool,
    pub flares: bool,
    pub test_instances: Vec<TestInstance>,
    pub num_successes: u64,
    pub num_failures: u64,
}

impl TestSuite {
    pub fn new(opts: Opts) -> Self {
        let cwd = std::env::current_dir().expect("current_dir failed");
        let verbose = opts.verbose;
        let flares = opts.flares;
        TestSuite {
            opts,
            cwd,
            verbose,
            flares,
            test_instances: Vec::new(),
            num_successes: 0,
            num_failures: 0,
        }
    }

    /// Tester's substring-AND filter (main.vale lines 37-47): a test name passes only if
    /// every filter is a substring of it. Empty filter list ⇒ every test passes.
    pub fn matches_filters(&self, name: &str) -> bool {
        for filter in &self.opts.test_filters {
            if !name.contains(filter) {
                return false;
            }
        }
        true
    }

    /// Build the `valec build` argv prefix shared by every test, mirroring lines 141-177
    /// of main.vale. The per-test args (`vtest=...`, `--output_dir`, `--region_override`,
    /// etc.) are appended later by `builder::start_build`.
    pub fn common_build_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        args.push("build".to_string());
        args.push("--frontend_path_override".to_string());
        args.push(self.opts.frontend_path.to_string_lossy().into_owned());
        args.push("--backend_path_override".to_string());
        args.push(self.opts.backend_path.to_string_lossy().into_owned());
        args.push("--builtins_dir_override".to_string());
        args.push(self.opts.builtins_dir.to_string_lossy().into_owned());
        if let Some(p) = &self.opts.clang_path {
            args.push("--clang_override".to_string());
            args.push(p.to_string_lossy().into_owned());
        }
        if let Some(p) = &self.opts.libc_path {
            args.push("--libc_override".to_string());
            args.push(p.to_string_lossy().into_owned());
        }
        args
    }

    /// Common rust-interop trailer — appended to `common_build_args` when the test under
    /// run imports `rust.*`. Equivalent of a Tester-side StartRustInteropTest variant.
    pub fn rust_interop_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        if let Some(p) = &self.opts.vale_ruster_path {
            args.push("--vale_ruster_path".to_string());
            args.push(p.to_string_lossy().into_owned());
        }
        if let Some(p) = &self.opts.divination_path {
            args.push("--divination_path".to_string());
            args.push(p.to_string_lossy().into_owned());
        }
        if let Some(p) = &self.opts.rust_cargo_toml {
            args.push("--rust_cargo_toml".to_string());
            args.push(p.to_string_lossy().into_owned());
        }
        args
    }
}
