// Hand-rolled flag parser to match Tester's CLI shape without taking a clap dependency.
// CoordinatorRust's convention is "no deps beyond what we already use," and Tester's flag
// surface is small enough (~12 flags) that std-only parsing is the same code size as the
// clap configuration would be.

use std::path::PathBuf;

/// Mirrors the union of Tester's CLI flags plus the three rust-interop additions.
pub struct Opts {
    // Tester-original flags.
    pub backend_path: PathBuf,
    pub frontend_path: PathBuf,
    pub builtins_dir: PathBuf,
    pub valec_path: PathBuf,
    pub clang_path: Option<PathBuf>,
    pub libc_path: Option<PathBuf>,
    pub backend_tests_dir: PathBuf,
    pub frontend_tests_dir: PathBuf,
    pub stdlib_dir: PathBuf,
    pub max_concurrent_tests: usize,
    pub verbose: bool,
    pub flares: bool,
    pub include_regions: Vec<String>,
    pub test_filters: Vec<String>,

    // Rust-interop additions. None ⇒ rust-interop tests aren't registered (so a plain Vale
    // corpus run doesn't require the rust toolchain to be present).
    pub vale_ruster_path: Option<PathBuf>,
    pub divination_path: Option<PathBuf>,
    pub rust_cargo_toml: Option<PathBuf>,
    pub rust_interop_tests_dir: PathBuf,
}

pub fn parse(args: &[String]) -> Result<Opts, String> {
    // First arg is argv[0]; skip it. The rest are flags or positional (regions / filters).
    let mut i = 1;

    // Defaults match Tester's defaults in main.vale lines 65-138 where applicable. Where
    // Tester defaults to a placeholder like "../Midas/build" we instead require the flag —
    // we don't have a Midas-style default tree.
    let mut backend_path: Option<PathBuf> = None;
    let mut frontend_path: Option<PathBuf> = None;
    let mut builtins_dir: Option<PathBuf> = None;
    let mut valec_path: Option<PathBuf> = None;
    let mut clang_path: Option<PathBuf> = None;
    let mut libc_path: Option<PathBuf> = None;
    let mut backend_tests_dir: Option<PathBuf> = None;
    let mut frontend_tests_dir: Option<PathBuf> = None;
    let mut stdlib_dir: Option<PathBuf> = None;
    let mut max_concurrent_tests: usize = 10;
    let mut verbose: bool = true;
    let mut flares: bool = false;

    let mut vale_ruster_path: Option<PathBuf> = None;
    let mut divination_path: Option<PathBuf> = None;
    let mut rust_cargo_toml: Option<PathBuf> = None;
    let mut rust_interop_tests_dir: Option<PathBuf> = None;

    let mut include_regions: Vec<String> = Vec::new();
    let mut test_filters: Vec<String> = Vec::new();

    while i < args.len() {
        let arg = &args[i];

        if let Some(rest) = arg.strip_prefix("@") {
            // `@<region>` registers an explicit region filter, same as Tester.
            include_regions.push(rest.to_string());
            i += 1;
            continue;
        }

        if !arg.starts_with("--") {
            // Unrecognized positional: it's a test-name substring filter.
            test_filters.push(arg.clone());
            i += 1;
            continue;
        }

        let want = |slot: &mut Option<PathBuf>, arg: &str, args: &[String], i: usize| -> Result<usize, String> {
            if i + 1 >= args.len() {
                Err(format!("{} requires a value", arg))
            } else {
                *slot = Some(PathBuf::from(&args[i + 1]));
                Ok(i + 2)
            }
        };
        let want_str = |arg: &str, args: &[String], i: usize| -> Result<(String, usize), String> {
            if i + 1 >= args.len() {
                Err(format!("{} requires a value", arg))
            } else {
                Ok((args[i + 1].clone(), i + 2))
            }
        };

        match arg.as_str() {
            "--backend_path"       => i = want(&mut backend_path, arg, args, i)?,
            "--frontend_path"      => i = want(&mut frontend_path, arg, args, i)?,
            "--builtins_dir"       => i = want(&mut builtins_dir, arg, args, i)?,
            "--valec_path"         => i = want(&mut valec_path, arg, args, i)?,
            "--clang_path"         => i = want(&mut clang_path, arg, args, i)?,
            "--libc_path"          => i = want(&mut libc_path, arg, args, i)?,
            "--backend_tests_dir"  => i = want(&mut backend_tests_dir, arg, args, i)?,
            "--frontend_tests_dir" => i = want(&mut frontend_tests_dir, arg, args, i)?,
            "--stdlib_dir"         => i = want(&mut stdlib_dir, arg, args, i)?,

            "--vale_ruster_path"   => i = want(&mut vale_ruster_path, arg, args, i)?,
            "--divination_path"    => i = want(&mut divination_path, arg, args, i)?,
            "--rust_cargo_toml"    => i = want(&mut rust_cargo_toml, arg, args, i)?,
            "--rust_interop_tests_dir" => i = want(&mut rust_interop_tests_dir, arg, args, i)?,

            "--concurrent" => {
                let (v, ni) = want_str(arg, args, i)?;
                max_concurrent_tests = v.parse::<usize>().map_err(|e| format!("--concurrent: {}", e))?;
                i = ni;
            }
            "--verbose" => {
                let (v, ni) = want_str(arg, args, i)?;
                verbose = parse_bool(&v)?;
                i = ni;
            }
            "--flares" => {
                let (v, ni) = want_str(arg, args, i)?;
                flares = parse_bool(&v)?;
                i = ni;
            }
            other => return Err(format!("Unrecognized flag: {}", other)),
        }
    }

    // Default regions match Tester's main.vale lines 209-214.
    if include_regions.is_empty() {
        include_regions.push("naive-rc".to_string());
        include_regions.push("resilient-v3".to_string());
        include_regions.push("unsafe-fast".to_string());
    }

    let backend_path = require(backend_path, "--backend_path")?;
    let frontend_path = require(frontend_path, "--frontend_path")?;
    let builtins_dir = require(builtins_dir, "--builtins_dir")?;
    let valec_path = require(valec_path, "--valec_path")?;
    let backend_tests_dir = require(backend_tests_dir, "--backend_tests_dir")?;
    let frontend_tests_dir = require(frontend_tests_dir, "--frontend_tests_dir")?;
    let stdlib_dir = require(stdlib_dir, "--stdlib_dir")?;

    // Rust-interop tests default to ./tests/rust-interop relative to the CWD. The user can
    // override via --rust_interop_tests_dir; if --vale_ruster_path isn't set, the dir is
    // ignored entirely (rust-interop registration is skipped in main.rs).
    let rust_interop_tests_dir = rust_interop_tests_dir
        .unwrap_or_else(|| PathBuf::from("tests/rust-interop"));

    Ok(Opts {
        backend_path,
        frontend_path,
        builtins_dir,
        valec_path,
        clang_path,
        libc_path,
        backend_tests_dir,
        frontend_tests_dir,
        stdlib_dir,
        max_concurrent_tests,
        verbose,
        flares,
        include_regions,
        test_filters,
        vale_ruster_path,
        divination_path,
        rust_cargo_toml,
        rust_interop_tests_dir,
    })
}

fn parse_bool(s: &str) -> Result<bool, String> {
    match s {
        "true" | "1" => Ok(true),
        "false" | "0" => Ok(false),
        other => Err(format!("expected true/false, got {}", other)),
    }
}

fn require(opt: Option<PathBuf>, name: &str) -> Result<PathBuf, String> {
    opt.ok_or_else(|| format!("missing required flag {}", name))
}
