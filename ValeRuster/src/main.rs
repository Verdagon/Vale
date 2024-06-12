#![allow(unused_imports)]

mod indexer;
mod resolve_id;
mod vast_types;

extern crate toml;

use std::collections::{HashMap, HashSet};
use std::{fs, io};
use std::cmp::max;
use std::fmt::{Debug, Pointer};
use clap::Arg;
use std::process::Command;
use clap::ArgAction;
use std::fs::File;
use std::io::{BufRead, Read};
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::ptr::replace;
use anyhow::{Context, Result};
use regex::Regex;
use rustdoc_types::{Crate, Id, ItemEnum};
use crate::indexer::ItemIndex;
use crate::ParsedType::ImplCast;
use crate::resolve_id::{extend_and_resolve_uid, lookup_uid};
use crate::ResolveError::ResolveFatal;

// Universal id.
// This can refer to anything, including type aliases, imports, etc.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct UId {
  crate_name: String,
  id: Id,
}

struct TypeInfo {
  canonical_type: ParsedFullType,
  public_type: ParsedFullType,
  c_name: String,
  size: usize,
  alignment: usize
}

struct FuncInfo {
  canonical_type: ParsedFullType,
  public_type: ParsedFullType,
  c_name: String,
  ret_type_rust_str: String,
  param_types_rust_strs: Vec<String>
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct ParsedFullType {
  steps: Vec<ParsedType>
}
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum ParsedType {
  Ref{ mutable: bool, inner: ParsedFullType },
  Value{ name: String, generic_args: Vec<ParsedFullType>, params: Vec<ParsedFullType> },
  Alias(String),
  Tuple{ generic_args: Vec<ParsedFullType> },
  Slice{ inner: ParsedFullType },
  ImplCast{ struct_: ParsedFullType, impl_: ParsedFullType },
  Primitive(String),
  Lifetime,
  Wildcard,
}

fn main() -> Result<(), anyhow::Error> {
  // _rust_2_std_collections_HashMap__i32__String
  // _rust_2_std_collections_HashMap__i32__2_std_collections_Vec__String

  let root_matches =
      clap::Command::new("ValeRuster")
          .version("1.0")
          .author("Evan Ovadia")
          .about("Creates bindings for arbitrary Rust libraries")
          .subcommand(
            clap::Command::new("list")
                .about("List all generic structs and functions")
                .arg(Arg::new("struct")
                    .long("struct")
                    .help("The struct to list methods for")
                    .action(ArgAction::Set)))
          .subcommand(
            clap::Command::new("instantiate")
                .about("Instantiate either a function or a struct.")
                .arg(Arg::new("input_file")
                    .long("input_file")
                    .help("File to read from.")
                    .action(ArgAction::Set))
            // .arg(Arg::new("as")
            //     .long("as")
            //     .help("Sets the name for the newly generated type.")
            //     .action(ArgAction::Set))
            // .arg(Arg::new("c_folder")
            //     .long("c_folder")
            //     .help("The folder to output the C code to")
            //     .action(ArgAction::Set)
            //     .required(true))

            // .arg(Arg::new("generated")
            //     .long("generated")
            //     .help("The folder to output the Rust code to")
            //     .action(ArgAction::Set)
            //     .required(true)))
          )
          .arg(Arg::new("crate")
              .long("crate")
              .help("The crate name to generate bindings for")
              .action(ArgAction::Set)
              .required(true))
          .arg(Arg::new("output_dir")
              .long("output_dir")
              .help("Directory to output to.")
              .action(ArgAction::Set)
              .required(true))
          .arg(Arg::new("output_sizes")
              .long("output_sizes")
              .help("File to output size information to.")
              .action(ArgAction::Set))
          .arg(Arg::new("cargo_toml")
              .long("cargo_toml")
              .help("The Cargo.toml to use for dependencies.")
              .action(ArgAction::Set)
              .required(true))
          .arg(Arg::new("rustc_path")
              .long("rustc_path")
              .help("Sets the path to rustc")
              .action(ArgAction::Set))
          .arg(Arg::new("cargo_path")
              .long("cargo_path")
              .help("Sets the path to cargo")
              .action(ArgAction::Set))
          .get_matches();

  let crate_name: String = root_matches.get_one::<String>("crate").unwrap().to_string();
  let rustc_path: String = root_matches.get_one::<String>("rustc_path").unwrap_or(&"rustc".to_string()).to_string();
  let cargo_path: String = root_matches.get_one::<String>("cargo_path").unwrap_or(&"cargo".to_string()).to_string();
  let cargo_toml_path: String = root_matches.get_one::<String>("cargo_toml").unwrap().to_string();

  // `rustc --print sysroot`/share/doc/rust/json
  let command = &rustc_path;
  let args = ["--print", "sysroot"];
  let output =
      Command::new(&rustc_path).args(&args).output()
          .with_context(|| format!("Failed to read {}", &rustc_path))?;
  if !output.status.success() {
    let error = String::from_utf8_lossy(&output.stderr);
    return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, error)));
  }
  let rustc_sysroot_path: String =
      String::from_utf8_lossy(&output.stdout).trim().to_string();
  if rustc_sysroot_path.len() == 0 {
    return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, format!("No stdout from command: {} {}", command, args.join(" ")))));
  }

  let output_dir_path = root_matches.get_one::<String>("output_dir").unwrap();
  let maybe_output_sizes_path = root_matches.get_one::<String>("output_sizes");

  // This should be done before read_toml, so it can read the generated docs from it.
  setup_output_dir(&cargo_toml_path, &output_dir_path)?;

  match root_matches.subcommand() {
    Some(("list", list_matches)) => {
      unimplemented!();
    }
    Some(("instantiate", instantiate_matches)) => {
      // let c_folder = instantiate_matches.get_one::<String>("c_folder").unwrap();
      // if Path::new(c_folder).exists() {
      //   fs::remove_dir_all(c_folder)?;
      // }
      // fs::create_dir(c_folder)
      //     .with_context(|| "Failed to create directory ".to_owned() + c_folder)?;

      // let rust_folder = instantiate_matches.get_one::<String>("generated").unwrap();
      // if Path::new(rust_folder).exists() {
      //   fs::remove_dir_all(rust_folder)?;
      // }
      // fs::create_dir(rust_folder)
      //     .with_context(|| "Failed to create directory ".to_owned() + rust_folder)?;

      let mut concrete_primitives: HashSet<String> = HashSet::new();
      concrete_primitives.insert("bool".to_owned());
      concrete_primitives.insert("char".to_owned());
      concrete_primitives.insert("f32".to_owned());
      concrete_primitives.insert("f64".to_owned());
      concrete_primitives.insert("f128".to_owned());
      concrete_primitives.insert("i128".to_owned());
      concrete_primitives.insert("i16".to_owned());
      concrete_primitives.insert("i32".to_owned());
      concrete_primitives.insert("i64".to_owned());
      concrete_primitives.insert("i8".to_owned());
      concrete_primitives.insert("isize".to_owned());
      // concrete_primitives.insert("str".to_owned()); because its not really a primitive to C
      concrete_primitives.insert("u128".to_owned());
      concrete_primitives.insert("u16".to_owned());
      concrete_primitives.insert("u32".to_owned());
      concrete_primitives.insert("u64".to_owned());
      concrete_primitives.insert("u8".to_owned());
      concrete_primitives.insert("usize".to_owned());

      let mut original_str_to_alias: HashMap<String, String> = HashMap::new();
      let mut alias_to_original_str: HashMap<String, String> = HashMap::new();
      let mut original_type_str_and_parsed_type_and_original_line_and_maybe_alias: Vec<(String, ParsedFullType, String, Option<String>)> = Vec::new();
      let mut original_func_str_and_parsed_type_and_original_line_and_maybe_alias: Vec<(String, ParsedFullType, String, Option<String>)> = Vec::new();

      let maybe_input_file_path = instantiate_matches.get_one::<String>("input_file");

      let mut lines: Vec<String> = Vec::new();
      if let Some(input_file_path) = maybe_input_file_path {
        if input_file_path.ends_with(".vast") {
          // TODO: handle vast directly?
        } else {
          if !(input_file_path.ends_with(".h") || input_file_path.ends_with(".c") || input_file_path.ends_with(".cpp")) {
            eprintln!("Input file doesn't end with .vast, .h, .c, or .cpp. Assuming C, proceeding...");
          }
          let file = File::open(input_file_path)?;
          let reader = io::BufReader::new(file);
          for line_res in reader.lines() {
            let line = line_res?;
            if Regex::new(r#"^#pragma\s+rs(use|fn)"#).unwrap().is_match(&line) {
              lines.push(line);
            }
          }
        }
      } else {
        for line in io::stdin().lock().lines() {
          lines.push(line?);
        }
      }

      let mut alias_to_original_full_type: HashMap<String, ParsedFullType> = HashMap::new();

      for line in lines {
        let line = line.trim().to_string();

        let maybe_aliasing_line_captures =
            Regex::new(r#"^\s*(#pragma\s+rs(use|fn)\s+)?(\w+)\s+=\s+(\S.+)\s*$"#).unwrap()
                .captures(&line);
        let (is_fn, target_type_str, maybe_alias) =
            if let Some(aliasing_line_captures) = maybe_aliasing_line_captures {
              let type_type_str =
                  aliasing_line_captures.get(2)
                      .expect("Bad rsuse/rsfn line")
                      .as_str().to_string();
              let is_fn =
                match type_type_str.as_str() {
                  "use" => false,
                  "fn" => true,
                  _ => panic!("Bad rsuse/rsfn line")
                };
              let maybe_alias =
                  aliasing_line_captures.get(3).map(|x| x.as_str().to_string());
              let target_type_str =
                  aliasing_line_captures.get(4)
                      .expect("Blork")
                      .as_str();
              (is_fn, target_type_str, maybe_alias)
            } else {
              let simple_line_captures =
                  Regex::new(r#"^\s*(#pragma\s+rs(use|fn)\s+)?(\S.+)\s*$"#).unwrap()
                      .captures(&line)
                      .expect(&("Bad line: ".to_owned() + &line));
              let type_type_str =
                  simple_line_captures.get(2)
                      .expect("Bad rsuse/rsfn line")
                      .as_str().to_string();
              let is_fn =
                  match type_type_str.as_str() {
                    "use" => false,
                    "fn" => true,
                    _ => panic!("Bad rsuse/rsfn line")
                  };
              let target_type_str =
                  simple_line_captures.get(3)
                      .expect("Blork")
                      .as_str();
              (is_fn, target_type_str, None)
            };

        if let Some(alias) = &maybe_alias {
          original_str_to_alias.insert(target_type_str.to_owned(), alias.clone());
          alias_to_original_str.insert(alias.clone(), target_type_str.to_owned());
        }
        println!("Adding {:?}", target_type_str);
        if is_fn {
          let (func_full_name, _, rest) =
              parse_full_type(&concrete_primitives, &alias_to_original_full_type, &HashMap::new(), target_type_str)?;
          original_func_str_and_parsed_type_and_original_line_and_maybe_alias.push(
            (target_type_str.to_owned(), func_full_name.clone(), line.clone(), maybe_alias.clone()));
          if let Some(alias) = &maybe_alias {
            alias_to_original_full_type.insert(alias.clone(), func_full_name);
          }
        } else {
          let (type_full_name, _, rest) =
              parse_full_type(&concrete_primitives, &alias_to_original_full_type, &HashMap::new(), target_type_str)?;
          assert!(rest.len() == 0); // DO NOT SUBMIT
          original_type_str_and_parsed_type_and_original_line_and_maybe_alias.push(
            (target_type_str.to_owned(), type_full_name.clone(), line.clone(), maybe_alias.clone()));
          if let Some(alias) = &maybe_alias {
            alias_to_original_full_type.insert(alias.clone(), type_full_name);
          }
        }
      }

      if !original_str_to_alias.contains_key("&str") {
        let parsed_type =
            ParsedFullType{
              steps: vec![
                ParsedType::Ref{
                  mutable: false,
                  inner: ParsedFullType {
                      steps: vec![
                        ParsedType::Primitive("str".to_owned())
                      ]
                    }
                }]
            };
        original_str_to_alias.insert("&str".to_owned(), "rust_str_ref".to_owned());
        alias_to_original_str.insert("rust_str_ref".to_owned(), "&str".to_owned());
        original_type_str_and_parsed_type_and_original_line_and_maybe_alias.push(
          ("&str".to_owned(), parsed_type, "(builtin)".to_owned(), Some("rust_str_ref".to_owned())));
      }
      let str_ref_alias = original_str_to_alias.get("&str").unwrap();

      let mut cbindgen_toml_contents = String::with_capacity(1000);
      cbindgen_toml_contents += "include_guard = \"EXAMPLE_PROJECT_H\"\n";
      cbindgen_toml_contents += "include_version = true\n";
      cbindgen_toml_contents += "language = \"C\"\n";
      cbindgen_toml_contents += "cpp_compat = true\n";
      cbindgen_toml_contents += "\n";
      cbindgen_toml_contents += "header = \"\"\"\n";
      cbindgen_toml_contents += "#define VALIGN(n) __attribute__ ((aligned(n)))\n";
      cbindgen_toml_contents += "#include <stdalign.h>\n";
      cbindgen_toml_contents += "\"\"\"\n";
      cbindgen_toml_contents += "\n";
      cbindgen_toml_contents += "[layout]\n";
      cbindgen_toml_contents += "aligned_n = \"VALIGN\"\n";

      fs::write(output_dir_path.to_owned() + "/cbindgen.toml", cbindgen_toml_contents)
          .with_context(|| "Failed to write ".to_owned() + output_dir_path + "/Cargo.toml")?;

      let mut primitives: HashSet<String> = HashSet::new();
      primitives.insert("&str".to_owned()); // TODO: add more

      let mut drop_func_type_original_str_to_maybe_drop_func_alias: HashMap<String, Option<String>> = HashMap::new();
      let mut scouting_strings: Vec<String> = Vec::new();
      // Drop functions expect this kind of return type
      scouting_strings.push(get_type_sizer_string("()", &ParsedFullType { steps: vec![ParsedType::Tuple {generic_args: Vec::new()}] }));

      // TODO: Expensive call to string_path
      original_type_str_and_parsed_type_and_original_line_and_maybe_alias.sort_by_key(|x| x.0.clone());
      original_func_str_and_parsed_type_and_original_line_and_maybe_alias.sort_by_key(|x| x.0.clone());
      for (original_type_str, parsed_type, line, _maybe_alias) in &original_type_str_and_parsed_type_and_original_line_and_maybe_alias {
        println!("Sizing type {}", &original_type_str);
        scouting_strings.push(get_type_sizer_string(original_type_str, parsed_type));
      }
      for (original_func_str, parsed_type, line, maybe_alias) in &original_func_str_and_parsed_type_and_original_line_and_maybe_alias {
        let last = parsed_type.steps.last().unwrap();
        match last {
          ParsedType::Ref { .. } => panic!("wat"),
          ParsedType::Primitive(_) => panic!("wat"),
          ParsedType::Lifetime => panic!("wat"),
          ParsedType::Wildcard => panic!("wat"),
          ParsedType::ImplCast { .. } => panic!("wat"),
          ParsedType::Alias(name) => unimplemented!(),
          ParsedType::Tuple { .. } => {}// continue
          ParsedType::Slice { .. } => {}// continue
          ParsedType::Value { name, generic_args, params } => {
            if name == "drop" {
              // // Make sure to resolve the type.
              // // TODO: This might be redundant.
              let param_full_type = full_type_get_init(parsed_type);
              println!("Sizing for drop function {}, type {}", &original_func_str, str_for_full_type(&param_full_type));
              // There wasn't an original name for it, so just generate one.
              let drop_type_original_name = sizify_full_type(&param_full_type);
              scouting_strings.push(get_type_sizer_string(&drop_type_original_name, &param_full_type));
              // Don't proceed to ask rustc anything about drop functions, because we can't
              // directly address drop functions. We get something like this:
              //   error[E0599]: no function or associated item named `drop` found for struct `OsString` in the current scope
              //      --> src/main.rs:62:80
              //   62  |   println!("fn {}", "std::ffi::OsString::drop");  print_fn(std::ffi::OsString::drop);
              //       note: if you're trying to build a new `OsString` consider using one of the following associated functions:
              // Instead, we'll just manually create them later.
              drop_func_type_original_str_to_maybe_drop_func_alias.insert(drop_type_original_name, maybe_alias.clone());
              continue;
            }
          }
        }
        println!("Sizing function {}", &original_func_str);
        scouting_strings.push(get_func_scouting_string(original_func_str, parsed_type));
      }

      println!("Running sizer program on {} types...", scouting_strings.len());

      let sizer_program_str =
          std::iter::once(sizer_preamble().to_owned()).into_iter()
              .chain(std::iter::once("fn main() {".to_owned()))
              .chain(scouting_strings)
              .chain(std::iter::once("}".to_owned()))
              .collect::<Vec<String>>()
              .join("\n");

      if Path::new(&(output_dir_path.to_owned() + "/src/lib.rs")).exists() {
        fs::remove_file(output_dir_path.to_owned() + "/src/lib.rs")
            .with_context(|| "Failed to remove ".to_owned() + output_dir_path + "/src/lib.rs")?;
      }

      let sizer_program_output_str =
          get_rust_program_output(&cargo_path, output_dir_path, &sizer_program_str)?;
      let mut type_rust_str_to_size_and_alignment: HashMap<String, (usize, usize)> = HashMap::new();
      let mut func_rust_str_to_ret_str_and_params_strs: HashMap<String, (String, Vec<String>)> = HashMap::new();

      let mut original_str_to_rust_str: HashMap<String, String> = HashMap::new();
      let mut rust_str_to_original_str: HashMap<String, String> = HashMap::new();
      let mut rust_str_to_alias: HashMap<String, String> = HashMap::new();
      let mut alias_to_rust_str: HashMap<String, String> = HashMap::new();

      println!("Got sizer program output:\n{}", sizer_program_output_str);

      let lines = sizer_program_output_str.split("\n").collect::<Vec<_>>();
      let mut root_line_i = 0;
      while root_line_i < lines.len() {
        let root_line = lines[root_line_i];

        let original_str =
            if let Some(first_space_pos) = root_line.find(" ") {
              root_line[first_space_pos..].trim()
            } else {
              return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, "Bad line from sizer program: ".to_string() + root_line)));
            };

        let mut next_root_line_i = root_line_i + 1;
        while next_root_line_i < lines.len() && lines[next_root_line_i].starts_with("  ") {
          next_root_line_i += 1;
        }

        let rust_str =
          if root_line.starts_with("type") {
            assert!(root_line_i + 1 < lines.len());
            let (size, alignment, rust_type_str) = parse_type_info_line(lines[root_line_i + 1].trim())?;

            type_rust_str_to_size_and_alignment.insert(rust_type_str.to_owned(), (size, alignment));

            if let Some(maybe_drop_func_alias) = drop_func_type_original_str_to_maybe_drop_func_alias.get(original_str) {
              let func_str = rust_type_str.to_owned() + "::drop";
              let ret_type_rust_str = "()".to_owned();
              let param_types_rust_strs = vec![rust_type_str.to_owned()];
              func_rust_str_to_ret_str_and_params_strs.insert(
                func_str.to_owned(), (ret_type_rust_str.to_owned(), param_types_rust_strs));
              if let Some(alias) = maybe_drop_func_alias {
                alias_to_rust_str.insert(alias.to_string(), func_str.to_string());
                rust_str_to_alias.insert(func_str.to_string(), alias.to_string());
              }
            }

            rust_type_str
          } else if root_line.starts_with("fn") {
            let (num_params, func_str_unturboed) = parse_func_info_line(lines[root_line_i + 1].trim())?;
            // Strings start with < if they're talking about impls, hence not doing the replace on that first one.
            let func_str =
                turbofishify(&func_str_unturboed)
                    // Rust's type_name shows this for any impl str method, like str::chars.
                    .replace("core::str::<impl str>::", "str::");

            let (ret_size, ret_alignment, ret_type_rust_str) = parse_type_info_line(lines[root_line_i + 2].trim())?;
            type_rust_str_to_size_and_alignment.insert(ret_type_rust_str.to_owned(), (ret_size, ret_alignment));

            let mut param_types_rust_strs = Vec::new();
            for param_line_i in (root_line_i + 3)..next_root_line_i {
              let (param_size, param_alignment, param_rust_type_str) = parse_type_info_line(lines[param_line_i].trim())?;
              type_rust_str_to_size_and_alignment.insert(param_rust_type_str.to_owned(), (param_size, param_alignment));
              param_types_rust_strs.push(param_rust_type_str.to_owned());
            }

            func_rust_str_to_ret_str_and_params_strs.insert(
              func_str.to_owned(), (ret_type_rust_str.to_owned(), param_types_rust_strs));
            func_str
          } else {
            panic!("Bad output from sizer program: {}", root_line);
          };

        rust_str_to_original_str.insert(rust_str.clone(), original_str.to_owned());
        original_str_to_rust_str.insert(original_str.to_owned(), rust_str.clone());

        if let Some(alias) = original_str_to_alias.get(original_str) {
          alias_to_rust_str.insert(alias.to_string(), rust_str.to_string());
          rust_str_to_alias.insert(rust_str.to_string(), alias.to_string());
        }

        root_line_i = next_root_line_i;
      }

      let mut alias_to_parsed_type: HashMap<String, ParsedFullType> = HashMap::new();
      let mut parsed_type_to_alias: HashMap<ParsedFullType, String> = HashMap::new();
      for (type_rust_str, (size, alignment)) in &type_rust_str_to_size_and_alignment {
        if let Some(alias) = rust_str_to_alias.get(type_rust_str) {
          // Handling in an empty map for aliases is fine because these come straight from Rust and
          // don't mention aliases.
          let (type_, _, _) = parse_full_type(&concrete_primitives, &HashMap::new(), &HashMap::new(), &type_rust_str)?;
          alias_to_parsed_type.insert(alias.to_string(), type_.clone());
          parsed_type_to_alias.insert(type_.clone(), alias.to_string());
        }
      }
      for (func_rust_str, (size, alignment)) in &type_rust_str_to_size_and_alignment {
        if let Some(alias) = rust_str_to_alias.get(func_rust_str) {
          // Handling in an empty map for aliases is fine because these come straight from Rust and
          // don't mention aliases.
          let (type_, _, _) = parse_full_type(&concrete_primitives, &HashMap::new(), &HashMap::new(), &func_rust_str)?;
          alias_to_parsed_type.insert(alias.to_string(), type_.clone());
          parsed_type_to_alias.insert(type_.clone(), alias.to_string());
        }
      }

      for (key, val) in &parsed_type_to_alias {
        eprintln!("alias type key: {:?}", str_for_full_type(&key));
      }

      let mut crates = HashMap::new();
      crates.insert("std".to_string(), indexer::get_crate(&rustc_sysroot_path, &cargo_path, &output_dir_path, "std")?);
      crates.insert("alloc".to_string(), indexer::get_crate(&rustc_sysroot_path, &cargo_path, &output_dir_path, "alloc")?);
      crates.insert("core".to_string(), indexer::get_crate(&rustc_sysroot_path, &cargo_path, &output_dir_path, "core")?);
      indexer::get_dependency_crates(&rustc_sysroot_path, &cargo_path, &output_dir_path, &cargo_toml_path, &mut crates)?;
      let item_index = indexer::genealogize(&crates)?;

      let mut type_rust_str_to_info: HashMap<String, TypeInfo> = HashMap::new();
      let mut func_rust_str_to_info: HashMap<String, FuncInfo> = HashMap::new();
      for (func_rust_str, (ret_type_rust_str, param_types_rust_strs)) in func_rust_str_to_ret_str_and_params_strs {
        let (type_without_aliasing, type_with_aliasing, _) =
            parse_full_type(&concrete_primitives, &HashMap::new(), &parsed_type_to_alias, &func_rust_str)?;
        // let (type_with_aliasing, _) = parse_full_type(&concrete_primitives, &parsed_type_to_alias, &func_rust_str)?;
        let public_type = determine_public_type(&crates, &item_index, &type_without_aliasing)?;
        let c_name =
            rust_str_to_alias.get(&func_rust_str).map(|x| x.to_owned())
                .unwrap_or(get_pointered_prefixed_mangled_type(&type_with_aliasing));
        func_rust_str_to_info.insert(
          func_rust_str,
          FuncInfo {
              canonical_type: type_without_aliasing,
              public_type,
              c_name,
              ret_type_rust_str,
              param_types_rust_strs
            });
      }
      for (type_rust_str, (size, alignment)) in type_rust_str_to_size_and_alignment {
        let (type_without_aliasing, type_with_aliasing, _) =
            parse_full_type(&concrete_primitives, &HashMap::new(), &parsed_type_to_alias, &type_rust_str)?;
        eprintln!("Just parsed canonical {:?}, aliasing {:?}", str_for_full_type(&type_without_aliasing), str_for_full_type(&type_with_aliasing));
        // let (type_with_aliasing, _) = parse_full_type(&concrete_primitives, &parsed_type_to_alias, &type_rust_str)?;
        let public_type = determine_public_type(&crates, &item_index, &type_without_aliasing)?;
        let c_name =
            rust_str_to_alias.get(&type_rust_str).map(|x| x.to_owned())
                .unwrap_or(get_pointered_prefixed_mangled_type(&type_with_aliasing));
        type_rust_str_to_info.insert(
          type_rust_str,
          TypeInfo {
              canonical_type: type_without_aliasing,
              public_type,
              c_name,
              size,
              alignment
            });
      }


      let mut struct_strings: Vec<String> = Vec::new();
      let mut func_strings: Vec<String> = Vec::new();

      // type_and_original_line_and_type_str_and_maybe_alias
      //     .sort_by_key(|x| x.0.valtype.id.id.0.clone());

      for (rust_type_str, type_info) in &type_rust_str_to_info {
        match type_info.canonical_type.steps.last().unwrap() {
          ImplCast { .. } => unimplemented!(),
          ParsedType::Lifetime => unimplemented!(),
          ParsedType::Wildcard => unimplemented!(),
          ParsedType::Alias(name) => unimplemented!(),
          ParsedType::Ref { mutable, inner } => {
            eprintln!("TODO: put better logic in for whether something's sized");
            let sized =
              match inner.steps.last().unwrap() {
                ParsedType::Slice { inner: inner_inner } => false,
                ParsedType::Value { name, generic_args, params } => {
                  match name.as_str() {
                    "str" => false,
                    _ => true
                  }
                },
                _ => true,
              };
            if sized {
              continue; // Skip these, we don't need struct wrappers for pointers.
            } else {
              // If it's not sized, then its like a slice, and we do want a struct wrapper for that.
            }
          },
          ParsedType::Primitive(name) => continue,
          ParsedType::Value { .. } => {}
          ParsedType::Tuple { .. } => {}
          ParsedType::Slice { .. } => {}
        }
        struct_strings.push(
          instantiate_struct(type_info)?);
      }

      for (rust_type_str, func_info) in &func_rust_str_to_info {
        let maybe_alias = rust_str_to_alias.get(rust_type_str).map(|x| x.as_str());
        func_strings.push(instantiate_func(&type_rust_str_to_info, func_info)?);
      }

      if let Some(output_sizes_path) = maybe_output_sizes_path {
        let output_sizes_str =
            original_str_to_rust_str
                .iter()
                .map(|(original_str, rust_str)| {
                  if let Some(info) = type_rust_str_to_info.get(rust_str) {
                    let mangled = mangle_full_type(&info.public_type);
                    format!("type/{}/{}/{}/{}\n", original_str, mangled, info.size, info.alignment)
                  } else if let Some(info) = func_rust_str_to_info.get(rust_str) {
                    let mangled = mangle_full_type(&info.public_type);
                    let mut result_str = format!("fn/{}/{}/{}", original_str, mangled, info.ret_type_rust_str);
                    for param_str in &info.param_types_rust_strs {
                      result_str = result_str + "/" + &param_str;
                    }
                    result_str + "\n"
                  } else {
                    panic!("original str not fn or type?");
                  }
                })
                .collect::<Vec<_>>()
                .join("");
        fs::write(output_sizes_path, output_sizes_str)
            .with_context(|| "Failed to write ".to_string() + output_sizes_path)?;
      }

      let final_program_str =
          [common_preamble().to_owned(), instantiations_preamble(str_ref_alias).to_owned()].into_iter()
              .chain(struct_strings.into_iter())
              .chain(func_strings.into_iter())
              .collect::<Vec<String>>()
              .join("\n");

      fs::write(output_dir_path.to_string() + "/src/capi.rs", final_program_str)
          .with_context(|| "Failed to write ".to_string() + output_dir_path + "/src/capi.rs")?;

      fs::write(
        output_dir_path.to_string() + "/src/lib.rs",
        r#"
#![feature(os_str_display)]

#[cfg(feature = "capi")]
mod capi;
"#)
          .with_context(|| "Failed to write ".to_string() + output_dir_path + "/src/lib.rs")?;

      fs::remove_file(output_dir_path.to_string() + "/src/main.rs")
          .with_context(|| "Failed to remove ".to_string() + output_dir_path + "/src/main.rs")?;

      let output = Command::new(&cargo_path)
          .args(&[
            "+nightly",
            "cbuild",
            "--release",
            &("--manifest-path=".to_string() + output_dir_path + "/Cargo.toml"),
            "--destdir=clibthing",
            "--target", "aarch64-apple-darwin",
            "-Z", "build-std-features=panic_immediate_abort",
            "-Z", "build-std=std,panic_abort",
            "--library-type", "staticlib"])
          .env("RUSTFLAGS", "-Zlocation-detail=none")
          .output()
          .with_context(|| "Failed to execute cbuild command")?;
      if output.status.code() == Some(0) {
        // Continue
      } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let error = "Error from cbuild command: ".to_string() + &stderr;
        return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, error)));
      }

      let output = Command::new(&cargo_path)
          .args(&[
            "cinstall",
            &("--manifest-path=".to_string() + output_dir_path + "/Cargo.toml"),
            "--destdir=clibthing",
            "--library-type", "staticlib"])
          .output()
          .with_context(|| "Failed to execute cbuild command")?;
      if output.status.code() == Some(0) {
        // Continue
      } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let error = "Error from cinstall command: ".to_string() + &stderr;
        return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, error)));
      }
    }
    _ => {
      unimplemented!();
    }
  }

  // println!("{:?}", v);

  return Ok(());
}

fn turbofishify(rust_type_str_unturboed: &str) -> String {
  // Strings start with < if they're talking about impls, hence not doing the replace on that first one.
  rust_type_str_unturboed[0..1].to_string() +
      &rust_type_str_unturboed[1..].replace("<", "::<").replace("::::<", "::<")
}

fn full_type_get_init(parsed_type: &ParsedFullType) -> ParsedFullType {
  ParsedFullType { steps: parsed_type.steps[0..parsed_type.steps.len() - 1].into_iter().map(|x| x.clone()).collect::<Vec<_>>() }
}

fn determine_public_type(
  crates: &HashMap<String, Crate>,
  item_index: &ItemIndex,
  canonical_type: &ParsedFullType
) -> anyhow::Result<ParsedFullType> {
  let s = sizify_full_type(canonical_type);
  eprintln!("Doing type {:?}", s);
  let mut result_type = ParsedFullType { steps: Vec::new() };
  let mut previous: Option<UId> = None;
  for (step_i, step) in canonical_type.steps.iter().enumerate() {
    // let uid =
    match step {
      ParsedType::Alias(name) => unimplemented!(),
      ParsedType::Primitive(_) => {
        result_type.steps.push(step.clone());
      },
      ParsedType::Ref { mutable, inner } => {
        result_type.steps.push(
          ParsedType::Ref {
            mutable: *mutable,
            inner: determine_public_type(crates, item_index, inner)?
          });
      },
      ParsedType::Tuple { generic_args } => {
        let mut new_generic_args = Vec::new();
        for generic_arg in generic_args {
          new_generic_args.push(
            determine_public_type(crates, item_index, generic_arg)?);
        }
        result_type.steps.push(ParsedType::Tuple { generic_args: new_generic_args });
      },
      ParsedType::Slice { inner } => {
        result_type.steps.push(
          ParsedType::Slice {
            inner: determine_public_type(crates, item_index, inner)?
          });
      },
      ParsedType::Value { name, generic_args, params } => {
        match extend_and_resolve_uid(crates, &item_index.primitive_name_to_uid, previous.as_ref(), name) {
          Ok(uid) => {
            previous = Some(uid.clone());

            // sanity check
            match &lookup_uid(crates, &uid).inner {
              ItemEnum::Module(m) => {
                if m.is_stripped {
                  // Don't add this step to the final type, since its members were re-exported.
                  unimplemented!(); // curious
                }
              }
              _ => {}
            }

            let mut new_generic_args = Vec::new();
            for generic_arg in generic_args {
              new_generic_args.push(
                determine_public_type(crates, item_index, generic_arg)?);
            }
            let mut new_params = Vec::new();
            for param in params {
              new_params.push(
                determine_public_type(crates, item_index, param)?);
            }

            result_type.steps.push(ParsedType::Value {
              name: name.clone(),
              generic_args: new_generic_args,
              params: new_params
            });
          }
          Err(ResolveError::NotFound) => {
            // Sometimes when we get one of these path steps, it's actually a private module
            // such as the unwind_safe in core::panic::unwind_safe::RefUnwindSafe.
            // The RefUnwindSafe was actually pub use'd in core::panic.
            // If this isn't the last step, let's skip it and see if things work.
            // TODO: document better or something? seems sketchy.
            // Also, this seems like it has something to do with Module's is_stripped
            // field, but there's not really a good way to use that to help, AFAICT.
            continue;
          }
          Err(ResolveError::ResolveFatal(fatal)) => return Err(fatal)
        }
      }
      ImplCast { struct_, impl_ } => {
        // All impl methods are public, so we don't actually need to do anything here.
        // We can just use the struct's name.
        // Though, it does mean that things are ambiguous now, for example, we're turning:
        //   <std::ffi::os_str::OsString as core::convert::From<&str>>::from
        // into:
        //   std::ffi::os_str::OsString::from
        let mut struct_canonical_type = determine_public_type(crates, item_index, struct_)?;
        result_type.steps.append(&mut struct_canonical_type.steps);
        result_type.steps.append(&mut canonical_type.steps[(step_i + 1)..].iter().map(|x| x.clone()).collect::<Vec<ParsedType>>());
        eprintln!("Determined canonical: {:?}", sizify_full_type(&result_type));
        return Ok(result_type);
      }
      ParsedType::Lifetime => unimplemented!(),
      ParsedType::Wildcard => unimplemented!(),
    }
  }
  eprintln!("Determined canonical: {:?}", sizify_full_type(&result_type));
  return Ok(result_type);
}

fn parse_func_info_line(info_line: &str) -> Result<(usize, String)> {
  if let Some(first_space_pos) = info_line.find(" ") {
    let arity_str = &info_line[0..first_space_pos];
    match arity_str.parse::<usize>() {
      Ok(arity) => {
        let func_str = &info_line[first_space_pos..].trim();
        return Ok((arity, turbofishify(func_str)));
      },
      Err(e) => {}
    }
  }
  return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, format!("Bad func info line: {}", info_line))));
}

fn parse_type_info_line(info_line: &str) -> Result<(usize, usize, String)> {
  if let Some(first_space_pos) = info_line.find(" ") {
    let size_str = &info_line[0..first_space_pos];
    match size_str.parse::<usize>() {
      Ok(size) => {
        let line_after_size = info_line[first_space_pos..].trim();
        if let Some(second_space_pos_in_line_after_size) = line_after_size.find(" ") {
          let alignment_str = &line_after_size[0..second_space_pos_in_line_after_size];
          match alignment_str.parse::<usize>() {
            Ok(alignment) => {
              let type_str = &line_after_size[second_space_pos_in_line_after_size..].trim();
              return Ok((size, alignment, turbofishify(type_str)));
            }
            Err(E) => {}
          }
        }
      },
      Err(e) => {}
    }
  }
  return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, format!("Bad type info line: {}", info_line))));
}

fn setup_output_dir(cargo_toml_path: &String, output_dir_path: &String) -> Result<()> {
  if !Path::new(&output_dir_path).exists() {
    fs::create_dir(&output_dir_path)
        .with_context(|| "Failed to create ".to_owned() + output_dir_path + " directory")?;
  }

  let cargo_toml_contents =
      fs::read_to_string(&cargo_toml_path)
          .with_context(|| "Failed to read Cargo toml at given path: ".to_owned() + &cargo_toml_path)?;

  fs::write(output_dir_path.to_owned() + "/Cargo.toml", cargo_toml_contents)
      .with_context(|| "Failed to write ".to_owned() + output_dir_path + "/Cargo.toml")?;

  if !Path::new(&(output_dir_path.to_string() + "/src")).exists() {
    fs::create_dir(output_dir_path.to_string() + "/src")
        .with_context(|| "Failed to create ".to_owned() + output_dir_path + "/src directory")?;
  }
  // Cargo need something in main.rs or lib.rs to even be able to *parse* the toml.
  fs::write(output_dir_path.to_string() + "/src/main.rs", "")
      .with_context(|| "Failed to write ".to_string() + output_dir_path + "/src/main.rs")?;

  Ok(())
}

fn sizer_preamble() -> String {
  common_preamble() +
  r#"
fn print_type<T>() {
    println!("  {} {} {}", std::mem::size_of::<T>(), std::mem::align_of::<T>(), std::any::type_name::<T>());
}

trait PrintFn<Args> {
    fn print_fn();
}

fn print_fn<T: PrintFn<Args>, Args>(_: T) {
    T::print_fn()
}

impl <F, R> PrintFn<(R, )> for F where F: Fn() -> R {
    fn print_fn() {
        println!("  0 {}", std::any::type_name::<Self>());
        print_type::<R>();
    }
}

impl <F, R, P1> PrintFn<(R, P1, )> for F where F: Fn(P1) -> R {
    fn print_fn() {
        println!("  1 {}", std::any::type_name::<Self>());
        print_type::<R>();
        print_type::<P1>();
    }
}

impl <F, R, P1, P2> PrintFn<(R, P1, P2, )> for F where F: Fn(P1, P2) -> R {
    fn print_fn() {
        println!("  2 {}", std::any::type_name::<Self>());
        print_type::<R>();
        print_type::<P1>();
        print_type::<P2>();
    }
}

fn select_overload_printable_1<R, P1>(thing: impl Fn(P1) -> R + PrintFn<(R, P1, )>) -> impl Fn(P1) -> R + PrintFn<(R, P1,)> {
    thing
}
fn select_overload_printable_2<R, P1, P2>(thing: impl Fn(P1, P2) -> R + PrintFn<(R, P1, P2, )>) -> impl Fn(P1, P2) -> R + PrintFn<(R, P1, P2,)> {
    thing
}
"#
}

fn common_preamble() -> String {
  r#"
#![feature(os_str_display)]

use static_assertions::const_assert_eq;
use std::mem;
extern crate alloc;
use core;
use core::ffi::c_char;
"#.to_owned()
}

fn instantiations_preamble(str_ref_alias: &str) -> String {
  r#"
#[no_mangle]
pub extern "C" fn rust_StrFromCStr(char_ptr: *const c_char) -> rust_str_ref {
  let c_str = unsafe { core::ffi::CStr::from_ptr(char_ptr) };
  if let Ok(rust_str) = c_str.to_str() {
    let s_rs: rust_str_ref = unsafe { mem::transmute(rust_str) };
    return s_rs;
  } else {
    panic!("Error: c_str.to_str() failed.");
  }
}

// TODO: Is it okay to use u8 here instead of c_char?
#[no_mangle]
pub extern "C" fn rust_StrNew(length: usize, char_ptr: *const u8) -> rust_str_ref {
  let c_str = unsafe { std::slice::from_raw_parts(char_ptr, length) };
  if let Ok(rust_str) = core::ffi::CStr::from_bytes_with_nul(c_str) {
    if let Ok(rust_str) = rust_str.to_str() {
      let s_rs: rust_str_ref = unsafe { mem::transmute(rust_str) };
      return s_rs;
    } else {
      panic!("Error: c_str.to_str() failed.");
    }
  } else {
    panic!("Error: CStr::from_bytes_with_nul() failed.");
  }
}

#[no_mangle]
pub extern "C" fn rust_StrToCStr(str_c: rust_str_ref) -> *const c_char {
  let str_rs: &str = unsafe { mem::transmute(str_c) };
  let ptr = str_rs.as_ptr() as *const c_char;
  return ptr;
}

#[no_mangle]
pub extern "C" fn rust_StrLen(str_c: rust_str_ref) -> usize {
  let str_rs: &str = unsafe { mem::transmute(str_c) };
  let len: usize = str_rs.len();
  return len;
}

"#.replace("rust_str_ref", str_ref_alias)
}

enum ResolveError {
  NotFound,
  ResolveFatal(anyhow::Error)
}

fn sizify_type(type_: &ParsedType) -> String {
  match type_ {
    ParsedType::Alias(name) => unimplemented!(),
    ParsedType::ImplCast { struct_, impl_ } => {
      sizify_full_type(struct_)
    }
    ParsedType::Ref { mutable, inner } => {
      "&".to_string() + (if *mutable { "mut " } else { "" }) + &sizify_full_type(inner)
    }
    ParsedType::Tuple { generic_args} => {
      "(".to_owned() +
          &generic_args.into_iter().map(|x| sizify_full_type(x)).collect::<Vec<_>>().join(", ") +
          ")"
    }
    ParsedType::Slice { inner} => {
      "[".to_owned() + &sizify_full_type(inner) + "]"
    }
    ParsedType::Value { name, generic_args, params } => {
      name.to_owned() +
      &(if generic_args.len() > 0 {

        "::<".to_owned() +
        &generic_args
            .iter()
            .filter(|x| {
              // Can't specify lifetimes explicitly, for example:
              //   error[E0794]: cannot specify lifetime arguments explicitly if late bound lifetime parameters are present
              //      --> src/main.rs:69:87
              //       |
              //   69  |   println!("fn {}", "Regex::captures::<'static>");  print_fn(regex::Regex::captures::<'static>);
              //       |                                                                                       ^^^^^^^
              // so we just take them out.
              **x != (ParsedFullType { steps: vec![ParsedType::Lifetime] })
            })
            .map(sizify_full_type)
            .collect::<Vec<_>>()
            .join(", ") +
        ">"
      } else {
        "".to_owned()
      })
    }
    ParsedType::Primitive(name) => name.to_owned(),
    ParsedType::Lifetime => "'static".to_owned(),
    ParsedType::Wildcard => "_".to_owned(),
  }
}

fn sizify_full_type(full_type: &ParsedFullType) -> String {
  let steps = &full_type.steps;
  steps.into_iter().map(sizify_type).collect::<Vec<_>>().join("::")
}

fn sizify_func(full_type: &ParsedFullType) -> String {
  let inner = sizify_full_type(full_type);
  let last_step = full_type.steps.last().unwrap();
  match last_step {
    ParsedType::Alias(name) => unimplemented!(),
    ParsedType::ImplCast { .. } => panic!("wat"),
    ParsedType::Ref { .. } => panic!("Func last step is a ref?"),
    ParsedType::Primitive(_) => panic!("Func last step is a primitive?"),
    ParsedType::Lifetime => panic!("Func last step is a lifetime?"),
    ParsedType::Wildcard => panic!("Func last step is a wildcard?"),
    ParsedType::Slice { .. } => panic!("Func last step is a slice?"),
    ParsedType::Tuple { .. } => panic!("Func last step is a tuple?"),
    ParsedType::Value { name, generic_args, params } => {
      if params.len() > 0 {
        "select_overload_printable_".to_owned() +
        &params.len().to_string() +
        "::<_, " +
        &params.into_iter().map(sizify_full_type).collect::<Vec<_>>().join(", ") +
        ">(" +
        &inner +
        ")"
      } else {
        inner
      }
    }
  }
}

fn get_type_sizer_string(
  // This isn't necessarily the same as sizified_type.
  // For example, the user might have supplied std::vec::Vec<std::ffi::OsString>
  // and the sizified_type might be std::vec::Vec::<std::ffi::OsString>.
  original_str: &str,
  full_type: &ParsedFullType
) -> String {
  let sizified_type = sizify_full_type(full_type);
  let mut rust_src = String::with_capacity(1000);
  rust_src += "  println!(\"type {}\", \"";
  rust_src += original_str;
  rust_src += "\");";
  rust_src += "  print_type::<";
  rust_src += &sizified_type;
  rust_src += ">();";
  return rust_src;
}

fn get_func_scouting_string(
  // This isn't necessarily the same as sizified_type.
  // For example, the user might have supplied std::vec::Vec<std::ffi::OsString>::push
  // and the sizified_type might be std::vec::Vec::<std::ffi::OsString>::push.
  original_func_str: &str,
  full_type: &ParsedFullType
) -> String {
  let sizified_type = sizify_full_type(full_type);
  let sizified_func = sizify_func(full_type);
  let mut rust_src = String::with_capacity(1000);
  rust_src += "  println!(\"fn {}\", \"";
  rust_src += original_func_str;
  rust_src += "\");";
  rust_src += "  print_fn(";
  rust_src += &sizified_func;
  rust_src += ");";
  return rust_src;
}

fn get_rust_program_output(cargo_path: &String, output_dir_path: &String, rust_src: &str) -> anyhow::Result<String> {
  fs::write(output_dir_path.to_string() + "/src/main.rs", rust_src)
      .with_context(|| "Failed to write ".to_string() + output_dir_path + "/src/main.rs")?;

  let output = Command::new(cargo_path)
      .args(&["run", &("--manifest-path=".to_string() + output_dir_path + "/Cargo.toml")])
      .output()
      .with_context(|| "Failed to execute cargo run command")?;
  let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
  if output.status.code() == Some(0) {
    return Ok(stdout);
  } else {
    let stderr = String::from_utf8_lossy(&output.stderr);
    let error = "Error from cargo run command: ".to_string() + &stderr;
    return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, error)));
  }
}

fn instantiate_struct(
  info: &TypeInfo
) -> anyhow::Result<String> {
  let mut builder = String::with_capacity(1000);
  builder += "#[repr(C, align(";
  builder += &info.alignment.to_string();
  builder += "))]\n";
  // builder += "#[repr(C)]\n";
  builder += "pub struct ";
  builder += &info.c_name;
  builder += " (std::mem::MaybeUninit<[u8; ";
  builder += &info.size.to_string();
  builder += "]>);\n";
  builder += "const_assert_eq!(std::mem::size_of::<";
  builder += &info.c_name;
  builder += ">(), ";
  builder += &info.size.to_string();
  builder += ");\n";

  return Ok(builder);
}

fn instantiate_func(
  type_rust_str_to_info: &HashMap<String, TypeInfo>,
  info: &FuncInfo
) -> anyhow::Result<String> {
  let mut param_type_infos =
      info.param_types_rust_strs
          .iter()
          .map(|x| type_rust_str_to_info.get(x).unwrap())
          .collect::<Vec<&TypeInfo>>();
  let is_drop =
    match info.canonical_type.steps.last().unwrap() {
      ParsedType::Value { name, .. } => name == "drop",
      _ => false
    };
  if is_drop {
    let type_rust_str = sizify_full_type(&full_type_get_init(&info.canonical_type));
    param_type_infos[0] = type_rust_str_to_info.get(&type_rust_str).unwrap();
  }

  let ret_type_info = type_rust_str_to_info.get(&info.ret_type_rust_str).unwrap();

  let mut rust_builder = String::with_capacity(1000);
  rust_builder += "#[no_mangle]\n";
  rust_builder += "pub extern \"C\" fn ";
  rust_builder += &info.c_name;
  rust_builder += "(\n";
  for (param_type_rust_str, param_i) in info.param_types_rust_strs.iter().zip(0..info.param_types_rust_strs.len()) {
    let param_name = "  param_".to_owned() + &param_i.to_string() + &"_c";
    let param_c_type_str = &type_rust_str_to_info.get(param_type_rust_str).unwrap().c_name;
    rust_builder += &format!("  {}: {},\n", param_name, param_c_type_str);
  }
  rust_builder += ")";
  if !is_drop { // DO NOT SUBMIT can we instead check if its void
    // if let Some(return_type_simple) = &maybe_output_type {
    rust_builder += " -> ";
    rust_builder += &ret_type_info.c_name;
    // }
  }
  rust_builder += " {\n";
  for (param_type_str, param_i) in info.param_types_rust_strs.iter().zip(0..info.param_types_rust_strs.len()) {
    let c_param_name = "param_".to_owned() + &param_i.to_string() + &"_c";
    let rs_param_name = "param_".to_owned() + &param_i.to_string() + &"_rs";
    let param_type_info = type_rust_str_to_info.get(param_type_str).unwrap();

    rust_builder += "  const_assert_eq!(std::mem::size_of::<";
    rust_builder += &str_for_full_type(&param_type_info.public_type);
    rust_builder += ">(), std::mem::size_of::<";
    rust_builder += &param_type_info.c_name;
    rust_builder += ">());\n";

    rust_builder += "  let ";
    rust_builder += &rs_param_name;
    rust_builder += ": ";
    rust_builder += &str_for_full_type(&param_type_info.public_type);
    rust_builder += " = unsafe { mem::transmute(";
    rust_builder += &c_param_name;
    rust_builder += ") };\n";
  }

  if !is_drop {
    rust_builder += "  ";
    // if let Some(return_type_simple) = maybe_output_type {
      rust_builder += "let result_rs: ";
      rust_builder += &str_for_full_type(&ret_type_info.public_type);
      rust_builder += " = ";
    // }

    rust_builder += &caller_str_for_full_type(&info.public_type);
    rust_builder += "(";
    for (param_type_str, param_i) in info.param_types_rust_strs.iter().zip(0..info.param_types_rust_strs.len()) {
      let c_param_name = "param_".to_owned() + &param_i.to_string() + &"_c";
      let rs_param_name = "param_".to_owned() + &param_i.to_string() + &"_rs";
      let param_type_info = type_rust_str_to_info.get(param_type_str).unwrap();

      rust_builder += &rs_param_name;
      rust_builder += ",";
    }
    rust_builder += ");\n";

    // if let Some(return_type_simple) = &maybe_output_type {
      rust_builder += "  const_assert_eq!(std::mem::size_of::<";
      rust_builder += &str_for_full_type(&ret_type_info.public_type);
      rust_builder += ">(), std::mem::size_of::<";
      rust_builder += &ret_type_info.c_name;
      rust_builder += ">());\n";

      rust_builder += "  let result_c: ";
      rust_builder += &ret_type_info.c_name;
      rust_builder += " = unsafe { mem::transmute(result_rs) };\n";
      rust_builder += "  return result_c;\n";
    // }
  }
  rust_builder += "}\n";

  return Ok(rust_builder);
}

fn parse_type<'a>(
  concrete_primitives: &HashSet<String>,
  replacements: &HashMap<String, ParsedFullType>,
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str
) -> anyhow::Result<(ParsedType, ParsedType, &'a str)> {
  let mut rest = original;

  // TODO: prevent parsing 'staticky or something
  if rest.starts_with("'static") {
    rest = &rest["'static".len()..].trim();
    return Ok((ParsedType::Lifetime, ParsedType::Lifetime, rest));
  }

  let has_mut_ref = rest.starts_with("&mut"); // TODO: maybe handle &
  if has_mut_ref {
    rest = &rest["&mut".len()..].trim();
  }
  let has_imm_ref = rest.starts_with("&"); // TODO: maybe handle &
  if has_imm_ref {
    rest = &rest[1..].trim();
  }
  if has_imm_ref || has_mut_ref {
    let (inner_canonical, inner_aliasing, new_rest) =
        parse_full_type(&concrete_primitives, &replacements, &parsed_type_to_alias, rest)?;
    rest = new_rest;
    let result_canonical = ParsedType::Ref { mutable: has_mut_ref, inner: inner_canonical };
    let result_aliasing = ParsedType::Ref { mutable: has_mut_ref, inner: inner_aliasing };
    return Ok((result_canonical, result_aliasing, rest));
  }

  if rest.starts_with("<") {
    unimplemented!();
  }

  match parse_group(&concrete_primitives, &replacements, &parsed_type_to_alias, rest, false, true, false)? {
    (Some((tuple_members_canonical, tuple_members_aliasing)), new_rest) => {
      let result_canonical =
          ParsedType::Tuple { generic_args: tuple_members_canonical };
      let result_aliasing =
          ParsedType::Tuple { generic_args: tuple_members_aliasing };
      return Ok((result_canonical, result_aliasing, new_rest));
    }
    (None, _) => {} // continue
  }

  match parse_group(&concrete_primitives, &replacements, &parsed_type_to_alias, rest, false, false, true)? {
    (Some((tuple_members_canonical, tuple_members_aliasing)), new_rest) => {
      if tuple_members_canonical.len() != 1 {
        parse_group(&concrete_primitives, &replacements, &parsed_type_to_alias, rest, false, false, true);
        panic!("Bad slice!"); // DO NOT SUBMIT
      }
      let inner_canonical = tuple_members_canonical.first().unwrap();
      let result_canonical = ParsedType::Slice { inner: inner_canonical.clone() };
      let inner_aliasing = tuple_members_canonical.first().unwrap();
      let result_aliasing = ParsedType::Slice { inner: inner_aliasing.clone() };
      return Ok((result_canonical, result_aliasing, new_rest));
    }
    (None, _) => {} // continue
  }

  let re = Regex::new(r"( |,|::<|::|<|>|\[|\]|\(|\)|$)").unwrap();
  let name_end =
      if let Some(generic_name_match) = re.find(&rest) {
        generic_name_match.start()
      } else {
        rest.len()
      };
  let name = &rest[0..name_end];
  rest = &rest[name.len()..].trim();

  if name == "_" {
    return Ok((ParsedType::Wildcard, ParsedType::Wildcard, rest));
  }

  if concrete_primitives.contains(name) {
    return Ok((ParsedType::Primitive(name.to_string()), ParsedType::Primitive(name.to_string()), rest));
  }

  let (maybe_generic_args, new_rest) =
      parse_group(&concrete_primitives, replacements, parsed_type_to_alias, rest, true, false, false)?;
  rest = new_rest;
  let (generic_args_canonical, generic_args_aliasing) = maybe_generic_args.unwrap_or((Vec::new(), Vec::new()));

  let (maybe_params, new_rest) =
      parse_group(&concrete_primitives, replacements, parsed_type_to_alias, rest, false, true, false)?;
  rest = new_rest;
  let (params_canonical, params_aliasing) = maybe_params.unwrap_or((Vec::new(), Vec::new()));

  assert!(!name.contains("["));

  let result_canonical =
    ParsedType::Value {
      name: name.to_owned(),
      generic_args: generic_args_canonical,
      params: params_canonical
    };
  let result_aliasing =
      ParsedType::Value {
        name: name.to_owned(),
        generic_args: generic_args_aliasing,
        params: params_aliasing
      };
  Ok((result_canonical, result_aliasing, rest))
}

fn parse_group<'a>(
  concrete_primitives: &HashSet<String>,
  replacements: &HashMap<String, ParsedFullType>,
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str,
  allow_angles: bool,
  allow_parens: bool,
  allow_squares: bool,
) -> Result<(Option<(Vec<ParsedFullType>, Vec<ParsedFullType>)>, &'a str)> {
  let mut rest = original;
  if (allow_angles && (rest.starts_with("::<") || rest.starts_with("<"))) ||
      (allow_parens && rest.starts_with("(")) ||
      (allow_squares && rest.starts_with("[")) {
    let mut generic_args_canonical = Vec::new();
    let mut generic_args_aliasing = Vec::new();

    if rest.starts_with("::<") {
      rest = &rest["::<".len()..].trim();
    } else if rest.starts_with("<") {
      rest = &rest["<".len()..].trim();
    } else if rest.starts_with("(") {
      rest = &rest["(".len()..].trim();
    } else if rest.starts_with("[") {
      rest = &rest["[".len()..].trim();
    } else {
      panic!("wat");
    }

    if rest.starts_with(">") {
      // Do nothing
    } else if rest.starts_with(")") {
      // Do nothing
    } else if rest.starts_with("]") {
      // Do nothing
    } else {
      loop {
        let (generic_arg_canonical, generic_arg_aliasing, new_rest) =
            parse_full_type(&concrete_primitives, &replacements, &parsed_type_to_alias, rest)?;
        rest = new_rest;
        generic_args_canonical.push(generic_arg_canonical);
        generic_args_aliasing.push(generic_arg_aliasing);
        if rest.starts_with(",") {
          rest = &rest[",".len()..].trim();
          // continue
        } else if rest.starts_with(">") || rest.starts_with(")") || rest.starts_with("]") {
          break;
        } else {
          return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, format!("Bad type string: {}", original))));
        }
      }
    }
    if rest.starts_with(">") {
      rest = &rest[">".len()..].trim();
    } else if rest.starts_with(")") {
      rest = &rest[")".len()..].trim();
    } else if rest.starts_with("]") {
      rest = &rest["]".len()..].trim();
    } else {
      panic!("Wat");
    }

    Ok((Some((generic_args_canonical, generic_args_aliasing)), rest))
  } else {
    Ok((None, rest))
  }
}

// Returns:
// - Full type
// - rest of the string that wasnt parsed
fn parse_full_type<'a>(
  concrete_primitives: &HashSet<String>,
  replacements: &HashMap<String, ParsedFullType>,
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str
) -> anyhow::Result<(ParsedFullType, ParsedFullType, &'a str)> {
  let (full_type_canonical, full_type_with_aliases, rest) =
      parse_full_type_inner(concrete_primitives, replacements, parsed_type_to_alias, original)?;

  if let Some(alias_name) = parsed_type_to_alias.get(&full_type_canonical) {
    let alias_result =
        ParsedFullType {
          steps: vec![
            ParsedType::Alias(alias_name.clone())
          ]
        };
    return Ok((full_type_canonical, alias_result, rest));
  } else {
    return Ok((full_type_canonical, full_type_with_aliases, rest));
  }
}

fn parse_full_type_inner<'a>(
  concrete_primitives: &HashSet<String>,
  replacements: &HashMap<String, ParsedFullType>,
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str
) -> anyhow::Result<(ParsedFullType, ParsedFullType, &'a str)> {
  let mut rest = original;

  let re = Regex::new(r"(,|>|\)|$)").unwrap();
  let name_end =
      if let Some(generic_name_match) = re.find(&rest) {
        generic_name_match.start()
      } else {
        rest.len()
      };
  let full_name_preview = &rest[0..name_end];
  // if let Some(uid) = item_index.primitive_name_to_uid.get(full_name_preview) {
  //   rest = &rest[full_name_preview.len()..].trim();
  //
  //   return Ok(
  //     (
  //       SimpleValType {
  //         id: uid.clone(),
  //         generic_args: Vec::new(),
  //         maybe_parent_concrete: None,
  //         maybe_parent_impl: None
  //       },
  //       rest));
  // }

  let mut steps_canonical = Vec::new();
  let mut steps_aliasing = Vec::new();

  if rest.starts_with("<") {
    rest = &rest["<".len()..].trim();

    let (struct_full_type_canonical, struct_full_type_aliasing, new_rest) =
        parse_full_type(&concrete_primitives, &replacements, &parsed_type_to_alias, rest)?;
    rest = new_rest;

    if !rest.starts_with("as ") {
      panic!("wat");
    }
    rest = &rest["as".len()..].trim();

    let (impl_full_type_canonical, impl_full_type_aliasing, new_rest) =
        parse_full_type(&concrete_primitives, &replacements, &parsed_type_to_alias, rest)?;
    rest = new_rest;


    if !rest.starts_with(">") {
      panic!("wat");
    }
    rest = &rest[">".len()..].trim();

    if !rest.starts_with("::") {
      panic!("wat");
    }
    rest = &rest["::".len()..].trim();

    steps_canonical.push(
      ImplCast {
        struct_: struct_full_type_canonical,
        impl_: impl_full_type_canonical });
    steps_aliasing.push(
      ImplCast {
        struct_: struct_full_type_aliasing,
        impl_: impl_full_type_aliasing });
  }

  loop {
    let (new_step_canonical, new_step_aliasing, new_rest) =
        parse_type(&concrete_primitives, &replacements, &parsed_type_to_alias, rest)?;
    rest = new_rest;

    let maybe_replacement =
      if steps_canonical.len() == 0 {
        match &new_step_canonical {
          ParsedType::Value { name, generic_args, params } => {
            if generic_args.len() == 0 && params.len() == 0 {
              if let Some(replacement) = replacements.get(name) {
                Some(replacement)
              } else {
                None
              }
            } else {
              None
            }
          }
          _ => None
        }
      } else {
        None
      };
    if let Some(replacement) = maybe_replacement {
      steps_canonical.append(&mut replacement.steps.clone());
      steps_aliasing.append(&mut replacement.steps.clone());
    } else {
      steps_canonical.push(new_step_canonical);
      steps_aliasing.push(new_step_aliasing);
    }
    if rest.starts_with("::") {
      rest = &rest["::".len()..].trim();
      // continue
    } else {
      break;
    }
  }

  let result_canonical = ParsedFullType{ steps: steps_canonical };
  let result_aliasing = ParsedFullType{ steps: steps_aliasing };
  Ok((result_canonical, result_aliasing, rest))
}

fn mangle_generic_args(generic_args: &Vec<ParsedFullType>, more_after: bool) -> String {
  // If it's a tuple then we still want to print out the () even if
  // there's nothing inside it.
  if generic_args.len() > 0 {
    "_".to_owned() +
    &generic_args.len().to_string() +
    "__" +
    &generic_args
        .iter()
        .map(|x| {
          mangle_full_type(x)
        })
        .collect::<Vec<_>>()
        .join("__") +
        (if more_after { "__" } else { "" })
  } else {
    "".to_string()
    //("".to_string(), "".to_owned())
  }
}

fn mangle_type(
  valtype: &ParsedType,
  more: bool
) -> String {
  match valtype {
    ParsedType::Alias(name) => name.clone(),
    ParsedType::ImplCast { struct_, impl_ } => {
      mangle_full_type(struct_) +
          "__as_1__" +
          &mangle_full_type(impl_)
    }
    ParsedType::Ref { mutable, inner } => {
      (if *mutable { "mref_1__" } else { "iref_1__" }).to_owned() +
      &mangle_full_type(inner)
    }
    ParsedType::Value { name, generic_args, params } => {
      name.to_owned() +
          &mangle_generic_args(generic_args, more)
    },
    ParsedType::Tuple { generic_args } => {
      "tuple_".to_owned() +
          &mangle_generic_args(generic_args, more)
    }
    ParsedType::Slice { inner } => {
      "slice_".to_owned() +
          &mangle_generic_args(&vec![inner.clone()], more)
    },
    ParsedType::Primitive(name) => {
      if name == "str" {
        "str_ref".to_owned()
      } else {
        name.to_owned()
      }
    }
    ParsedType::Lifetime => "life".to_owned(),
    ParsedType::Wildcard => panic!("Wat wildcard"), // I dont think we can get these from rust
  }
}

fn mangle_full_type(type_: &ParsedFullType) -> String {
  let steps = &type_.steps;
  let mut result = "".to_string();
  for step_i in 0..steps.len() {
    if step_i > 0 {
      result += "_";
    }
    result += &mangle_type(&steps[step_i], step_i < steps.len() - 1);
  }
  result
}

fn get_pointered_prefixed_mangled_type(
  type_: &ParsedFullType
) -> String {
  match type_.steps.last().unwrap() {
    ImplCast { .. } => unimplemented!(),
    ParsedType::Lifetime => unimplemented!(),
    ParsedType::Wildcard => unimplemented!(),
    ParsedType::Ref { mutable, inner: referend } => {
      match referend.steps.last().unwrap() {
        ParsedType::Slice { inner: _IGNORED } => {
          // Something that's a reference to a slice should be a struct to C
          let thing = mangle_full_type(referend);
          thing
        }
        _ => {
          "*".to_owned() +
              (if *mutable { "mut " } else { "const " }) +
              &get_prefixed_mangled_type(referend)
        }
      }
    }
    _ => get_prefixed_mangled_type(type_)
  }
}

fn get_prefixed_mangled_type(
  type_: &ParsedFullType
) -> String {
  match type_.steps.last().unwrap() {
    ImplCast { .. } => unimplemented!(),
    ParsedType::Lifetime => unimplemented!(),
    ParsedType::Wildcard => unimplemented!(),
    ParsedType::Ref { mutable, inner: referend } => {
      unimplemented!(); // If we get here, we're probably in a double pointer
    }
    ParsedType::Value { .. } => {
      "rust_".to_owned() + &mangle_full_type(type_)
    }
    ParsedType::Alias(name) => name.clone(),
    ParsedType::Primitive(name) => name.clone(),
    ParsedType::Tuple { .. } => {
      "rust_".to_owned() + &mangle_full_type(type_)
    }
    ParsedType::Slice { .. } => {
      "rust_".to_owned() + &mangle_full_type(type_)
    }
  }
}

fn str_for_type(type_: &ParsedType) -> String {
  match type_ {
    ParsedType::Alias(name) => "$".to_string() + name,
    ParsedType::ImplCast { struct_, impl_ } => {
      str_for_full_type(struct_)
    }
    ParsedType::Ref { mutable, inner } => {
      "&".to_string() + (if *mutable { "mut " } else { "" }) + &str_for_full_type(inner)
    }
    ParsedType::Tuple { generic_args} => {
      "(".to_owned() +
          &generic_args.into_iter().map(|x| str_for_full_type(x)).collect::<Vec<_>>().join(", ") +
          ")"
    }
    ParsedType::Slice { inner} => {
      "[".to_owned() + &str_for_full_type(inner) + "]"
    }
    ParsedType::Value { name, generic_args, params } => {
      name.to_owned() +
          &(if generic_args.len() > 0 {
            "::<".to_owned() +
                &generic_args.into_iter().map(str_for_full_type).collect::<Vec<_>>().join(", ") +
                ">"
          } else {
            "".to_owned()
          })
    }
    ParsedType::Primitive(name) => name.to_owned(),
    ParsedType::Lifetime => "'static".to_owned(),
    ParsedType::Wildcard => "_".to_owned(),
  }
}

fn str_for_full_type(full_type: &ParsedFullType) -> String {
  full_type.steps.iter().map(str_for_type).collect::<Vec<_>>().join("::")
}

fn caller_str_for_full_type(full_type: &ParsedFullType) -> String {
  let init_steps: Vec<ParsedType> =
      full_type.steps[0..full_type.steps.len() - 1].to_vec();
  let last_step =
    match full_type.steps.last().unwrap().clone() {
      ParsedType::Alias(name) => unimplemented!(),
      ParsedType::Ref { .. } => panic!("wat"),
      ParsedType::Tuple { .. } => panic!("wat"),
      ParsedType::Slice { .. } => panic!("wat"),
      ImplCast { .. } => panic!("wat"),
      ParsedType::Primitive(_) => panic!("wat"),
      ParsedType::Lifetime => panic!("wat"),
      ParsedType::Wildcard => panic!("wat"),
      ParsedType::Value { name, generic_args, params } => {
        ParsedType::Value { name, generic_args: Vec::new(), params: Vec::new() }
      }
    };
  let mut steps = init_steps;
  steps.push(last_step);
  steps.iter().map(str_for_type).collect::<Vec<_>>().join("::")
}
