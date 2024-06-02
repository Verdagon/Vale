#![allow(unused_imports)]

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
use anyhow::{Context, Result};
use regex::Regex;
use crate::ParsedType::ImplCast;
use crate::ResolveError::ResolveFatal;

struct TypeInfo {
  type_: ParsedFullType,
  c_name: String,
  size: usize,
  alignment: usize
}

struct FuncInfo {
  type_: ParsedFullType,
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
  Ref{ mutable: bool, inner: Box<ParsedFullType> },
  Value{ name: String, generic_args: Vec<ParsedFullType>, params: Vec<ParsedFullType> },
  Tuple{ generic_args: Vec<ParsedFullType> },
  Slice{ inner: ParsedFullType },
  ImplCast{ struct_: ParsedFullType, impl_: ParsedFullType },
  Primitive(String),
  Lifetime,
  Wildcard,
}

fn main() -> Result<(), anyhow::Error> {
  // _VR_2_std_collections_HashMap__i32__String
  // _VR_2_std_collections_HashMap__i32__2_std_collections_Vec__String

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

      let mut original_str_to_alias: HashMap<String, String> = HashMap::new();
      let mut alias_to_original_str: HashMap<String, String> = HashMap::new();
      let mut type_str_and_parsed_type_and_original_line_and_maybe_alias: Vec<(String, ParsedFullType, String, Option<String>)> = Vec::new();
      let mut func_str_and_parsed_type_and_original_line_and_maybe_alias: Vec<(String, ParsedFullType, String, Option<String>)> = Vec::new();

      let maybe_input_file_path = instantiate_matches.get_one::<String>("input_file");

      let mut lines: Vec<String> = Vec::new();
      if let Some(input_file_path) = maybe_input_file_path {
        let file = File::open(input_file_path)?;
        let reader = io::BufReader::new(file);
        for line_res in reader.lines() {
          let line = line_res?;
          if Regex::new(r#"^#pragma\s+rs(use|fn)"#).unwrap().is_match(&line) {
            lines.push(line);
          }
        }
      } else {
        for line in io::stdin().lock().lines() {
          lines.push(line?);
        }
      }

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
          let (func, rest) = parse_full_type(&HashMap::new(), target_type_str)?;
          func_str_and_parsed_type_and_original_line_and_maybe_alias.push(
            (target_type_str.to_owned(), func, line.clone(), maybe_alias));
        } else {
          let (type_, rest) = parse_full_type(&HashMap::new(), target_type_str)?;
          assert!(rest.len() == 0); // DO NOT SUBMIT
          type_str_and_parsed_type_and_original_line_and_maybe_alias.push(
            (target_type_str.to_owned(), type_, line.clone(), maybe_alias));
        }
      }

      if !original_str_to_alias.contains_key("&str") {
        let parsed_type =
            ParsedFullType{
              steps: vec![
                ParsedType::Ref{
                  mutable: false,
                  inner: Box::from(
                    ParsedFullType {
                      steps: vec![
                        ParsedType::Primitive("str".to_owned())
                      ]
                    }) }]
            };
        original_str_to_alias.insert("&str".to_owned(), "VR_str_ref".to_owned());
        alias_to_original_str.insert("VR_str_ref".to_owned(), "&str".to_owned());
        type_str_and_parsed_type_and_original_line_and_maybe_alias.push(
          ("&str".to_owned(), parsed_type, "(builtin)".to_owned(), Some("VR_str_ref".to_owned())));
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

      let mut scouting_strings: Vec<String> = Vec::new();

      let mut primitives: HashSet<String> = HashSet::new();
      primitives.insert("&str".to_owned()); // TODO: add more

      // TODO: Expensive call to string_path
      type_str_and_parsed_type_and_original_line_and_maybe_alias.sort_by_key(|x| x.0.clone());
      func_str_and_parsed_type_and_original_line_and_maybe_alias.sort_by_key(|x| x.0.clone());
      for (type_str, parsed_type, line, _maybe_alias) in &type_str_and_parsed_type_and_original_line_and_maybe_alias {
        println!("Sizing type {}", &type_str);
        scouting_strings.push(get_type_sizer_string(parsed_type));
      }
      for (func_str, parsed_type, line, _maybe_alias) in &func_str_and_parsed_type_and_original_line_and_maybe_alias {
        let last = parsed_type.steps.last().unwrap();
        match &last {
          ParsedType::Ref { .. } => panic!("wat"),
          ParsedType::Primitive(_) => panic!("wat"),
          ParsedType::Lifetime => panic!("wat"),
          ParsedType::Wildcard => panic!("wat"),
          ParsedType::ImplCast { .. } => panic!("wat"),
          ParsedType::Tuple { .. } => {}// continue
          ParsedType::Slice { .. } => {}// continue
          ParsedType::Value { name, generic_args, params } => {
            if name == "drop" {
              // Make sure to resolve the type.
              // TODO: This might be redundant.
              let type_ =
                  ParsedFullType{ steps: parsed_type.steps[0..parsed_type.steps.len() - 1].into_iter().map(|x| x.clone()).collect::<Vec<_>>() };
              println!("Sizing type for drop function {}", &func_str);
              scouting_strings.push(get_type_sizer_string(&type_));
              continue;
            }
          }
        }
        println!("Sizing function {}", &func_str);
        scouting_strings.push(get_func_scouting_string(parsed_type));
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

      let mut rust_str_to_alias: HashMap<String, String> = HashMap::new();
      let mut alias_to_rust_str: HashMap<String, String> = HashMap::new();

      println!("Got sizer program output:\n{}", sizer_program_output_str);

      let lines = sizer_program_output_str.split("\n").collect::<Vec<_>>();
      let mut root_line_i = 0;
      while root_line_i < lines.len() {
        let root_line = lines[root_line_i];

        let original_str =
            if let Some(first_space_pos) = root_line.find(" ") {
              root_line[0..first_space_pos].trim()
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
            rust_type_str
          } else if root_line.starts_with("fn") {
            let (num_params, func_str) = parse_func_info_line(lines[root_line_i + 1].trim())?;

            let (ret_size, ret_alignment, ret_type_rust_str) = parse_type_info_line(lines[root_line_i + 2].trim())?;
            type_rust_str_to_size_and_alignment.insert(ret_type_rust_str.to_owned(), (ret_size, ret_alignment));

            let mut param_types_rust_strs = Vec::new();
            for param_line_i in (root_line_i + 3)..next_root_line_i {
              let (param_size, param_alignment, param_rust_type_str) = parse_type_info_line(lines[param_line_i].trim())?;
              type_rust_str_to_size_and_alignment.insert(param_rust_type_str.to_owned(), (param_size, ret_alignment));
              param_types_rust_strs.push(param_rust_type_str.to_owned());
            }

            func_rust_str_to_ret_str_and_params_strs.insert(
              func_str.to_owned(), (ret_type_rust_str.to_owned(), param_types_rust_strs));
            func_str
          } else {
            panic!("Bad output from sizer program: {}", root_line);
          };

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
          let (type_, _) = parse_full_type(&HashMap::new(), &type_rust_str)?;
          alias_to_parsed_type.insert(alias.to_string(), type_.clone());
          parsed_type_to_alias.insert(type_.clone(), alias.to_string());
        }
      }
      for (func_rust_str, (size, alignment)) in &type_rust_str_to_size_and_alignment {
        if let Some(alias) = rust_str_to_alias.get(func_rust_str) {
          // Handling in an empty map for aliases is fine because these come straight from Rust and
          // don't mention aliases.
          let (type_, _) = parse_full_type(&HashMap::new(), &func_rust_str)?;
          alias_to_parsed_type.insert(alias.to_string(), type_.clone());
          parsed_type_to_alias.insert(type_.clone(), alias.to_string());
        }
      }

      let mut type_rust_str_to_info: HashMap<String, TypeInfo> = HashMap::new();
      let mut func_rust_str_to_info: HashMap<String, FuncInfo> = HashMap::new();
      for (func_rust_str, (ret_type_rust_str, param_types_rust_strs)) in func_rust_str_to_ret_str_and_params_strs {
        let (type_without_aliasing, _) = parse_full_type(&HashMap::new(), &func_rust_str)?;
        let (type_with_aliasing, _) = parse_full_type(&parsed_type_to_alias, &func_rust_str)?;
        let c_name = mangle_full_type(&type_with_aliasing, true);
        func_rust_str_to_info.insert(
          func_rust_str,
          FuncInfo {
              type_: type_without_aliasing,
              c_name,
              ret_type_rust_str,
              param_types_rust_strs
            });
      }
      for (type_rust_str, (size, alignment)) in type_rust_str_to_size_and_alignment {
        let (type_without_aliasing, _) = parse_full_type(&HashMap::new(), &type_rust_str)?;
        let (type_with_aliasing, _) = parse_full_type(&parsed_type_to_alias, &type_rust_str)?;
        let c_name = mangle_full_type(&type_with_aliasing, true);
        type_rust_str_to_info.insert(
          type_rust_str,
          TypeInfo {
              type_: type_without_aliasing,
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
        let maybe_alias = rust_str_to_alias.get(rust_type_str).map(|x| x.as_str());
        struct_strings.push(
          instantiate_struct(rust_type_str, maybe_alias, type_info)?);
      }

      for (rust_type_str, func_info) in &func_rust_str_to_info {
        let maybe_alias = None;
        func_strings.push(
          instantiate_func(
            &type_rust_str_to_info, func_info, &maybe_alias)?);
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

fn parse_func_info_line(info_line: &str) -> Result<(usize, &str)> {
  if let Some(first_space_pos) = info_line.find(" ") {
    let arity_str = &info_line[0..first_space_pos];
    match arity_str.parse::<usize>() {
      Ok(arity) => {
        let type_str = &info_line[first_space_pos..].trim();
        return Ok((arity, type_str));
      },
      Err(e) => {}
    }
  }
  return Err(anyhow::Error::new(std::io::Error::new(std::io::ErrorKind::Other, format!("Bad func info line: {}", info_line))));
}

fn parse_type_info_line(info_line: &str) -> Result<(usize, usize, &str)> {
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
              return Ok((size, alignment, type_str));
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
pub extern "C" fn VR_StrFromCStr(char_ptr: *const c_char) -> VR_str_ref {
  let c_str = unsafe { core::ffi::CStr::from_ptr(char_ptr) };
  if let Ok(rust_str) = c_str.to_str() {
    let s_rs: VR_str_ref = unsafe { mem::transmute(rust_str) };
    return s_rs;
  } else {
    panic!("Error: c_str.to_str() failed.");
  }
}

// TODO: Is it okay to use u8 here instead of c_char?
#[no_mangle]
pub extern "C" fn VR_StrNew(length: usize, char_ptr: *const u8) -> VR_str_ref {
  let c_str = unsafe { std::slice::from_raw_parts(char_ptr, length) };
  if let Ok(rust_str) = core::ffi::CStr::from_bytes_with_nul(c_str) {
    if let Ok(rust_str) = rust_str.to_str() {
      let s_rs: VR_str_ref = unsafe { mem::transmute(rust_str) };
      return s_rs;
    } else {
      panic!("Error: c_str.to_str() failed.");
    }
  } else {
    panic!("Error: CStr::from_bytes_with_nul() failed.");
  }
}

#[no_mangle]
pub extern "C" fn VR_StrToCStr(str_c: VR_str_ref) -> *const c_char {
  let str_rs: &str = unsafe { mem::transmute(str_c) };
  let ptr = str_rs.as_ptr() as *const c_char;
  return ptr;
}

#[no_mangle]
pub extern "C" fn VR_StrLen(str_c: VR_str_ref) -> usize {
  let str_rs: &str = unsafe { mem::transmute(str_c) };
  let len: usize = str_rs.len();
  return len;
}

"#.replace("VR_str_ref", str_ref_alias)
}

enum ResolveError {
  NotFound,
  ResolveFatal(anyhow::Error)
}

fn sizify_type(type_: &ParsedType) -> String {
  match type_ {
    ParsedType::ImplCast { struct_, impl_ } => {
      sizify_full_type(struct_)
    }
    ParsedType::Ref { mutable, inner } => {
      "&".to_string() + (if *mutable { "mut" } else { "" }) + &sizify_full_type(inner)
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
        &generic_args.into_iter().map(sizify_full_type).collect::<Vec<_>>().join(", ") +
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

fn get_type_sizer_string(full_type: &ParsedFullType) -> String {
  let sizified_type = sizify_full_type(full_type);
  let mut rust_src = String::with_capacity(1000);
  rust_src += "  println!(\"type {}\", \"";
  rust_src += &sizified_type;
  rust_src += "\");";
  rust_src += "  print_type::<";
  rust_src += &sizified_type;
  rust_src += ">();";
  return rust_src;
}

fn get_func_scouting_string(full_type: &ParsedFullType) -> String {
  let sizified_type = sizify_full_type(full_type);
  let sizified_func = sizify_func(full_type);
  let mut rust_src = String::with_capacity(1000);
  rust_src += "  println!(\"fn {}\", \"";
  rust_src += &sizified_type;
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
  type_str: &str,
  maybe_alias: Option<&str>,
  info: &TypeInfo
) -> anyhow::Result<String> {
  let as_ = maybe_alias.unwrap_or(&info.c_name);

  let mut builder = String::with_capacity(1000);
  builder += "#[repr(C, align(";
  builder += &info.alignment.to_string();
  builder += "))]\n";
  // builder += "#[repr(C)]\n";
  builder += "pub struct ";
  builder += as_;
  builder += " (std::mem::MaybeUninit<[u8; ";
  builder += &info.size.to_string();
  builder += "]>);\n";
  builder += "const_assert_eq!(std::mem::size_of::<";
  builder += type_str;
  builder += ">(), ";
  builder += &info.size.to_string();
  builder += ");\n";

  return Ok(builder);
}

fn instantiate_func(
  type_rust_str_to_info: &HashMap<String, TypeInfo>,
  info: &FuncInfo,
  maybe_alias: &Option<String>,
  // c_folder: &str,
  // rust_folder: &str
) -> anyhow::Result<String> {
  let as_ = maybe_alias.as_ref().unwrap_or(&info.c_name);

  let ret_type_rust_str = &info.ret_type_rust_str;
  let ret_type_info = type_rust_str_to_info.get(ret_type_rust_str).unwrap();

  let mut rust_builder = String::with_capacity(1000);
  rust_builder += "#[no_mangle]\n";
  rust_builder += "pub extern \"C\" fn ";
  rust_builder += &as_;
  rust_builder += "(\n";
  for (param_type_rust_str, param_i) in info.param_types_rust_strs.iter().zip(0..info.param_types_rust_strs.len()) {
    let param_name = "  param_".to_owned() + &param_i.to_string() + &"_c";
    let param_c_type_str = &type_rust_str_to_info.get(param_type_rust_str).unwrap().c_name;
    rust_builder += &format!("  {}: {},\n", param_name, param_c_type_str);
  }
  rust_builder += ")";
  // if let Some(return_type_simple) = &maybe_output_type {
  rust_builder += " -> ";
  rust_builder += &ret_type_info.c_name;
  // }
  rust_builder += " {\n";
  for (param_type_str, param_i) in info.param_types_rust_strs.iter().zip(0..info.param_types_rust_strs.len()) {
    let c_param_name = "param_".to_owned() + &param_i.to_string() + &"_c";
    let rs_param_name = "param_".to_owned() + &param_i.to_string() + &"_rs";
    let param_type_info = type_rust_str_to_info.get(param_type_str).unwrap();

    rust_builder += "  const_assert_eq!(std::mem::size_of::<";
    rust_builder += param_type_str;
    rust_builder += ">(), std::mem::size_of::<";
    rust_builder += &param_type_info.c_name;
    rust_builder += ">());\n";

    rust_builder += "  let ";
    rust_builder += &rs_param_name;
    rust_builder += ": ";
    rust_builder += param_type_str;
    rust_builder += " = unsafe { mem::transmute(";
    rust_builder += &c_param_name;
    rust_builder += ") };\n";
  }

  rust_builder += "  ";
  // if let Some(return_type_simple) = maybe_output_type {
    rust_builder += "let result_rs: ";
    rust_builder += &info.ret_type_rust_str;
    rust_builder += " = ";
  // }
  rust_builder += &caller_str_for_full_type(&info.type_);
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
    rust_builder += ret_type_rust_str;
    rust_builder += ">(), std::mem::size_of::<";
    rust_builder += &ret_type_info.c_name;
    rust_builder += ">());\n";

    rust_builder += "  let result_c: ";
    rust_builder += &ret_type_info.c_name;
    rust_builder += " = unsafe { mem::transmute(result_rs) };\n";
    rust_builder += "  return result_c;\n";
  // }
  rust_builder += "}\n";

  return Ok(rust_builder);
}

fn parse_type<'a>(
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str
) -> anyhow::Result<(ParsedType, &'a str)> {
  let mut rest = original;

  // TODO: prevent parsing 'staticky or something
  if rest.starts_with("'static") {
    rest = &rest["'static".len()..].trim();
    return Ok((ParsedType::Lifetime, rest));
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
    let (inner, new_rest) =
        parse_full_type(&parsed_type_to_alias, rest)?;
    rest = new_rest;
    let result = ParsedType::Ref { mutable: has_mut_ref, inner: Box::from(inner) };
    return Ok((result, rest));
  }

  if rest.starts_with("<") {
    unimplemented!();
  }

  match parse_group(&parsed_type_to_alias, rest, false, true, false)? {
    (Some(tuple_members), new_rest) => {
      let result =
          ParsedType::Tuple { generic_args: tuple_members };
      return Ok((result, new_rest));
    }
    (None, _) => {} // continue
  }

  match parse_group(&parsed_type_to_alias, rest, false, false, true)? {
    (Some(tuple_members), new_rest) => {
      if tuple_members.len() != 1 {
        parse_group(&parsed_type_to_alias, rest, false, false, true);
        panic!("Bad slice!"); // DO NOT SUBMIT
      }
      let inner = tuple_members.first().unwrap();
      let result = ParsedType::Slice { inner: inner.clone() };
      return Ok((result, new_rest));
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
    return Ok((ParsedType::Wildcard, rest));
  }

  let (maybe_generic_args, new_rest) =
      parse_group(parsed_type_to_alias, rest, true, false, false)?;
  rest = new_rest;
  let generic_args = maybe_generic_args.unwrap_or(Vec::new());

  let (maybe_params, new_rest) =
      parse_group(parsed_type_to_alias, rest, false, true, false)?;
  rest = new_rest;
  let params = maybe_params.unwrap_or(Vec::new());

  assert!(!name.contains("["));

  let result =
    ParsedType::Value {
      name: name.to_owned(),
      generic_args: generic_args,
      params: params
    };
  Ok((result, rest))
}

fn parse_group<'a>(
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str,
  allow_angles: bool,
  allow_parens: bool,
  allow_squares: bool,
) -> Result<(Option<Vec<ParsedFullType>>, &'a str)> {
  let mut rest = original;
  if (allow_angles && (rest.starts_with("::<") || rest.starts_with("<"))) ||
      (allow_parens && rest.starts_with("(")) ||
      (allow_squares && rest.starts_with("[")) {
    let mut generic_args = Vec::new();

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
        let (generic_arg, new_rest) =
            parse_full_type(&parsed_type_to_alias, rest)?;
        rest = new_rest;
        generic_args.push(generic_arg);
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

    Ok((Some(generic_args), rest))
  } else {
    Ok((None, rest))
  }
}

fn parse_full_type<'a>(
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str
) -> anyhow::Result<(ParsedFullType, &'a str)> {
  let (inner, rest) = parse_full_type_inner(parsed_type_to_alias, original)?;
  if let Some(alias_name) = parsed_type_to_alias.get(&inner) {
    assert!(!alias_name.contains("["));

    return Ok(
      (
        ParsedFullType {
          steps: vec![
            ParsedType::Value {
              name: alias_name.clone(),
              generic_args: Vec::new(),
              params: Vec::new()
            }
          ]
        },
        rest));
  }
  return Ok((inner, rest));
}

fn parse_full_type_inner<'a>(
  parsed_type_to_alias: &HashMap<ParsedFullType, String>,
  original: &'a str
) -> anyhow::Result<(ParsedFullType, &'a str)> {
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

  let mut steps = Vec::new();

  if rest.starts_with("<") {
    rest = &rest["<".len()..].trim();

    let (struct_full_type, new_rest) =
        parse_full_type(&parsed_type_to_alias, rest)?;
    rest = new_rest;

    if !rest.starts_with("as ") {
      panic!("wat");
    }
    rest = &rest["as".len()..].trim();

    let (impl_full_type, new_rest) =
        parse_full_type(&parsed_type_to_alias, rest)?;
    rest = new_rest;


    if !rest.starts_with(">") {
      panic!("wat");
    }
    rest = &rest[">".len()..].trim();

    if !rest.starts_with("::") {
      panic!("wat");
    }
    rest = &rest["::".len()..].trim();

    steps.push(
      ImplCast {
        struct_: struct_full_type,
        impl_: impl_full_type });
  }

  loop {
    let (new_step, new_rest) =
        parse_type(&parsed_type_to_alias, rest)?;
    steps.push(new_step);
    rest = new_rest;
    if rest.starts_with("::") {
      rest = &rest["::".len()..].trim();
      // continue
    } else {
      break;
    }
  }

  let result = ParsedFullType{ steps: steps };
  Ok((result, rest))
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
          mangle_full_type(x, false)
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
    ParsedType::ImplCast { struct_, impl_ } => {
      mangle_full_type(struct_, more) +
          "__as_1__" +
          &mangle_full_type(impl_, more)
    }
    ParsedType::Ref { mutable, inner } => {
      (if *mutable { "mref_1__" } else { "iref_1__" }).to_owned() +
      &mangle_full_type(inner, more)
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

fn mangle_full_type(
  type_: &ParsedFullType,
  is_root: bool,
) -> String {
  let steps = &type_.steps;
  let mut result = (if is_root { "VR_" } else { "" }).to_string();
  for step_i in 0..steps.len() {
    if step_i > 0 {
      result += "_";
    }
    result += &mangle_type(&steps[step_i], step_i < steps.len() - 1);
  }
  return result;
}

fn caller_str_for_type(type_: &ParsedType) -> String {
  match type_ {
    ParsedType::ImplCast { struct_, impl_ } => {
      crate::sizify_full_type(struct_)
    }
    ParsedType::Ref { mutable, inner } => {
      "&".to_string() + (if *mutable { "mut" } else { "" }) + &crate::sizify_full_type(inner)
    }
    ParsedType::Tuple { generic_args} => {
      "(".to_owned() +
          &generic_args.into_iter().map(|x| crate::sizify_full_type(x)).collect::<Vec<_>>().join(", ") +
          ")"
    }
    ParsedType::Slice { inner} => {
      "[".to_owned() + &crate::sizify_full_type(inner) + "]"
    }
    ParsedType::Value { name, generic_args, params } => {
      name.to_owned() +
          &(if generic_args.len() > 0 {
            "::<".to_owned() +
                &generic_args.into_iter().map(crate::sizify_full_type).collect::<Vec<_>>().join(", ") +
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

fn caller_str_for_full_type(full_type: &ParsedFullType) -> String {
  let init_steps: Vec<ParsedType> =
      full_type.steps[0..full_type.steps.len() - 1].to_vec();
  let last_step =
    match full_type.steps.last().unwrap().clone() {
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
  steps.iter().map(caller_str_for_type).collect::<Vec<_>>().join("::")
}
