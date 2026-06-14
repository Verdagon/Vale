// From Frontend/ParsingPass/src/dev/vale/parsing/ParsedLoader.scala
// Loads .vpst files from JSON
//
// MIGRATION GUIDELINES FOR THIS FILE
// 1) Keep Rust structure/order close to the Scala reference below.
// 2) Use serde_json types directly (`Value`, `Map<String, Value>`), not lift-json names.
// 3) Prefer borrowed JSON inputs (`&Value`, `&Map<String, Value>`) to avoid cloning.
// 4) Match "__type" tags exactly and map each case to the corresponding Rust AST variant.
// 5) Keep helper error messages explicit so bad VPST shapes fail at the exact field/type.
// 6) Keep new Rust code above the equivalent Scala comment block.
// 7) If a Scala case is not ported yet, leave an explicit `panic!`/`vimpl` marker.

use crate::interner::StrI;
use crate::parse_arena::ParseArena;
use crate::lexing::{ParseError, RangeL};
use crate::parsing::ast::*;
use crate::utils::code_hierarchy::{FileCoordinate, PackageCoordinate};
use serde_json::{Map, Value, from_str};


fn expect_object<'p>(obj: &'p Value) -> &'p Map<String, Value> {
  obj
    .as_object()
    .unwrap_or_else(|| panic!("BadVPSTError: Expected JSON object, got: {:?}", obj))
}

fn expect_string<'p>(obj: &'p Value) -> &'p str {
  obj
    .as_str()
    .unwrap_or_else(|| panic!("BadVPSTError: Expected JSON string, got: {:?}", obj))
}

fn expect_number(obj: &Value) -> i64 {
  obj
    .as_i64()
    .unwrap_or_else(|| panic!("BadVPSTError: Expected JSON number, got: {:?}", obj))
}

fn expect_object_typed<'p>(obj: &'p Value, expected_type: &str) -> &'p Map<String, Value> {
  let jobj = expect_object(obj);
  let actual_type = get_string_field(jobj, "__type");
  if actual_type != expected_type {
    panic!(
      "BadVPSTError: Expected {} but got a {}",
      expected_type, actual_type
    );
  }
  jobj
}

fn get_field<'p>(jobj: &'p Map<String, Value>, field_name: &str) -> &'p Value {
  jobj
    .get(field_name)
    .unwrap_or_else(|| panic!("BadVPSTError: Object had no field named {}", field_name))
}

fn get_object_field<'p>(
  container_jobj: &'p Map<String, Value>,
  field_name: &str,
) -> &'p Map<String, Value> {
  expect_object(get_field(container_jobj, field_name))
}

// fn get_object_field_with_expected_type<'p>(
//   container_jobj: &'p Map<String, Value>,
//   field_name: &str,
//   expected_type: &str,
// ) -> &'p Map<String, Value> {
//   let jobj = expect_object(get_field(container_jobj, field_name));
//   expect_type(jobj, expected_type);
//   jobj
// }

fn get_string_field<'p>(jobj: &'p Map<String, Value>, field_name: &str) -> &'p str {
  expect_string(get_field(jobj, field_name))
}

fn get_int_field(jobj: &Map<String, Value>, field_name: &str) -> i32 {
  get_field(jobj, field_name)
    .as_i64()
    .unwrap_or_else(|| panic!("BadVPSTError: Field {} wasn't a number!", field_name))
    as i32
}

fn get_long_field(jobj: &Map<String, Value>, field_name: &str) -> i64 {
  let field = get_field(jobj, field_name);
  if let Some(n) = field.as_i64() {
    return n;
  }
  if let Some(s) = field.as_str() {
    let parsed = s
      .parse::<i64>()
      .unwrap_or_else(|_| panic!("BadVPSTError: Field {} wasn't a number!", field_name));
    if parsed.to_string() == s {
      return parsed;
    }
  }
  panic!("BadVPSTError: Field {} wasn't a number!", field_name);
}

fn get_float_field(jobj: &Map<String, Value>, field_name: &str) -> f64 {
  get_field(jobj, field_name)
    .as_f64()
    .unwrap_or_else(|| panic!("BadVPSTError: Field {} wasn't a double!", field_name))
}

fn get_boolean_field(jobj: &Map<String, Value>, field_name: &str) -> bool {
  get_field(jobj, field_name)
    .as_bool()
    .unwrap_or_else(|| panic!("BadVPSTError: Field {} wasn't a boolean!", field_name))
}

fn get_array_field<'p>(jobj: &'p Map<String, Value>, field_name: &str) -> &'p [Value] {
  get_field(jobj, field_name)
    .as_array()
    .map(|v| v.as_slice())
    .unwrap_or_else(|| panic!("BadVPSTError: Field {} wasn't an array!", field_name))
}

fn expect_type(jobj: &Map<String, Value>, expected_type: &str) -> () {
  let actual_type = get_type(jobj);
  if actual_type != expected_type {
    panic!(
      "BadVPSTError: Expected {} but got a {}",
      expected_type, actual_type
    );
  }
}

fn get_type<'p>(jobj: &'p Map<String, Value>) -> &'p str {
  get_string_field(jobj, "__type")
}

fn load_range(jobj: &Map<String, Value>) -> RangeL {
  expect_type(jobj, "Range");
  RangeL(
    get_int_field(jobj, "begin"),
    get_int_field(jobj, "end"),
  )
}

fn load_name<'p>(parse_arena: &ParseArena<'p>, jobj: &Map<String, Value>) -> NameP<'p> {
  expect_type(jobj, "Name");
  NameP(
    load_range(get_object_field(jobj, "range")),
    parse_arena.intern_str(get_string_field(jobj, "name")),
  )
}

pub fn load<'p>(
  parse_arena: &ParseArena<'p>,

  source: &str,
) -> Result<FileP<'p>, ParseError> {
  let parsed: Value = from_str(source).map_err(|err| ParseError::BadVPSTError {
    message: format!("Failed to parse VPST JSON: {}", err),
  })?;
  let jfile = expect_object_typed(&parsed, "File");
  let comments: Vec<_> = get_array_field(jfile, "commentsRanges")
    .iter()
    .map(expect_object)
    .map(load_range)
    .collect();
  let denizens: Vec<_> = get_array_field(jfile, "denizens")
    .iter()
    .map(expect_object)
    .map(|denizen| match get_type(denizen) {
      "Struct" => IDenizenP::TopLevelStruct(load_struct(parse_arena,denizen)),
      "Interface" => IDenizenP::TopLevelInterface(load_interface(parse_arena,denizen)),
      "Function" => IDenizenP::TopLevelFunction(load_function(parse_arena,denizen)),
      "Impl" => IDenizenP::TopLevelImpl(load_impl(parse_arena,denizen)),
      "Import" => IDenizenP::TopLevelImport(load_import(parse_arena,denizen)),
      "ExportAs" => IDenizenP::TopLevelExportAs(load_export_as(parse_arena,denizen)),
      other => panic!("Not implemented: unknown denizen type {}", other),
    })
    .collect();
  Ok(FileP {
    file_coord: load_file_coord(parse_arena, get_object_field(jfile, "fileCoord")),
    comments_ranges: parse_arena.alloc_slice_copy(&comments),
    denizens: parse_arena.alloc_slice_from_vec(denizens),
  })
}

fn load_function<'p>(
  parse_arena: &ParseArena<'p>,

  denizen: &Map<String, Value>,
) -> FunctionP<'p> {
  FunctionP {
    range: load_range(get_object_field(denizen, "range")),
    header: load_function_header(parse_arena,get_object_field(denizen, "header")),
    body: load_optional_object(get_object_field(denizen, "body"), |x| load_block(parse_arena,x))
      .map(|b| &*parse_arena.alloc(b)),
  }
}

fn load_impl<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ImplP<'p> {
  let attributes: Vec<_> = get_array_field(jobj, "attributes")
    .iter()
    .map(expect_object)
    .map(|x| load_attribute(parse_arena, x))
    .collect();
  ImplP {
    range: load_range(get_object_field(jobj, "range")),
    generic_params: load_optional_object(get_object_field(jobj, "identifyingRunes"), |x| {
      load_identifying_runes(parse_arena,x)
    }),
    template_rules: load_optional_object(get_object_field(jobj, "templateRules"), |x| {
      load_template_rules(parse_arena,x)
    }),
    struct_: load_optional_object(get_object_field(jobj, "struct"), |x| load_templex(parse_arena,x)),
    interface: load_templex(parse_arena,get_object_field(jobj, "interface")),
    attributes: parse_arena.alloc_slice_from_vec(attributes),
  }
}

fn load_export_as<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ExportAsP<'p> {
  ExportAsP {
    range: load_range(get_object_field(jobj, "range")),
    struct_: load_templex(parse_arena,get_object_field(jobj, "struct")),
    exported_name: load_name(parse_arena, get_object_field(jobj, "exportedName")),
  }
}

fn load_import<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ImportP<'p> {
  let package_steps: Vec<_> = get_array_field(jobj, "packageSteps")
    .iter()
    .map(expect_object)
    .map(|x| load_name(parse_arena, x))
    .collect();
  ImportP {
    range: load_range(get_object_field(jobj, "range")),
    module_name: load_name(parse_arena, get_object_field(jobj, "moduleName")),
    package_steps: parse_arena.alloc_slice_from_vec(package_steps),
    importee_name: load_name(parse_arena, get_object_field(jobj, "importeeName")),
  }
}

fn load_struct<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> StructP<'p> {
  let attributes: Vec<_> = get_array_field(jobj, "attributes")
    .iter()
    .map(expect_object)
    .map(|x| load_attribute(parse_arena, x))
    .collect();
  StructP {
    range: load_range(get_object_field(jobj, "range")),
    name: load_name(parse_arena, get_object_field(jobj, "name")),
    attributes: parse_arena.alloc_slice_from_vec(attributes),
    mutability: load_optional_object(get_object_field(jobj, "mutability"), |x| load_templex(parse_arena,x)),
    identifying_runes: load_optional_object(
      get_object_field(jobj, "identifyingRunes"),
      |x| load_identifying_runes(parse_arena,x),
    ),
    template_rules: load_optional_object(get_object_field(jobj, "templateRules"), |x| {
      load_template_rules(parse_arena,x)
    }),
    maybe_default_region_rune: load_optional_object(
      get_object_field(jobj, "maybeDefaultRegion"),
      |x| load_region_rune(parse_arena, x),
    ),
    body_range: load_range(get_object_field(jobj, "bodyRange")),
    members: load_struct_members(parse_arena,get_object_field(jobj, "members")),
  }
}

fn load_interface<'p>(
  parse_arena: &ParseArena<'p>,

  denizen: &Map<String, Value>,
) -> InterfaceP<'p> {
  let attributes: Vec<_> = get_array_field(denizen, "attributes")
    .iter()
    .map(expect_object)
    .map(|x| load_attribute(parse_arena, x))
    .collect();
  InterfaceP {
    range: load_range(get_object_field(denizen, "range")),
    name: load_name(parse_arena, get_object_field(denizen, "name")),
    attributes: parse_arena.alloc_slice_from_vec(attributes),
    mutability: load_optional_object(get_object_field(denizen, "mutability"), |x| load_templex(parse_arena,x)),
    maybe_identifying_runes: load_optional_object(
      get_object_field(denizen, "maybeIdentifyingRunes"),
      |x| load_identifying_runes(parse_arena,x),
    ),
    template_rules: load_optional_object(get_object_field(denizen, "templateRules"), |x| {
      load_template_rules(parse_arena,x)
    }),
    maybe_default_region_rune: load_optional_object(
      get_object_field(denizen, "maybeDefaultRegion"),
      |x| load_region_rune(parse_arena, x),
    ),
    body_range: load_range(get_object_field(denizen, "bodyRange")),
    members: parse_arena.alloc_slice_from_vec(
      get_array_field(denizen, "members")
        .iter()
        .map(expect_object)
        .map(|x| load_function(parse_arena,x))
        .collect::<Vec<_>>(),
    ),
  }
}

fn load_function_header<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> FunctionHeaderP<'p> {
  let attributes: Vec<_> = get_array_field(jobj, "attributes")
    .iter()
    .map(expect_object)
    .map(|x| load_attribute(parse_arena, x))
    .collect();
  FunctionHeaderP {
    range: load_range(get_object_field(jobj, "range")),
    name: load_optional_object(get_object_field(jobj, "name"), |x| load_name(parse_arena, x)),
    attributes: parse_arena.alloc_slice_from_vec(attributes),
    generic_parameters: load_optional_object(
      get_object_field(jobj, "maybeUserSpecifiedIdentifyingRunes"),
      |x| load_identifying_runes(parse_arena,x),
    ),
    template_rules: load_optional_object(get_object_field(jobj, "templateRules"), |x| {
      load_template_rules(parse_arena,x)
    }),
    params: load_optional_object(get_object_field(jobj, "params"), |x| load_params(parse_arena,x)),
    ret: load_function_return(parse_arena,get_object_field(jobj, "return")),
  }
}

fn load_file_coord<'p>(parse_arena: &ParseArena<'p>, jobj: &Map<String, Value>) -> &'p FileCoordinate<'p> {
  let package_coord = load_package_coord(parse_arena, get_object_field(jobj, "packageCoord"));
  parse_arena.intern_file_coordinate(package_coord, get_string_field(jobj, "filepath"))
}

fn load_package_coord<'p>(
  parse_arena: &ParseArena<'p>,
  jobj: &Map<String, Value>,
) -> &'p PackageCoordinate<'p> {
  let module = parse_arena.intern_str(get_string_field(jobj, "module"));
  let packages: Vec<StrI<'p>> = get_array_field(jobj, "packages")
    .iter()
    .map(expect_string)
    .map(|s| parse_arena.intern_str(s))
    .collect();
  parse_arena.intern_package_coordinate(module, &packages)
}

fn load_params<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ParamsP<'p> {
  let params_vec: Vec<_> = get_array_field(jobj, "params")
    .iter()
    .map(expect_object)
    .map(|x| load_parameter(parse_arena,x))
    .collect();
  ParamsP {
    range: load_range(get_object_field(jobj, "range")),
    params: parse_arena.alloc_slice_from_vec(params_vec),
  }
}

fn load_parameter<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ParameterP<'p> {
  ParameterP {
    range: load_range(get_object_field(jobj, "range")),
    virtuality: load_optional_object(get_object_field(jobj, "virtuality"), |x| {
      load_virtuality(parse_arena, x)
    }),
    maybe_pre_checked: load_optional_object(get_object_field(jobj, "maybePreChecked"), load_range),
    self_borrow: load_optional_object(get_object_field(jobj, "selfBorrow"), load_range),
    pattern: load_optional_object(get_object_field(jobj, "pattern"), |x| load_pattern(parse_arena,x)),
  }
}

fn load_pattern<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> PatternPP<'p> {
  PatternPP {
    range: load_range(get_object_field(jobj, "range")),
    destination: load_optional_object(get_object_field(jobj, "capture"), |x| {
      load_destination_local(parse_arena, x)
    }),
    templex: load_optional_object(get_object_field(jobj, "templex"), |x| load_templex(parse_arena,x)),
    destructure: load_optional_object(get_object_field(jobj, "destructure"), |x| {
      load_destructure(parse_arena,x)
    }),
  }
}

fn load_destructure<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> DestructureP<'p> {
  let patterns_vec: Vec<_> = get_array_field(jobj, "patterns")
    .iter()
    .map(expect_object)
    .map(|x| load_pattern(parse_arena,x))
    .collect();
  DestructureP {
    range: load_range(get_object_field(jobj, "range")),
    patterns: parse_arena.alloc_slice_from_vec(patterns_vec),
  }
}

fn load_destination_local<'p>(
  parse_arena: &ParseArena<'p>,
  jobj: &Map<String, Value>,
) -> DestinationLocalP<'p> {
  DestinationLocalP {
    decl: load_name_declaration(parse_arena, get_object_field(jobj, "name")),
    mutate: load_optional_object(get_object_field(jobj, "mutate"), load_range),
  }
}


fn load_name_declaration<'p>(
  parse_arena: &ParseArena<'p>,
  jobj: &Map<String, Value>,
) -> INameDeclarationP<'p> {
  match get_type(jobj) {
    "IgnoredLocalNameDeclaration" => {
      INameDeclarationP::IgnoredLocalNameDeclaration(load_range(get_object_field(jobj, "range")))
    }
    "LocalNameDeclaration" => {
      INameDeclarationP::LocalNameDeclaration(load_name(parse_arena, get_object_field(jobj, "name")))
    }
    "IterableNameDeclaration" => {
      INameDeclarationP::IterableNameDeclaration(load_range(get_object_field(jobj, "range")))
    }
    "IteratorNameDeclaration" => {
      INameDeclarationP::IteratorNameDeclaration(load_range(get_object_field(jobj, "range")))
    }
    "IterationOptionNameDeclaration" => INameDeclarationP::IterationOptionNameDeclaration(
      load_range(get_object_field(jobj, "range")),
    ),
    "ConstructingMemberNameDeclaration" => {
      INameDeclarationP::ConstructingMemberNameDeclaration(load_name(
        parse_arena,
        get_object_field(jobj, "name"),
      ))
    }
    other => panic!("Not implemented: load_name_declaration {}", other),
  }
}

fn load_imprecise_name<'p>(
  parse_arena: &ParseArena<'p>,
  jobj: &Map<String, Value>,
) -> IImpreciseNameP<'p> {
  match get_type(jobj) {
    "LookupName" => IImpreciseNameP::LookupName(load_name(parse_arena, get_object_field(jobj, "name"))),
    "IterableName" => IImpreciseNameP::IterableName(load_range(get_object_field(jobj, "range"))),
    "IteratorName" => IImpreciseNameP::IteratorName(load_range(get_object_field(jobj, "range"))),
    "IterationOptionName" => {
      IImpreciseNameP::IterationOptionName(load_range(get_object_field(jobj, "range")))
    }
    other => panic!("Not implemented: load_imprecise_name {}", other),
  }
}

// fn load_capture_name(jobj: &Map<String, Value>) -> INameDeclarationP {
//   panic!("Not implemented");
// }

fn load_block<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> BlockPE<'p> {
  BlockPE {
    range: load_range(get_object_field(jobj, "range")),
    maybe_pure: load_optional_object(get_object_field(jobj, "maybePure"), load_range),
    maybe_default_region: load_optional_object(
      get_object_field(jobj, "maybeDefaultRegion"),
      |x| load_region_rune(parse_arena, x),
    ),
    inner: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "inner"))),
  }
}

fn load_consecutor<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ConsecutorPE<'p> {
  let inners: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "inners")
    .iter()
    .map(expect_object)
    .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
    .collect();
  ConsecutorPE {
    inners: parse_arena.alloc_slice_from_vec(inners),
  }
}

fn load_function_return<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> FunctionReturnP<'p> {
  FunctionReturnP {
    range: load_range(get_object_field(jobj, "range")),
    ret_type: load_optional_object(get_object_field(jobj, "retType"), |x| load_templex(parse_arena,x)),
  }
}

fn load_unit(_jobj: &Map<String, Value>) -> UnitP {
  panic!("Not implemented");
}

fn load_struct_members<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> StructMembersP<'p> {
  let contents: Vec<_> = get_array_field(jobj, "members")
    .iter()
    .map(expect_object)
    .map(|x| load_struct_content(parse_arena,x))
    .collect();
  StructMembersP {
    range: load_range(get_object_field(jobj, "range")),
    contents: parse_arena.alloc_slice_from_vec(contents),
  }
}

fn load_expression<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> IExpressionPE<'p> {
  match get_type(jobj) {
    "Return" => IExpressionPE::Return(ReturnPE {
      range: load_range(get_object_field(jobj, "range")),
      expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "expr"))),
    }),
    "Void" => IExpressionPE::Void(VoidPE {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "ConstantInt" => IExpressionPE::ConstantInt(ConstantIntPE {
      range: load_range(get_object_field(jobj, "range")),
      value: get_long_field(jobj, "value"),
      bits: {
        let bits = get_object_field(jobj, "bits");
        match get_type(bits) {
          "None" => None,
          "Some" => Some(expect_number(get_field(bits, "value"))),
          other => panic!("BadVPSTError: Expected None/Some for bits but got {}", other),
        }
      },
    }),
    "ConstantStr" => IExpressionPE::ConstantStr(ConstantStrPE {
      range: load_range(get_object_field(jobj, "range")),
      value: parse_arena.intern_str(get_string_field(jobj, "value")),
    }),
    "ConstantFloat" => IExpressionPE::ConstantFloat(ConstantFloatPE {
      range: load_range(get_object_field(jobj, "range")),
      value: get_float_field(jobj, "value"),
    }),
    "ConstantBool" => IExpressionPE::ConstantBool(ConstantBoolPE {
      range: load_range(get_object_field(jobj, "range")),
      value: get_boolean_field(jobj, "value"),
    }),
    "StrInterpolate" => {
      let parts: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "parts")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
        .collect();
      IExpressionPE::StrInterpolate(StrInterpolatePE {
        range: load_range(get_object_field(jobj, "range")),
        parts: parse_arena.alloc_slice_from_vec(parts),
      })
    }
    "Dot" => IExpressionPE::Dot(DotPE {
      range: load_range(get_object_field(jobj, "range")),
      left: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "left"))),
      operator_range: load_range(get_object_field(jobj, "operatorRange")),
      member: load_name(parse_arena, get_object_field(jobj, "member")),
    }),
    "FunctionCall" => {
      let arg_exprs: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "argExprs")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
        .collect();
      IExpressionPE::FunctionCall(FunctionCallPE {
        range: load_range(get_object_field(jobj, "range")),
        operator_range: load_range(get_object_field(jobj, "operatorRange")),
        callable_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "callableExpr"))),
        arg_exprs: parse_arena.alloc_slice_from_vec(arg_exprs),
      })
    }
    "BinaryCall" => IExpressionPE::BinaryCall(BinaryCallPE {
      range: load_range(get_object_field(jobj, "range")),
      function_name: load_name(parse_arena, get_object_field(jobj, "functionName")),
      left_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "leftExpr"))),
      right_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "rightExpr"))),
    }),
    "Lambda" => IExpressionPE::Lambda(LambdaPE {
      captures: load_optional_object(get_object_field(jobj, "captures"), load_unit),
      function: load_function(parse_arena,get_object_field(jobj, "function")),
    }),
    "MagicParamLookup" => IExpressionPE::MagicParamLookup(MagicParamLookupPE {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "If" => IExpressionPE::If(IfPE {
      range: load_range(get_object_field(jobj, "range")),
      condition: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "condition"))),
      then_body: &*parse_arena.alloc(load_block(parse_arena,get_object_field(jobj, "thenBody"))),
      else_body: &*parse_arena.alloc(load_block(parse_arena,get_object_field(jobj, "elseBody"))),
    }),
    "SubExpression" => IExpressionPE::SubExpression(SubExpressionPE {
      range: load_range(get_object_field(jobj, "range")),
      inner: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "innerExpr"))),
    }),
    "Let" => IExpressionPE::Let(LetPE {
      range: load_range(get_object_field(jobj, "range")),
      pattern: &*parse_arena.alloc(load_pattern(parse_arena,get_object_field(jobj, "pattern"))),
      source: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "source"))),
    }),
    "While" => IExpressionPE::While(WhilePE {
      range: load_range(get_object_field(jobj, "range")),
      condition: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "condition"))),
      body: &*parse_arena.alloc(load_block(parse_arena,get_object_field(jobj, "body"))),
    }),
    "Mutate" => IExpressionPE::Mutate(MutatePE {
      range: load_range(get_object_field(jobj, "range")),
      mutatee: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "mutatee"))),
      source: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "source"))),
    }),
    "MethodCall" => {
      let arg_exprs: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "argExprs")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
        .collect();
      IExpressionPE::MethodCall(MethodCallPE {
        range: load_range(get_object_field(jobj, "range")),
        subject_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "subjectExpr"))),
        operator_range: load_range(get_object_field(jobj, "operatorRange")),
        method_lookup: &*parse_arena.alloc(load_lookup(parse_arena,get_object_field(jobj, "method"))),
        arg_exprs: parse_arena.alloc_slice_from_vec(arg_exprs),
      })
    }
    "Tuple" => {
      let elements: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "elements")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
        .collect();
      IExpressionPE::Tuple(TuplePE {
        range: load_range(get_object_field(jobj, "range")),
        elements: parse_arena.alloc_slice_from_vec(elements),
      })
    }
    "Augment" => IExpressionPE::Augment(AugmentPE {
      range: load_range(get_object_field(jobj, "range")),
      target_ownership: load_ownership(get_object_field(jobj, "targetOwnership")),
      inner: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "inner"))),
    }),
    "Each" => IExpressionPE::Each(EachPE {
      range: load_range(get_object_field(jobj, "range")),
      maybe_pure: load_optional_object(get_object_field(jobj, "maybePure"), load_range),
      entry_pattern: load_pattern(parse_arena,get_object_field(jobj, "entryPattern")),
      in_keyword_range: load_range(get_object_field(jobj, "inRange")),
      iterable_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "iterableExpr"))),
      body: &*parse_arena.alloc(load_block(parse_arena,get_object_field(jobj, "body"))),
    }),
    "Destruct" => IExpressionPE::Destruct(DestructPE {
      range: load_range(get_object_field(jobj, "range")),
      inner: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "inner"))),
    }),
    "And" => IExpressionPE::And(AndPE {
      range: load_range(get_object_field(jobj, "range")),
      left: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "left"))),
      right: &*parse_arena.alloc(load_block(parse_arena,get_object_field(jobj, "right"))),
    }),
    "Or" => IExpressionPE::Or(OrPE {
      range: load_range(get_object_field(jobj, "range")),
      left: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "left"))),
      right: &*parse_arena.alloc(load_block(parse_arena,get_object_field(jobj, "right"))),
    }),
    "Range" => IExpressionPE::Range(RangePE {
      range: load_range(get_object_field(jobj, "range")),
      from_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "begin"))),
      to_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "end"))),
    }),
    "BraceCall" => {
      let arg_exprs: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "argExprs")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
        .collect();
      IExpressionPE::BraceCall(BraceCallPE {
        range: load_range(get_object_field(jobj, "range")),
        operator_range: load_range(get_object_field(jobj, "operatorRange")),
        subject_expr: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "callableExpr"))),
        arg_exprs: parse_arena.alloc_slice_from_vec(arg_exprs),
        callable_readwrite: get_boolean_field(jobj, "callableReadwrite"),
      })
    }
    "Not" => IExpressionPE::Not(NotPE {
      range: load_range(get_object_field(jobj, "range")),
      inner: &*parse_arena.alloc(load_expression(parse_arena,get_object_field(jobj, "innerExpr"))),
    }),
    "ConstructArray" => IExpressionPE::ConstructArray(load_construct_array(parse_arena,jobj)),
    "Lookup" => IExpressionPE::Lookup(&*parse_arena.alloc(load_lookup(parse_arena,jobj))),
    "Consecutor" => IExpressionPE::Consecutor(load_consecutor(parse_arena,jobj)),
    "Block" => IExpressionPE::Block(load_block(parse_arena,jobj)),
    "Break" => IExpressionPE::Break(BreakPE {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "Unlet" => IExpressionPE::Unlet(UnletPE {
      range: load_range(get_object_field(jobj, "range")),
      name: load_imprecise_name(parse_arena, get_object_field(jobj, "localName")),
    }),
    other => panic!("Not implemented: load_expression {}", other),
  }
}

fn load_array_size<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> IArraySizeP<'p> {
  match get_type(jobj) {
    "RuntimeSized" => IArraySizeP::RuntimeSized,
    "StaticSized" => IArraySizeP::StaticSized(StaticSizedArraySizeP {
      size_pt: load_optional_object(get_object_field(jobj, "size"), |x| load_templex(parse_arena,x)),
    }),
    other => panic!("Not implemented: load_array_size {}", other),
  }
}

fn load_construct_array<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ConstructArrayPE<'p> {
  ConstructArrayPE {
    range: load_range(get_object_field(jobj, "range")),
    type_pt: load_optional_object(get_object_field(jobj, "type"), |x| load_templex(parse_arena,x)),
    mutability_pt: load_optional_object(get_object_field(jobj, "mutability"), |x| {
      load_templex(parse_arena,x)
    }),
    variability_pt: load_optional_object(get_object_field(jobj, "variability"), |x| {
      load_templex(parse_arena,x)
    }),
    size: load_array_size(parse_arena,get_object_field(jobj, "size")),
    initializing_individual_elements: get_boolean_field(jobj, "initializingIndividualElements"),
    args: {
      let v: Vec<&'p IExpressionPE<'p>> = get_array_field(jobj, "args")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_expression(parse_arena,x)))
        .collect();
      parse_arena.alloc_slice_from_vec(v)
    },
  }
}

fn load_lookup<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> LookupPE<'p> {
  LookupPE {
    name: load_imprecise_name(parse_arena, get_object_field(jobj, "name")),
    template_args: load_optional_object(get_object_field(jobj, "templateArgs"), |x| {
      load_template_args(parse_arena,x)
    }),
  }
}

fn load_template_args<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> TemplateArgsP<'p> {
  TemplateArgsP {
    range: load_range(get_object_field(jobj, "range")),
    args: parse_arena.alloc_slice_from_vec(
      get_array_field(jobj, "args")
        .iter()
        .map(expect_object)
        .map(|x| &*parse_arena.alloc(load_templex(parse_arena,x)))
        .collect(),
    ),
  }
}

// fn load_load_as(jobj: &Map<String, Value>) -> LoadAsP {
//   match get_type(jobj) {
//     "Move" => LoadAsP::Move,
//     "Use" => LoadAsP::Use,
//     "LoadAsBorrow" => LoadAsP::LoadAsBorrow,
//     "LoadAsWeak" => LoadAsP::LoadAsWeak,
//     other => panic!("Not implemented: load_load_as {}", other),
//   }
// }

fn load_virtuality<'p>(_parse_arena: &ParseArena<'p>, jobj: &Map<String, Value>) -> AbstractP {
  AbstractP {
    range: load_range(get_object_field(jobj, "range")),
  }
}

fn load_struct_content<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> IStructContent<'p> {
  match get_type(jobj) {
    "NormalStructMember" => IStructContent::NormalStructMember(NormalStructMemberP {
      range: load_range(get_object_field(jobj, "range")),
      name: load_name(parse_arena, get_object_field(jobj, "name")),
      variability: load_variability(get_object_field(jobj, "variability")),
      tyype: load_templex(parse_arena,get_object_field(jobj, "type")),
    }),
    "VariadicStructMember" => IStructContent::VariadicStructMember(VariadicStructMemberP {
      range: load_range(get_object_field(jobj, "range")),
      variability: load_variability(get_object_field(jobj, "variability")),
      tyype: load_templex(parse_arena,get_object_field(jobj, "type")),
    }),
    "StructMethod" => IStructContent::StructMethod(load_function(parse_arena,get_object_field(jobj, "function"))),
    other => panic!("Not implemented: load_struct_content {}", other),
  }
}

fn load_optional_object<T, F>(jobj: &Map<String, Value>, load_contents: F) -> Option<T>
where
  F: Fn(&Map<String, Value>) -> T,
{
  match get_type(jobj) {
    "None" => None,
    "Some" => Some(load_contents(get_object_field(jobj, "value"))),
    other => panic!("BadVPSTError: Expected None/Some but got {}", other),
  }
}

// fn load_optional<T, F>(jobj: &Map<String, Value>, load_contents: F) -> Option<T>
// where
//   F: Fn(&Value) -> T,
// {
//   panic!("Not implemented");
// }

fn load_template_rules<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> TemplateRulesP<'p> {
  TemplateRulesP {
    range: load_range(get_object_field(jobj, "range")),
    rules: parse_arena.alloc_slice_from_vec(
      get_array_field(jobj, "rules")
        .iter()
        .map(expect_object)
        .map(|x| load_rulex(parse_arena,x))
        .collect(),
    ),
  }
}

fn load_rulex<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> IRulexPR<'p> {
  match get_type(jobj) {
    "TypedPR" => IRulexPR::Typed(load_typed_pr(parse_arena,jobj)),
    "ComponentsPR" => IRulexPR::Components(ComponentsPR {
      range: load_range(get_object_field(jobj, "range")),
      container: load_rulex_type(get_object_field(jobj, "container")),
      components: parse_arena.alloc_slice_from_vec(
        get_array_field(jobj, "components")
          .iter()
          .map(expect_object)
          .map(|x| load_rulex(parse_arena,x))
          .collect(),
      ),
    }),
    "OrPR" => IRulexPR::Or(OrPR {
      range: load_range(get_object_field(jobj, "range")),
      possibilities: parse_arena.alloc_slice_from_vec(
        get_array_field(jobj, "possibilities")
          .iter()
          .map(expect_object)
          .map(|x| load_rulex(parse_arena,x))
          .collect(),
      ),
    }),
    "DotPR" => IRulexPR::Dot(DotPR {
      range: load_range(get_object_field(jobj, "range")),
      container: &*parse_arena.alloc(load_rulex(parse_arena,get_object_field(jobj, "container"))),
      member_name: load_name(parse_arena, get_object_field(jobj, "memberName")),
    }),
    "TemplexPR" => IRulexPR::Templex(load_templex(parse_arena,get_object_field(jobj, "templex"))),
    "EqualsPR" => IRulexPR::Equals(EqualsPR {
      range: load_range(get_object_field(jobj, "range")),
      left: &*parse_arena.alloc(load_rulex(parse_arena,get_object_field(jobj, "left"))),
      right: &*parse_arena.alloc(load_rulex(parse_arena,get_object_field(jobj, "right"))),
    }),
    "BuiltinCallPR" => IRulexPR::BuiltinCall(BuiltinCallPR {
      range: load_range(get_object_field(jobj, "range")),
      name: load_name(parse_arena, get_object_field(jobj, "name")),
      args: parse_arena.alloc_slice_from_vec(
        get_array_field(jobj, "args")
          .iter()
          .map(expect_object)
          .map(|x| load_rulex(parse_arena,x))
          .collect(),
      ),
    }),
    other => panic!("Not implemented: load_rulex {}", other),
  }
}

fn load_typed_pr<'p>(parse_arena: &ParseArena<'p>, jobj: &Map<String, Value>) -> TypedPR<'p> {
  TypedPR {
    range: load_range(get_object_field(jobj, "range")),
    rune: load_optional_object(get_object_field(jobj, "rune"), |x| load_name(parse_arena, x)),
    tyype: load_rulex_type(get_object_field(jobj, "type")),
  }
}

fn load_rulex_type(jobj: &Map<String, Value>) -> ITypePR {
  match get_type(jobj) {
    "IntTypePR" => ITypePR::IntType,
    "BoolTypePR" => ITypePR::BoolType,
    "OwnershipTypePR" => ITypePR::OwnershipType,
    "MutabilityTypePR" => ITypePR::MutabilityType,
    "VariabilityTypePR" => ITypePR::VariabilityType,
    "LocationTypePR" => ITypePR::LocationType,
    "CoordTypePR" => ITypePR::CoordType,
    "CoordListTypePR" => ITypePR::CoordListType,
    "PrototypeTypePR" => ITypePR::PrototypeType,
    "KindTypePR" => ITypePR::KindType,
    "RegionTypePR" => ITypePR::RegionType,
    "CitizenTemplateTypePR" => ITypePR::CitizenTemplateType,
    other => panic!("Not implemented: load_rulex_type {}", other),
  }
}

fn load_generic_parameter_type(jobj: &Map<String, Value>) -> GenericParameterTypeP {
  GenericParameterTypeP {
    range: load_range(get_object_field(jobj, "range")),
    tyype: load_rulex_type(get_object_field(jobj, "type")),
  }
}

fn load_rune_attribute(jobj: &Map<String, Value>) -> IRuneAttributeP {
  match get_type(jobj) {
    "ReadOnlyRuneAttribute" => {
      IRuneAttributeP::ReadOnlyRegionRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "ReadWriteRuneAttribute" => {
      IRuneAttributeP::ReadWriteRegionRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "ImmutableRuneAttribute" => {
      IRuneAttributeP::ImmutableRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "MutableRuneAttribute" => {
      IRuneAttributeP::MutableRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "ImmutableRegionRuneAttribute" => {
      IRuneAttributeP::ImmutableRegionRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "AdditiveRuneAttribute" => {
      IRuneAttributeP::AdditiveRegionRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "PoolRuneAttribute" => {
      IRuneAttributeP::PoolRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "ArenaRuneAttribute" => {
      IRuneAttributeP::ArenaRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    "BumpRuneAttribute" => {
      IRuneAttributeP::BumpRuneAttribute(load_range(get_object_field(jobj, "range")))
    }
    other => panic!("Not implemented: load_rune_attribute {}", other),
  }
}

fn load_attribute<'p>(parse_arena: &ParseArena<'p>, jobj: &Map<String, Value>) -> IAttributeP<'p> {
  match get_type(jobj) {
    "AbstractAttribute" => IAttributeP::AbstractAttribute(AbstractAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "PureAttribute" => IAttributeP::PureAttribute(PureAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "AdditiveAttribute" => IAttributeP::AdditiveAttribute(AdditiveAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "ExportAttribute" => IAttributeP::ExportAttribute(ExportAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "ExternAttribute" => IAttributeP::ExternAttribute(ExternAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "LinearAttribute" => IAttributeP::LinearAttribute(LinearAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "BuiltinAttribute" => IAttributeP::BuiltinAttribute(BuiltinAttributeP {
      range: load_range(get_object_field(jobj, "range")),
      generator_name: load_name(parse_arena, get_object_field(jobj, "generatorName")),
    }),
    "SealedAttribute" => IAttributeP::SealedAttribute(SealedAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "WeakableAttribute" => IAttributeP::WeakableAttribute(WeakableAttributeP {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "MacroCall" => IAttributeP::MacroCall(MacroCallP {
      range: load_range(get_object_field(jobj, "range")),
      inclusion: if get_boolean_field(jobj, "dontCall") {
        IMacroInclusionP::DontCallMacro
      } else {
        IMacroInclusionP::CallMacro
      },
      name: load_name(parse_arena, get_object_field(jobj, "name")),
    }),
    other => panic!("Not implemented: unknown attribute type {}", other),
  }
}

fn load_mutability(jobj: &Map<String, Value>) -> MutabilityP {
  match get_type(jobj) {
    "Mutable" => MutabilityP::Mutable,
    "Immutable" => MutabilityP::Immutable,
    other => panic!("Not implemented: load_mutability {}", other),
  }
}

fn load_variability(jobj: &Map<String, Value>) -> VariabilityP {
  match get_type(jobj) {
    "Varying" => VariabilityP::Varying,
    "Final" => VariabilityP::Final,
    other => panic!("Not implemented: load_variability {}", other),
  }
}

fn load_ownership(jobj: &Map<String, Value>) -> OwnershipP {
  match get_type(jobj) {
    "Own" => OwnershipP::Own,
    "Borrow" => OwnershipP::Borrow,
    "Live" => OwnershipP::Live,
    "Weak" => OwnershipP::Weak,
    "Share" => OwnershipP::Share,
    other => panic!("Not implemented: load_ownership {}", other),
  }
}

fn load_templex<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> ITemplexPT<'p> {
  match get_type(jobj) {
    "NameOrRuneT" => ITemplexPT::NameOrRune(NameOrRunePT(load_name(
      parse_arena,
      get_object_field(jobj, "rune"),
    ))),
    "InterpretedT" => ITemplexPT::Interpreted(InterpretedPT {
      range: load_range(get_object_field(jobj, "range")),
      maybe_ownership: load_optional_object(
        get_object_field(jobj, "maybeOwnership"),
        |x| load_ownership_pt(parse_arena, x),
      )
      .map(|x| &*parse_arena.alloc(x)),
      maybe_region: load_optional_object(get_object_field(jobj, "maybeRegion"), |x| {
        load_region_rune(parse_arena, x)
      })
      .map(|x| &*parse_arena.alloc(x)),
      inner: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "inner"))),
    }),
    "CallT" => ITemplexPT::Call(CallPT {
      range: load_range(get_object_field(jobj, "range")),
      template: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "template"))),
      args: parse_arena.alloc_slice_from_vec(
        get_array_field(jobj, "args")
          .iter()
          .map(expect_object)
          .map(|x| &*parse_arena.alloc(load_templex(parse_arena,x)))
          .collect(),
      ),
    }),
    "MutabilityT" => ITemplexPT::Mutability(MutabilityPT(
      load_range(get_object_field(jobj, "range")),
      load_mutability(get_object_field(jobj, "mutability")),
    )),
    "VariabilityT" => ITemplexPT::Variability(VariabilityPT(
      load_range(get_object_field(jobj, "range")),
      load_variability(get_object_field(jobj, "variability")),
    )),
    "IntT" => ITemplexPT::Int(IntPT {
      range: load_range(get_object_field(jobj, "range")),
      value: get_long_field(jobj, "inner"),
    }),
    "AnonymousRuneT" => ITemplexPT::AnonymousRune(AnonymousRunePT {
      range: load_range(get_object_field(jobj, "range")),
    }),
    "RuntimeSizedArrayT" => ITemplexPT::RuntimeSizedArray(RuntimeSizedArrayPT {
      range: load_range(get_object_field(jobj, "range")),
      mutability: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "mutability"))),
      element: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "element"))),
    }),
    "StaticSizedArrayT" => ITemplexPT::StaticSizedArray(StaticSizedArrayPT {
      range: load_range(get_object_field(jobj, "range")),
      mutability: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "mutability"))),
      variability: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "variability"))),
      size: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "size"))),
      element: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "element"))),
    }),
    "ManualSequenceT" => ITemplexPT::Tuple(TuplePT {
      range: load_range(get_object_field(jobj, "range")),
      elements: parse_arena.alloc_slice_from_vec(
        get_array_field(jobj, "members")
          .iter()
          .map(expect_object)
          .map(|x| &*parse_arena.alloc(load_templex(parse_arena,x)))
          .collect(),
      ),
    }),
    "PrototypeT" => ITemplexPT::Func(FuncPT {
      range: load_range(get_object_field(jobj, "range")),
      name: load_name(parse_arena, get_object_field(jobj, "name")),
      params_range: load_range(get_object_field(jobj, "paramsRange")),
      parameters: parse_arena.alloc_slice_from_vec(
        get_array_field(jobj, "params")
          .iter()
          .map(expect_object)
          .map(|x| &*parse_arena.alloc(load_templex(parse_arena,x)))
          .collect(),
      ),
      return_type: &*parse_arena.alloc(load_templex(parse_arena,get_object_field(jobj, "returnType"))),
    }),
    "OwnershipT" => ITemplexPT::Ownership(load_ownership_pt(parse_arena, jobj)),
    other => panic!("Not implemented: load_templex {}", other),
  }
}

fn load_ownership_pt<'p>(_parse_arena: &ParseArena<'p>, jobj: &Map<String, Value>) -> OwnershipPT {
  OwnershipPT(
    load_range(get_object_field(jobj, "range")),
    load_ownership(get_object_field(jobj, "ownership")),
  )
}

fn load_region_rune<'p>(_parse_arena: &ParseArena<'p>, _jobj: &Map<String, Value>) -> RegionRunePT<'p> {
  panic!("Not implemented");
}

fn load_identifying_runes<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> GenericParametersP<'p> {
  GenericParametersP {
    range: load_range(get_object_field(jobj, "range")),
    params: parse_arena.alloc_slice_from_vec(
      get_array_field(jobj, "identifyingRunes")
        .iter()
        .map(expect_object)
        .map(|x| load_identifying_rune(parse_arena,x))
        .collect(),
    ),
  }
}

fn load_identifying_rune<'p>(
  parse_arena: &ParseArena<'p>,

  jobj: &Map<String, Value>,
) -> GenericParameterP<'p> {
  GenericParameterP {
    range: load_range(get_object_field(jobj, "range")),
    name: load_name(parse_arena, get_object_field(jobj, "name")),
    maybe_type: load_optional_object(
      get_object_field(jobj, "maybeType"),
      load_generic_parameter_type,
    ),
    coord_region: load_optional_object(get_object_field(jobj, "maybeCoordRegion"), |x| {
      load_region_rune(parse_arena, x)
    }),
    attributes: parse_arena.alloc_slice_from_vec(
      get_array_field(jobj, "attributes")
        .iter()
        .map(expect_object)
        .map(load_rune_attribute)
        .collect(),
    ),
    maybe_default: load_optional_object(get_object_field(jobj, "maybeDefault"), |x| {
      load_templex(parse_arena,x)
    }),
  }
}

