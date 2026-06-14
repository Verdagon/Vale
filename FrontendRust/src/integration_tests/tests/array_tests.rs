use crate::collect_only_tnode;
use crate::integration_tests::tests::run_compilation::test;
use crate::integration_tests::tests::run_compilation::test_no_builtins;
use crate::interner::StrI;
use crate::keywords::Keywords;
use crate::parse_arena::ParseArena;
use crate::scout_arena::ScoutArena;
use crate::simplifying::hammer_interner::HammerInterner;
use crate::tests::tests::load_expected;
use crate::typing::ast::expressions::LetNormalTE;
use crate::typing::ast::expressions::NewImmRuntimeSizedArrayTE;
use crate::typing::ast::expressions::RuntimeSizedArrayLookupTE;
use crate::typing::ast::expressions::StaticSizedArrayLookupTE;
use crate::typing::compiler_error_reporter::ICompileErrorT;
use crate::typing::env::function_environment_t::ILocalVariableT;
use crate::typing::env::function_environment_t::ReferenceLocalVariableT;
use crate::typing::names::names::CodeVarNameT;
use crate::typing::names::names::IVarNameT;
use crate::typing::templata::templata::ITemplataT;
use crate::typing::templata::templata::MutabilityTemplataT;
use crate::typing::test::traverse::NodeRefT;
use crate::typing::types::types::CoordT;
use crate::typing::types::types::KindT;
use crate::typing::types::types::MutabilityT;
use crate::typing::types::types::OwnershipT;
use crate::typing::typing_interner::TypingInterner;
use crate::von::ast::IVonData;
use crate::von::ast::VonBool;
use crate::von::ast::VonInt;
use crate::von::ast::VonStr;

// mig: struct ArrayTests
pub struct ArrayTests;

// mig: fn returning_static_array_from_function_and_dotting_it
#[test]
fn returning_static_array_from_function_and_dotting_it() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test_no_builtins(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nfunc makeArray() [#5]int { return [#](2, 3, 4, 5, 6); }\nexported func main() int {\n  a = makeArray();\n  x = a.3;\n  [_, _, _, _, _] = a;\n  return x;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 5 }) => {}
        other => panic!("expected VonInt(5), got {:?}", other),
    }
}

// mig: fn simple_static_array_and_runtime_index_lookup
#[test]
fn simple_static_array_and_runtime_index_lookup() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  i = 2;\n  a = [#](2, 3, 4, 5, 6);\n  return a[i];\n}\n",
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::StaticSizedArrayLookup(_) => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 4 }) => {}
        other => panic!("expected VonInt(4), got {:?}", other),
    }
}

// mig: fn destroy_ssa_of_imms_into_function
#[test]
fn destroy_ssa_of_imms_into_function() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  a = [#](13, 14, 15);\n  sum = 0;\n  drop_into(a, &(e) => { set sum = sum + e; });\n  return sum;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn destroy_rsa_of_imms_into_function
#[test]
fn destroy_rsa_of_imms_into_function() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  a = Array<imm, int>(3, {13 + _});\n  sum = 0;\n  drop_into(a, &(e) => { set sum = sum + e; });\n  return sum;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn destroy_ssa_of_muts_into_function
#[test]
fn destroy_ssa_of_muts_into_function() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nstruct Spaceship { fuel int; }\nexported func main() int {\n  a = [#](Spaceship(13), Spaceship(14), Spaceship(15));\n  sum = 0;\n  drop_into(a, &(e) => { set sum = sum + e.fuel; });\n  return sum;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn destroy_rsa_of_muts_into_function
#[test]
fn destroy_rsa_of_muts_into_function() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nstruct Spaceship { fuel int; }\nexported func main() int {\n  a = MakeArray<Spaceship>(3, &{Spaceship(13 + _)});\n  sum = 0;\n  drop_into(a, &(e) => { set sum = sum + e.fuel; });\n  return sum;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn migrate_rsa
#[test]
fn migrate_rsa() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nstruct Spaceship { fuel int; }\nexported func main() int {\n  a = Array<mut, Spaceship>(3, &{Spaceship(41 + _)});\n  b = Array<mut, Spaceship>(3);\n  migrate(a, &b);\n  return b[1].fuel;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn migrate_ssa
#[test]
fn migrate_ssa() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nstruct Spaceship { fuel int; }\nexported func main() int {\n  a = [#3](&{Spaceship(41 + _)});\n  b = Array<mut, Spaceship>(3);\n  migrate(a, &b);\n  return b[1].fuel;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn unspecified_mutability_static_array_from_lambda_defaults_to_mutable
#[test]
fn unspecified_mutability_static_array_from_lambda_defaults_to_mutable() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  i = 3;\n  a = [#5](&{_ * 42});\n  return a[1];\n}\n",
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::StaticSizedArrayLookup(StaticSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Mutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn immutable_static_array_from_lambda
#[test]
fn immutable_static_array_from_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let source = load_expected("programs/arrays/ssaimmfromcallable.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        &source,
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::StaticSizedArrayLookup(StaticSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Immutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn mutable_static_array_from_lambda
#[test]
fn mutable_static_array_from_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let source = load_expected("programs/arrays/ssamutfromcallable.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        &source,
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::StaticSizedArrayLookup(StaticSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Mutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn immutable_static_array_from_values
#[test]
fn immutable_static_array_from_values() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let source = load_expected("programs/arrays/ssaimmfromvalues.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        &source,
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::StaticSizedArrayLookup(StaticSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Immutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn mutable_static_array_from_values
#[test]
fn mutable_static_array_from_values() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let source = load_expected("programs/arrays/ssamutfromvalues.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        &source,
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::StaticSizedArrayLookup(StaticSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Mutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn unspecified_mutability_runtime_array_from_lambda_defaults_to_mutable
#[test]
fn unspecified_mutability_runtime_array_from_lambda_defaults_to_mutable() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nexported func main() int {\n  i = 3;\n  a = MakeArray<int>(5, &{_ * 42});\n  return a[1];\n}\n",
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::RuntimeSizedArrayLookup(RuntimeSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Mutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn immutable_runtime_array_from_lambda
#[test]
fn immutable_runtime_array_from_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let source = load_expected("programs/arrays/rsaimmfromcallable.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        &source,
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::RuntimeSizedArrayLookup(RuntimeSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Immutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn mutable_runtime_array_from_lambda
#[test]
fn mutable_runtime_array_from_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let source = load_expected("programs/arrays/rsamutfromcallable.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        &source,
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::RuntimeSizedArrayLookup(RuntimeSizedArrayLookupTE {
                array_type,
                ..
            }) if array_type.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Mutable })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

//m [<mut> 3 * [#3]<mut>int] = [mut][ [mut][1, 2, 3], [mut][4, 5, 6], [mut][7, 8, 9] ];
// mig: fn take_arraysequence_as_a_parameter
#[test]
fn take_arraysequence_as_a_parameter() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nfunc doThings(arr [#5]<imm>int) int {\n  return arr.3;\n}\nexported func main() int {\n  a = #[#](2, 3, 4, 5, 6);\n  return doThings(a);\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 5 }) => {}
        other => panic!("expected VonInt(5), got {:?}", other),
    }
}

// mig: fn borrow_arraysequence_as_a_parameter
#[test]
fn borrow_arraysequence_as_a_parameter() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nstruct MutableStruct {\n  x int;\n}\n\nfunc doThings(arr &[#3]^MutableStruct) int {\n  return arr.2.x;\n}\nexported func main() int {\n  a = [#](MutableStruct(2), MutableStruct(3), MutableStruct(4));\n  return doThings(&a);\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 4 }) => {}
        other => panic!("expected VonInt(4), got {:?}", other),
    }
}

// the argument to __Array doesnt even have to be a struct or a lambda or an
// interface or whatever, its just passed straight through to the prototype
// mig: fn array_map_with_int
#[test]
fn array_map_with_int() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nfunc __call(lol int, i int) int { return i; }\n\nexported func main() int {\n  a = #[]int(10, 1337);\n  return a.3;\n}\n",
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::NewImmRuntimeSizedArray(NewImmRuntimeSizedArrayTE {
                array_type: rsa,
                ..
            }) if rsa.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Immutable })
                && matches!(rsa.element_type(), CoordT { ownership: OwnershipT::Share, kind: KindT::Int(_), .. })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 3 }) => {}
        other => panic!("expected VonInt(3), got {:?}", other),
    }
}

// mig: fn new_rsa
#[test]
fn new_rsa() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  a = []int(3);\n  a.push(73);\n  a.push(42);\n  a.push(73);\n  return a.1;\n}\n",
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::LetNormal(LetNormalTE {
                variable: ILocalVariableT::Reference(ReferenceLocalVariableT {
                    name: IVarNameT::CodeVar(CodeVarNameT { name: StrI("a"), .. }),
                    coord: CoordT {
                        ownership: OwnershipT::Own,
                        kind: KindT::RuntimeSizedArray(rsa),
                        ..
                    },
                    ..
                }),
                ..
            }) if rsa.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Mutable })
                && matches!(rsa.element_type(), CoordT { ownership: OwnershipT::Share, kind: KindT::Int(_), .. })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 42 }) => {}
        other => panic!("expected VonInt(42), got {:?}", other),
    }
}

// mig: fn array_map_with_lambda
#[test]
fn array_map_with_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nstruct Lam imm {}\nfunc __call(lam Lam, i int) int { return i; }\n\nexported func main() int\nwhere F Prot = func(Lam, int)int {\n  a = #[]int(10, Lam());\n  return a.3;\n}\n",
    );
    {
        let coutputs = compile.expect_compiler_outputs();
        let main = coutputs.lookup_function_by_str("main");
        collect_only_tnode!(
            NodeRefT::FunctionDefinition(main),
            NodeRefT::NewImmRuntimeSizedArray(NewImmRuntimeSizedArrayTE {
                array_type: rsa,
                ..
            }) if rsa.mutability() == ITemplataT::Mutability(MutabilityTemplataT { mutability: MutabilityT::Immutable })
                && matches!(rsa.element_type(), CoordT { ownership: OwnershipT::Share, kind: KindT::Int(_), .. })
                => Some(())
        );
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 3 }) => {}
        other => panic!("expected VonInt(3), got {:?}", other),
    }
}

// mig: fn make_array_map_with_struct
#[test]
fn make_array_map_with_struct() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\n\nstruct Lam imm {}\nfunc __call(lam Lam, i int) int { return i; }\n\nexported func main() int {\n  a = MakeArray<int>(10, Lam());\n  return a.3;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 3 }) => {}
        other => panic!("expected VonInt(3), got {:?}", other),
    }
}

// mig: fn make_array_map_with_lambda
#[test]
fn make_array_map_with_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nexported func main() int {\n  a = MakeArray<int>(10, {_});\n  return a.3;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 3 }) => {}
        other => panic!("expected VonInt(3), got {:?}", other),
    }
}

// mig: fn array_map_with_interface
#[test]
fn array_map_with_interface() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\n\nsealed interface IThing {\n  func __call(virtual self &IThing, i int) int;\n}\n\nstruct MyThing { }\nfunc __call(self &MyThing, i int) int { i }\n\nimpl IThing for MyThing;\n\nexported func main() int {\n  i IThing = MyThing();\n  a = Array<imm, int>(10, &i);\n  return a.3;\n}\n",
    );
    {
        let _coutputs = compile.expect_compiler_outputs();
    }
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 3 }) => {}
        other => panic!("expected VonInt(3), got {:?}", other),
    }
}

// mig: fn array_map_taking_a_closure_which_captures_something
#[test]
fn array_map_taking_a_closure_which_captures_something() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nexported func main() int {\n  x = 7;\n  a = MakeImmArray<int>(10, { _ + x });\n  return a.3;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 10 }) => {}
        other => panic!("expected VonInt(10), got {:?}", other),
    }
}

// mig: fn simple_array_map_with_runtime_index_lookup
#[test]
fn simple_array_map_with_runtime_index_lookup() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nexported func main() int {\n  a = MakeImmArray<int>(10, {_});\n  i = 5;\n  return a[i];\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 5 }) => {}
        other => panic!("expected VonInt(5), got {:?}", other),
    }
}

// mig: fn nested_array
#[test]
fn nested_array() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  return [#]([#](2)).0.0;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 2 }) => {}
        other => panic!("expected VonInt(2), got {:?}", other),
    }
}

// mig: fn two_dimensional_array
#[test]
fn two_dimensional_array() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nexported func main() int {\n  board =\n      MakeArray<Array<mut, int>>(\n          3,\n          (row) => { MakeArray<int>(3, { row + _ }) });\n  return board.1.2;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 3 }) => {}
        other => panic!("expected VonInt(3), got {:?}", other),
    }
}

// mig: fn array_with_capture
#[test]
fn array_with_capture() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nstruct IntBox {\n  i int;\n}\n\nexported func main() int {\n  box = IntBox(7);\n  board = MakeArray<int>(3, &(col) => { box.i });\n  return board.1;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 7 }) => {}
        other => panic!("expected VonInt(7), got {:?}", other),
    }
}

// mig: fn capture
#[test]
fn capture() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nfunc myFunc<T, F>(generator F) T\nwhere func(&F, int)T, func drop(F)void\n{\n  return generator(9);\n}\n\nstruct IntBox {\n  i int;\n}\n\nexported func main() int {\n  box = IntBox(7);\n  lam = (col) => { box.i };\n  board = myFunc<int>(&lam);\n  return board;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 7 }) => {}
        other => panic!("expected VonInt(7), got {:?}", other),
    }
}

// mig: fn mutate_array
#[test]
fn mutate_array() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nexported func main() int {\n  arr = MakeArray<int>(3, {_});\n  set arr[1] = 1337;\n  return arr.1;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 1337 }) => {}
        other => panic!("expected VonInt(1337), got {:?}", other),
    }
}

// mig: fn capture_mutable_array
#[test]
fn capture_mutable_array() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nstruct MyIntIdentity {}\nfunc __call(this &MyIntIdentity, i int) int { return i; }\nexported func main() {\n  m = MyIntIdentity();\n  arr = MakeArray<int>(10, &m);\n  lam = { print(str(arr.6)); };\n  lam();\n}\n",
    );
    assert_eq!(compile.eval_for_stdout(Vec::new()).unwrap(), "6");
}

// mig: fn swap_out_of_array
#[test]
fn swap_out_of_array() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nstruct Goblin { }\n\nexported func main() int {\n  arr = MakeArray<Goblin>(1, i => Goblin());\n  set arr.0 = Goblin();\n  return 4;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 4 }) => {}
        other => panic!("expected VonInt(4), got {:?}", other),
    }
}

// mig: fn test_array_length
#[test]
fn test_array_length() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nexported func main() int {\n  a = MakeArray<int>(11, {_});\n  return len(&a);\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 11 }) => {}
        other => panic!("expected VonInt(11), got {:?}", other),
    }
}

// mig: fn map_using_array_construct
#[test]
fn map_using_array_construct() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nexported func main() int {\n  board = MakeArray<int>(5, {_});\n  result =\n      MakeArray<int>(5, &(i) => {\n        board[i] + 2\n      });\n  return result.2;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 4 }) => {}
        other => panic!("expected VonInt(4), got {:?}", other),
    }
}

// mig: fn map_from_hardcoded_values
#[test]
fn map_from_hardcoded_values() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nfunc toArray<N Int, E, SourceM Mutability>(seq &[#N]<SourceM>E) []E\nwhere func clone(&E)E {\n  return MakeArray<E>(N, &{ clone(seq[_]) });\n}\nexported func main() int {\n  return #[#]int(6, 4, 3, 5, 2, 8).toArray()[3];\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 5 }) => {}
        other => panic!("expected VonInt(5), got {:?}", other),
    }
}

// mig: fn nested_imm_arrays
#[test]
fn nested_imm_arrays() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nexported func main() int {\n  return #[#]#[]int(\n    #[#]int(6, 60).toImmArray(),\n    #[#]int(4, 40).toImmArray(),\n    #[#]int(3, 30).toImmArray()\n  ).toImmArray()[2][1];\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 30 }) => {}
        other => panic!("expected VonInt(30), got {:?}", other),
    }
}

// mig: fn array_foreach
#[test]
fn array_foreach() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nimport array.each.*;\nexported func main() int {\n  sum = 0;\n  [#]int(6, 60, 103)&.each(&{ set sum = sum + _; });\n  return sum;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 169 }) => {}
        other => panic!("expected VonInt(169), got {:?}", other),
    }
}

// mig: fn array_has
#[test]
fn array_has() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.has.*;\nexported func main() bool {\n  return [#]int(6, 60, 103)&.has(103);\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Bool(VonBool { value: true }) => {}
        other => panic!("expected VonBool(true), got {:?}", other),
    }
}

// mig: fn each_on_ssa
#[test]
fn each_on_ssa() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nimport array.make.*;\nimport array.iter.*;\nexported func main() {\n  planets = [#](\"Venus\", \"Earth\", \"Mars\");\n  foreach planet in planets {\n    print(planet);\n  }\n}\n",
    );
    assert_eq!(compile.eval_for_stdout(Vec::new()).unwrap(), "VenusEarthMars");
}

// mig: fn change_mutability
#[test]
fn change_mutability() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "import array.make.*;\nexported func main() str {\n  a = MakeArray<str>(10, { str(_) });\n  b = a.toImmArray();\n  return a.3;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Str(VonStr { ref value }) if value == "3" => {}
        other => panic!("expected VonStr(\"3\"), got {:?}", other),
    }
}

// mig: fn reports_when_making_new_imm_rsa_without_lambda
#[test]
fn reports_when_making_new_imm_rsa_without_lambda() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  a = #[]int(3);\n  a.push(73);\n  return a.0;\n}\n",
    );
    match compile.get_compiler_outputs() {
        Err(ICompileErrorT::NewImmRSANeedsCallable { .. }) => {}
        Err(e) => panic!("expected NewImmRSANeedsCallable, got Err({:?})", e),
        Ok(_) => panic!("expected NewImmRSANeedsCallable, got Ok"),
    }
}

// mig: fn new_immutable_array
#[test]
fn new_immutable_array() {
    let compilation_bump = bumpalo::Bump::new();
    let parse_bump = bumpalo::Bump::new();
    let scout_bump = bumpalo::Bump::new();
    let typing_bump = bumpalo::Bump::new();
    let instantiating_bump = bumpalo::Bump::new();
    let hammer_bump = bumpalo::Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let hammer_interner = HammerInterner::new(&hammer_bump);
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        "\nexported func main() int {\n  arr = Array<mut, int>(3);\n  arr.push(13);\n  arr.push(14);\n  arr.push(15);\n  immArr = toImmArray(&arr);\n  return immArr[1];\n}\n\nfunc toImmArray<E Ref imm>(arr &[]E) Array<imm, E> {\n  Array<imm, E>(arr.len(), &{ arr[_] })\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 14 }) => {}
        other => panic!("expected VonInt(14), got {:?}", other),
    }
}

