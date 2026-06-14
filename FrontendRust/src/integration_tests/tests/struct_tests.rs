use crate::integration_tests::tests::run_compilation::test;
use crate::integration_tests::tests::run_compilation::test_no_builtins;
use crate::keywords::Keywords;
use crate::parse_arena::ParseArena;
use crate::scout_arena::ScoutArena;
use crate::simplifying::hammer_interner::HammerInterner;
use crate::tests::tests::load_expected;
use crate::testvm::vivem::VmRuntimeErrorV;
use crate::typing::typing_interner::TypingInterner;
use crate::utils::utils::replace_all;
use crate::utils::utils::scrambles;
use crate::von::ast::IVonData;
use crate::von::ast::VonInt;
// mig: struct StructTests

// mig: fn make_empty_imm_struct
#[test]
fn make_empty_imm_struct() {
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
        "struct Marine imm {}\nexported func main() {\n  Marine();\n}\n",
    );
    compile.run_primitive_args(Vec::new()).unwrap();
}

// mig: fn make_imm_struct_with_one_member
#[test]
fn make_imm_struct_with_one_member() {
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
        "struct Marine imm { hp int; }\nexported func main() {\n  Marine(7);\n}\n",
    );
    compile.run_primitive_args(Vec::new()).unwrap();
}

// mig: fn make_nested_imm_struct
#[test]
fn make_nested_imm_struct() {
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
        "struct Weapon imm { ammo int; }\nstruct Marine imm { hp int; weapon Weapon; }\nexported func main() {\n  Marine(5, Weapon(7));\n}\n",
    );
    compile.run_primitive_args(Vec::new()).unwrap();
}

// mig: fn make_empty_mut_struct
#[test]
fn make_empty_mut_struct() {
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
        "struct Marine {}\nexported func main() {\n  Marine();\n}\n",
    );
    compile.run_primitive_args(Vec::new()).unwrap();
}

// mig: fn constructor_with_self
#[test]
fn constructor_with_self() {
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
    let source = load_expected("programs/structs/constructor.vale");
    let mut compile = test(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        source.as_str(),
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 10 }) => {}
        other => panic!("Expected VonInt(10), got {:?}", other),
    }
}

// mig: fn make_struct
#[test]
fn make_struct() {
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
        "struct Marine { hp int; }\nexported func main() {\n  Marine(9);\n}\n",
    );
    compile.run_primitive_args(Vec::new()).unwrap();
}

// mig: fn make_struct_and_get_member
#[test]
fn make_struct_and_get_member() {
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
    let source = load_expected("programs/structs/getMember.vale");
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test_no_builtins(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        source.as_str(),
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 9 }) => {}
        other => panic!("Expected VonInt(9), got {:?}", other),
    }
}

// mig: fn mutate_struct
#[test]
fn mutate_struct() {
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
    let source = load_expected("programs/structs/mutate.vale");
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = test_no_builtins(
        &compilation_bump,
        &hammer_interner, &typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena,
        &instantiating_bump,
        source.as_str(),
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 4 }) => {}
        other => panic!("Expected VonInt(4), got {:?}", other),
    }
}

// mig: fn normal_destructure
#[test]
fn normal_destructure() {
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
        "struct Marine {\n  hp int;\n  ammo int;\n}\nexported func main() int {\n  m = Marine(4, 7);\n  Marine[hp, ammo] = m;\n  return ammo;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 7 }) => {}
        other => panic!("Expected VonInt(7), got {:?}", other),
    }
}

// mig: fn sugar_destructure
#[test]
fn sugar_destructure() {
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
        "struct Marine {\n  hp int;\n  ammo int;\n}\nexported func main() int {\n  m = Marine(4, 7);\n  destruct m;\n  return 9;\n}\n",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()).unwrap() {
        IVonData::Int(VonInt { value: 9 }) => {}
        other => panic!("Expected VonInt(9), got {:?}", other),
    }
}

// mig: fn destroy_members_at_right_times
#[test]
fn destroy_members_at_right_times() {
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
        "import printutils.*;\n\n#!DeriveStructDrop\nstruct Weapon { }\nfunc drop(weapon Weapon) {\n  println(\"Destroying weapon!\");\n  Weapon[ ] = weapon;\n}\n#!DeriveStructDrop\nstruct Marine {\n  weapon Weapon;\n}\nfunc drop(marine Marine) {\n  println(\"Destroying marine!\");\n  Marine[weapon] = marine;\n}\nexported func main() {\n  Marine(Weapon());\n}\n",
    );
    assert_eq!(compile.eval_for_stdout(Vec::new()).unwrap(), "Destroying marine!\nDestroying weapon!\n");
}


// mig: fn panic_function
#[test]
fn panic_function() {
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
        "\nimport v.builtins.panic.*;\nimport v.builtins.drop.*;\n\nsealed interface XOpt<T Ref>\nwhere func drop(T)void {\n  func get(virtual opt &XOpt<T>) &T;\n}\n\nstruct XNone<T Ref> where func drop(T)void  { }\nimpl<T> XOpt<T> for XNone<T>;\n\nfunc get<T>(opt &XNone<T>) &T {\n  __vbi_panic();\n}\n\nexported func main() int {\n  m XOpt<int> = XNone<int>();\n  return m.get();\n}\n      ",
    );
    match compile.eval_for_kind_primitive_args(Vec::new()) {
        Err(VmRuntimeErrorV::PanicException(_)) => {}
        other => panic!("Expected PanicException, got {:?}", other),
    }
}

// mig: fn odmfrc
#[test]
fn odmfrc() {
    let code = "\nimport v.builtins.opt.*;\n\nstruct _X { }\nfunc __call(self &_X) int { 0 }\n\nstruct _Y<H>\nwhere func(&H)int, func drop(H)void {\n  hasher H;\n}\n\nstruct _Z {\n  idByName _Y<_X>;\n}\n    ";

    let replacements_set: Vec<indexmap::IndexMap<&str, &str>> = scrambles(&{
        let mut m = indexmap::IndexMap::new();
        m.insert("_X", "_A");
        m.insert("_Y", "_B");
        m.insert("_Z", "_C");
        m
    });
    for replacements in &replacements_set {
        let replaced_code = replace_all(code, replacements);
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
            &replaced_code,
        );
        let _ = compile.get_monouts();
    }
}

