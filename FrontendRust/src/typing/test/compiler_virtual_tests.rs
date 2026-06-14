use bumpalo::Bump;
use crate::interner::StrI;
use crate::keywords::Keywords;
use crate::parse_arena::ParseArena;
use crate::scout_arena::ScoutArena;
use crate::typing::names::names::{FunctionNameT, FunctionTemplateNameT, IdT, INameT};
use crate::typing::test::compiler_test_compilation::compiler_test_compilation;
use crate::utils::code_hierarchy::{self, IPackageResolver, PackageCoordinate};
use std::collections::HashMap;
use crate::typing::typing_interner::TypingInterner;
use crate::builtins::builtins::get_embedded_modulized_code_map;
use crate::tests::tests::get_package_to_resource_resolver;
use crate::tests::tests::load_expected;


// mig: fn regular_interface_and_struct
#[test]
fn regular_interface_and_struct() {

    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\nsealed interface Opt { }\n\nstruct Some { x int; }\nimpl Opt for Some;\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    let coutputs = compile.expect_compiler_outputs();

    let drop_func_names: Vec<_> = coutputs.functions.iter().map(|f| f.header.id).filter_map(|f| {
        match f {
            id @ IdT { local_name: INameT::Function(FunctionNameT { template: FunctionTemplateNameT { human_name: StrI("drop"), .. }, .. }), .. } => Some(id),
            _ => None,
        }
    }).collect();
    assert_eq!(drop_func_names.len(), 2);

    let interface = coutputs.lookup_interface_by_human_name("Opt");
    let _ = interface.internal_methods;
}

// mig: fn regular_open_interface_and_struct_no_anonymous_interface
#[test]
fn regular_open_interface_and_struct_no_anonymous_interface() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveAnonymousSubstruct\ninterface Opt { }\n\nstruct Some { x int; }\nimpl Opt for Some;\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    let coutputs = compile.expect_compiler_outputs();
    let drop_func_names: Vec<_> = coutputs.functions.iter().map(|f| f.header.id).filter_map(|f| {
        match f {
            id @ IdT { local_name: INameT::Function(FunctionNameT { template: FunctionTemplateNameT { human_name: StrI("drop"), .. }, .. }), .. } => Some(id),
            _ => None,
        }
    }).collect();
    assert_eq!(drop_func_names.len(), 2);
}

// mig: fn implementing_two_interfaces_causes_no_vdrop_conflict
#[test]
fn implementing_two_interfaces_causes_no_vdrop_conflict() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\nstruct MyStruct {}\n\ninterface IA {}\nimpl IA for MyStruct;\n\ninterface IB {}\nimpl IB for MyStruct;\n\nfunc bork(a IA) {}\nfunc zork(b IB) {}\nexported func main() {\n  bork(MyStruct());\n  zork(MyStruct());\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn upcast
#[test]
fn upcast() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n\ninterface IShip {}\nstruct Raza { fuel int; }\nimpl IShip for Raza;\n\nexported func main() {\n  ship IShip = Raza(42);\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn virtual_with_body
#[test]
fn virtual_with_body() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\ninterface IBork { }\nstruct Bork { }\nimpl IBork for Bork;\n\nfunc rebork(virtual result *IBork) bool { true }\nexported func main() {\n  rebork(&Bork());\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let _compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
}

// mig: fn templated_interface_and_struct
#[test]
fn templated_interface_and_struct() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\nsealed interface Opt<T Ref>\nwhere func drop(T)void\n{ }\n\nstruct Some<T>\nwhere func drop(T)void\n{ x T; }\n\nimpl<T> Opt<T> for Some<T>\nwhere func drop(T)void;\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    let coutputs = compile.expect_compiler_outputs();
    let drop_func_names: Vec<_> = coutputs.functions.iter().map(|f| f.header.id).filter_map(|f| {
        match f {
            id @ IdT { local_name: INameT::Function(FunctionNameT { template: FunctionTemplateNameT { human_name: StrI("drop"), .. }, .. }), .. } => Some(id),
            _ => None,
        }
    }).collect();
    assert_eq!(drop_func_names.len(), 2);
}

// mig: fn custom_drop_with_concept_function
#[test]
fn custom_drop_with_concept_function() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveInterfaceDrop\nsealed interface Opt<T Ref> { }\n\nabstract func drop<T>(virtual opt Opt<T>)\nwhere func drop(T)void;\n\n#!DeriveStructDrop\nstruct Some<T> { x T; }\nimpl<T> Opt<T> for Some<T>;\n\nfunc drop<T>(opt Some<T>)\nwhere func drop(T)void\n{\n  [x] = opt;\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn test_complex_interface
#[test]
fn test_complex_interface() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = load_expected("programs/genericvirtuals/templatedinterface.vale");
    let resolver = get_embedded_modulized_code_map(&parse_arena, &parser_keywords)
        .or(code_hierarchy::test_from_vec(&parse_arena, vec![code]))
        .or(get_package_to_resource_resolver());
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn test_specializing_interface
#[test]
fn test_specializing_interface() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = load_expected("programs/genericvirtuals/specializeinterface.vale");
    let resolver = get_embedded_modulized_code_map(&parse_arena, &parser_keywords)
        .or(code_hierarchy::test_from_vec(&parse_arena, vec![code]))
        .or(get_package_to_resource_resolver());
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn use_bound_from_struct
#[test]
fn use_bound_from_struct() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveStructDrop\nstruct BorkForwarder<Lam>\nwhere func __call(&Lam)int // 3\n{\n  lam Lam;\n}\n\n\nfunc bork<Lam>( // 1\n  self &BorkForwarder<Lam> // 2\n) int {\n  return (self.lam)();\n}\n\nexported func main() {\n  b = BorkForwarder({ 7 });\n  b.bork();\n  [_] = b;\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn basic_interface_forwarder
#[test]
fn basic_interface_forwarder() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveInterfaceDrop\nsealed interface Bork {\n  func bork(virtual self &Bork) int;\n}\n\n#!DeriveStructDrop\nstruct BorkForwarder<Lam>\nwhere func drop(Lam)void, func __call(&Lam)int {\n  lam Lam;\n}\n\nimpl<Lam> Bork for BorkForwarder<Lam>;\n\nfunc bork<Lam>(self &BorkForwarder<Lam>) int {\n  return (self.lam)();\n}\n\nexported func main() int {\n  f = BorkForwarder({ 7 });\n  z = f.bork();\n  [_] = f;\n  return z;\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn generic_interface_forwarder
#[test]
fn generic_interface_forwarder() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveInterfaceDrop\nsealed interface Bork<T Ref> {\n  func bork(virtual self &Bork<T>) int;\n}\n\n#!DeriveStructDrop\nstruct BorkForwarder<T Ref, Lam>\nwhere func drop(Lam)void, func __call(&Lam)T {\n  lam Lam;\n}\n\nimpl<T, Lam> Bork<T> for BorkForwarder<T, Lam>;\n\nfunc bork<T, Lam>(self &BorkForwarder<T, Lam>) T {\n  return (self.lam)();\n}\n\nexported func main() int {\n  f = BorkForwarder<int>({ 7 });\n  z = f.bork();\n  [_] = f;\n  return z;\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn generic_interface_forwarder_with_bound
#[test]
fn generic_interface_forwarder_with_bound() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveInterfaceDrop\nsealed interface Bork<T Ref>\nwhere func threeify(T)T {\n  func bork(virtual self &Bork<T>) int;\n}\n\n#!DeriveStructDrop\nstruct BorkForwarder<T Ref, Lam>\nwhere func drop(Lam)void, func __call(&Lam)T, func threeify(T)T {\n  lam Lam;\n}\n\nimpl<T, Lam> Bork<T> for BorkForwarder<T, Lam>;\n\nfunc bork<T, Lam>(self &BorkForwarder<T, Lam>) T {\n  return (self.lam)().threeify();\n}\n\nfunc threeify(x int) int { 3 }\n\nexported func main() int {\n  f = BorkForwarder<int>({ 7 });\n  z = f.bork();\n  [_] = f;\n  return z;\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn basic_interface_anonymous_subclass
#[test]
fn basic_interface_anonymous_subclass() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\ninterface Bork {\n  func bork(virtual self &Bork) int;\n}\n\nexported func main() int {\n  f = Bork({ 7 });\n  return f.bork();\n}\n\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn integer_is_compatible_with_interface_anonymous_substruct
#[test]
fn integer_is_compatible_with_interface_anonymous_substruct() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\nimport v.builtins.drop.*;\ninterface AFunction2<R Ref, P1 Ref> {\n  func doCall(virtual this &AFunction2<R, P1>, a P1) R;\n}\nfunc __call(x6 int, x42 int)str { \"hi\" }\nexported func main() str {\n  func = AFunction2<str, int>(6);\n  return func.doCall(42);\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(get_embedded_modulized_code_map(&parse_arena, &parser_keywords))
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn lambda_is_compatible_with_interface_anonymous_substruct
#[test]
fn lambda_is_compatible_with_interface_anonymous_substruct() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\nimport v.builtins.str.*;\n\ninterface AFunction2<R Ref, P1 Ref> {\n  func __call(virtual this &AFunction2<R, P1>, a P1) R;\n}\nexported func main() str {\n  func = AFunction2<str, int>((i) => { str(i) });\n  return func(42);\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(get_embedded_modulized_code_map(&parse_arena, &parser_keywords))
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn implementing_a_non_generic_interface_call
#[test]
fn implementing_a_non_generic_interface_call() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\n#!DeriveInterfaceDrop\ninterface IObserver<T Ref> { }\n\n#!DeriveStructDrop\nstruct MyThing { }\n\nimpl<T> IObserver<T> for MyThing;\n\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

// mig: fn anonymous_substruct_8
#[test]
fn anonymous_substruct_8() {
    let parse_bump = Bump::new();
    let scout_bump = Bump::new();
    let typing_bump = Bump::new();
    let parse_arena = ParseArena::new(&parse_bump);
    let scout_arena = ScoutArena::new(&scout_bump);
    let keywords = Keywords::new_for_scout(&scout_arena);
    let parser_keywords = Keywords::new_for_parse(&parse_arena);
    let code = "\nimport v.builtins.arrays.*;\n//import array.make.*;\n\ninterface IThing {\n  func __call(virtual self &IThing, i int) int;\n}\n\nstruct MyThing { }\nfunc __call(self &MyThing, i int) int { i }\n\nimpl IThing for MyThing;\n\nexported func main() int {\n  i IThing = MyThing();\n  a = Array<imm, int>(10, &i);\n  return a.3;\n}\n";
    let resolver = code_hierarchy::test_from_vec(&parse_arena, vec![code.to_string()])
        .or(get_embedded_modulized_code_map(&parse_arena, &parser_keywords))
        .or(|_: &PackageCoordinate<'_>| -> Option<HashMap<String, String>> { None });
    let typing_interner = TypingInterner::new(&typing_bump);
    let mut compile = compiler_test_compilation(&typing_interner, &scout_arena, &keywords, &parser_keywords, &parse_arena, &resolver);
    compile.expect_compiler_outputs();
}

