// Verbatim port of the test catalog in Tester/src/main.vale lines 235-385. Same
// test names, same expected return codes, same per-region gates. Each
// `suite.start_test(42, "addret", &samples.join("programs/addret.vale"), &[], region,
// false, None)` corresponds 1:1 to a `suite.StartTest(42, "addret", ...)` in main.vale.

use crate::suite::TestSuite;
use std::path::PathBuf;

pub fn register(suite: &mut TestSuite) {
    // Mirrors main.vale line 206:
    //     samples_path = frontend_tests_dir./("Tests/test/main/resources")
    let samples: PathBuf = suite
        .opts
        .frontend_tests_dir
        .join("Tests/test/main/resources");

    // Hoist these to locals so the per-test calls can take `&mut suite` without colliding
    // with a long-lived `suite.opts.*` borrow.
    let include_v3 = has_region(suite, "resilient-v3");
    let stdlib_dir = suite.opts.stdlib_dir.clone();
    let regions: Vec<String> = suite.opts.include_regions.clone();

    // resilient-v3-only tests (main.vale lines 237-284).
    if include_v3 {
        let r = "resilient-v3";
        suite.start_replay_test(42, 84, "simpleexternreturn", &samples.join("programs/externs/simpleexternreturn"), &[], r, false, None);
        suite.start_replay_test(42, 84, "simpleexternparam",  &samples.join("programs/externs/simpleexternparam"),  &[], r, false, None);
        suite.start_test(7,  "addret",          &samples.join("programs/addret.vale"),          &[], r, false, None);
        suite.start_test(42, "add64ret",        &samples.join("programs/add64ret.vale"),        &[], r, false, None);
        suite.start_test(42, "comparei64",      &samples.join("programs/comparei64.vale"),      &[], r, false, None);
        suite.start_test(42, "truncate",        &samples.join("programs/truncate.vale"),        &[], r, false, None);
        suite.start_test(42, "floatarithmetic", &samples.join("programs/floatarithmetic.vale"), &[], r, false, None);
        suite.start_test(42, "floateq",         &samples.join("programs/floateq.vale"),         &[], r, false, None);
        suite.start_test(42, "concatstrfloat",  &samples.join("programs/concatstrfloat.vale"),  &[], r, false, None);
        suite.start_test(42, "voidreturnexport", &samples.join("programs/externs/voidreturnexport"), &[], r, false, None);
        suite.start_replay_test(42, 84, "structimmreturnextern",    &samples.join("programs/externs/structimmreturnextern"),    &[], r, false, None);
        suite.start_replay_test(42, 84, "structimmreturnexport",    &samples.join("programs/externs/structimmreturnexport"),    &[], r, false, None);
        suite.start_replay_test(42, 84, "structimmparamextern",     &samples.join("programs/externs/structimmparamextern"),     &[], r, false, None);
        suite.start_replay_test(42, 84, "structimmparamexport",     &samples.join("programs/externs/structimmparamexport"),     &[], r, false, None);
        suite.start_replay_test(42, 84, "structimmparamdeepextern", &samples.join("programs/externs/structimmparamdeepextern"), &[], r, false, None);
        suite.start_replay_test(42, 84, "structimmparamdeepexport", &samples.join("programs/externs/structimmparamdeepexport"), &[], r, false, None);
        suite.start_test(42, "interfaceimmparamextern",     &samples.join("programs/externs/interfaceimmparamextern"),     &[], r, false, None);
        suite.start_test(42, "interfaceimmparamexport",     &samples.join("programs/externs/interfaceimmparamexport"),     &[], r, false, None);
        suite.start_test(42, "interfaceimmparamdeepextern", &samples.join("programs/externs/interfaceimmparamdeepextern"), &[], r, false, None);
        suite.start_test(42, "interfaceimmparamdeepexport", &samples.join("programs/externs/interfaceimmparamdeepexport"), &[], r, false, None);
        suite.start_test(42, "interfaceimmreturnextern",    &samples.join("programs/externs/interfaceimmreturnextern"),    &[], r, false, None);
        suite.start_test(42, "interfaceimmreturnexport",    &samples.join("programs/externs/interfaceimmreturnexport"),    &[], r, false, None);
        suite.start_replay_test(42, 84, "rsaimmreturnextern", &samples.join("programs/externs/rsaimmreturnextern"), &[], r, false, None);
        suite.start_test(42, "rsaimmreturnexport", &samples.join("programs/externs/rsaimmreturnexport"), &[], r, false, None);
        suite.start_replay_test(10, 20, "rsaimmparamextern", &samples.join("programs/externs/rsaimmparamextern"), &[], r, false, None);
        suite.start_replay_test(10, 20, "rsaimmparamexport", &samples.join("programs/externs/rsaimmparamexport"), &[], r, false, None);
        suite.start_replay_test(20, 40, "rsaimmparamdeepextern", &samples.join("programs/externs/rsaimmparamdeepextern"), &[], r, false, None);
        suite.start_replay_test(42, 84, "rsaimmparamdeepexport", &samples.join("programs/externs/rsaimmparamdeepexport"), &[], r, false, None);
        suite.start_replay_test(42, 84, "ssaimmparamextern", &samples.join("programs/externs/ssaimmparamextern"), &[], r, false, None);
        suite.start_replay_test(42, 84, "ssaimmparamexport", &samples.join("programs/externs/ssaimmparamexport"), &[], r, false, None);
        suite.start_replay_test(42, 84, "ssaimmreturnextern", &samples.join("programs/externs/ssaimmreturnextern"), &[], r, false, None);
        suite.start_test(42, "ssaimmreturnexport", &samples.join("programs/externs/ssaimmreturnexport"), &[], r, false, None);
        suite.start_replay_test(42, 84, "ssaimmparamdeepextern", &samples.join("programs/externs/ssaimmparamdeepextern"), &[], r, false, None);
        suite.start_replay_test(42, 84, "ssaimmparamdeepexport", &samples.join("programs/externs/ssaimmparamdeepexport"), &[], r, false, None);
        suite.start_replay_test(6, 12, "strreturnexport", &samples.join("programs/externs/strreturnexport"), &[], r, false, None);
        suite.start_replay_test(11, 22, "strlenextern",    &samples.join("programs/externs/strlenextern"),    &[], r, false, None);
        suite.start_test(42, "smallstr",           &samples.join("programs/strings/smallstr.vale"),           &[], r, false, None);
        suite.start_test(42, "immtupleaccess",     &samples.join("programs/tuples/immtupleaccess.vale"),       &[], r, false, None);
        suite.start_test(42, "ssaimmfromcallable", &samples.join("programs/arrays/ssaimmfromcallable.vale"),  &[], r, false, None);
        suite.start_test(42, "ssaimmfromvalues",   &samples.join("programs/arrays/ssaimmfromvalues.vale"),    &[], r, false, None);
        // Commented-out replayprint / roguelike tests omitted, matching main.vale.
    }

    // kldc (main.vale lines 301-304) — resilient-v3, two extra build flag pairs.
    if include_v3 {
        let extra = vec![
            "--force_all_known_live".to_string(),
            "true".to_string(),
            "--elide_checks_for_known_live".to_string(),
            "false".to_string(),
        ];
        suite.start_test(116, "kldc", &samples.join("programs/structs/deadmutstruct.vale"), &extra, "resilient-v3", false, None);
    }

    // Per-region loop from main.vale lines 311-369. Naive-rc excludes the mut
    // extern/export tests at the bottom (main.vale line 358).
    for region in &regions {
        let r = region.as_str();
        suite.start_test(42, "mutswaplocals",           &samples.join("programs/mutswaplocals.vale"),                       &[], r, false, None);
        suite.start_test(5,  "structimm",               &samples.join("programs/structs/structimm.vale"),                   &[], r, false, None);
        suite.start_test(5,  "memberrefcount",          &samples.join("programs/structs/memberrefcount.vale"),              &[], r, false, None);
        suite.start_test(42, "bigstructimm",            &samples.join("programs/structs/bigstructimm.vale"),                &[], r, false, None);
        suite.start_test(8,  "structmut",               &samples.join("programs/structs/structmut.vale"),                   &[], r, false, None);
        suite.start_test(42, "restackify",              &samples.join("programs/restackify.vale"),                          &[], r, false, None);
        suite.start_test(42, "destructure_restackify",  &samples.join("programs/destructure_restackify.vale"),              &[], r, false, None);
        suite.start_test(42, "loop_restackify",         &samples.join("programs/loop_restackify.vale"),                     &[], r, false, None);
        suite.start_test(42, "lambda",                  &samples.join("programs/lambdas/lambda.vale"),                      &[], r, false, None);
        suite.start_test(42, "if",                      &samples.join("programs/if/if.vale"),                               &[], r, false, None);
        suite.start_test(42, "upcastif",                &samples.join("programs/if/upcastif.vale"),                         &[], r, false, None);
        suite.start_test(42, "ifnevers",                &samples.join("programs/if/ifnevers.vale"),                         &[], r, false, None);
        suite.start_test(42, "mutlocal",                &samples.join("programs/mutlocal.vale"),                            &[], r, false, None);
        suite.start_test(42, "while",                   &samples.join("programs/while/while.vale"),                         &[], r, false, None);
        suite.start_test(8,  "constraintRef",           &samples.join("programs/constraintRef.vale"),                       &[], r, false, None);
        suite.start_test(42, "ssamutfromcallable",      &samples.join("programs/arrays/ssamutfromcallable.vale"),           &[], r, false, None);
        suite.start_test(42, "ssamutfromvalues",        &samples.join("programs/arrays/ssamutfromvalues.vale"),             &[], r, false, None);
        suite.start_test(42, "interfaceimm",            &samples.join("programs/virtuals/interfaceimm.vale"),               &[], r, false, None);
        suite.start_test(42, "interfacemut",            &samples.join("programs/virtuals/interfacemut.vale"),               &[], r, false, None);
        suite.start_test(42, "structmutstore",          &samples.join("programs/structs/structmutstore.vale"),              &[], r, false, None);
        suite.start_test(42, "structmutstoreinner",     &samples.join("programs/structs/structmutstoreinner.vale"),         &[], r, false, None);
        suite.start_test(3,  "rsaimm",                  &samples.join("programs/arrays/rsaimm.vale"),                       &[], r, false, None);
        suite.start_test(3,  "rsamut",                  &samples.join("programs/arrays/rsamut.vale"),                       &[], r, false, None);
        suite.start_test(42, "rsamutdestroyintocallable", &samples.join("programs/arrays/rsamutdestroyintocallable.vale"), &[], r, false, None);
        suite.start_test(42, "ssamutdestroyintocallable", &samples.join("programs/arrays/ssamutdestroyintocallable.vale"), &[], r, false, None);
        suite.start_test(5,  "rsamutlen",               &samples.join("programs/arrays/rsamutlen.vale"),                    &[], r, false, None);
        suite.start_test(42, "rsamutcapacity",          &samples.join("programs/arrays/rsamutcapacity.vale"),               &[], r, false, None);
        suite.start_test(42, "stradd",                  &samples.join("programs/strings/stradd.vale"),                      &[], r, false, None);
        suite.start_test(42, "strneq",                  &samples.join("programs/strings/strneq.vale"),                      &[], r, false, None);
        suite.start_test(42, "lambdamut",               &samples.join("programs/lambdas/lambdamut.vale"),                   &[], r, false, None);
        suite.start_test(42, "strprint",                &samples.join("programs/strings/strprint.vale"),                    &[], r, false, None);
        suite.start_test(4,  "inttostr",                &samples.join("programs/strings/inttostr.vale"),                    &[], r, false, None);
        suite.start_test(4,  "i64tostr",                &samples.join("programs/strings/i64tostr.vale"),                    &[], r, false, None);
        suite.start_test(42, "nestedif",                &samples.join("programs/if/nestedif.vale"),                         &[], r, false, None);
        suite.start_test(42, "unstackifyret",           &samples.join("programs/unstackifyret.vale"),                       &[], r, false, None);
        suite.start_test(42, "swaprsamutdestroy",       &samples.join("programs/arrays/swaprsamutdestroy.vale"),            &[], r, false, None);
        suite.start_test(42, "downcastBorrowSuccessful",  &samples.join("programs/downcast/downcastBorrowSuccessful.vale"), &[], r, false, None);
        suite.start_test(42, "downcastBorrowFailed",      &samples.join("programs/downcast/downcastBorrowFailed.vale"),     &[], r, false, None);
        suite.start_test(42, "downcastOwningSuccessful",  &samples.join("programs/downcast/downcastOwningSuccessful.vale"), &[], r, false, None);
        suite.start_test(42, "downcastOwningFailed",      &samples.join("programs/downcast/downcastOwningFailed.vale"),     &[], r, false, None);
        suite.start_test(42, "unreachablemoot",         &samples.join("programs/unreachablemoot.vale"),                     &[], r, false, None);
        suite.start_test(1,  "panic",                   &samples.join("programs/panic.vale"),                               &[], r, false, None);
        suite.start_test(42, "panicnot",                &samples.join("programs/panicnot.vale"),                            &[], r, false, None);
        suite.start_test(42, "nestedblocks",            &samples.join("programs/nestedblocks.vale"),                        &[], r, false, None);
        suite.start_test(42, "restackify",              &samples.join("programs/restackify.vale"),                          &[], r, false, None);

        if r != "naive-rc" {
            suite.start_test(42, "interfacemutreturnexport",    &samples.join("programs/externs/interfacemutreturnexport"),    &[], r, false, None);
            suite.start_test(42, "interfacemutparamexport",     &samples.join("programs/externs/interfacemutparamexport"),     &[], r, false, None);
            suite.start_test(42, "structmutreturnexport",       &samples.join("programs/externs/structmutreturnexport"),       &[], r, false, None);
            suite.start_test(42, "structmutparamexport",        &samples.join("programs/externs/structmutparamexport"),        &[], r, false, None);
            suite.start_test(42, "structmutparamdeepexport",    &samples.join("programs/externs/structmutparamdeepexport"),    &[], r, false, None);
            suite.start_test(10, "rsamutparamexport",           &samples.join("programs/externs/rsamutparamexport"),           &[], r, false, None);
            suite.start_test(42, "rsamutreturnexport",          &samples.join("programs/externs/rsamutreturnexport"),          &[], r, false, None);
            suite.start_test(10, "ssamutparamexport",           &samples.join("programs/externs/ssamutparamexport"),           &[], r, false, None);
            suite.start_test(42, "ssamutreturnexport",          &samples.join("programs/externs/ssamutreturnexport"),          &[], r, false, None);
        }
    }

    // Stdlib + foreach (main.vale lines 376-382). resilient-v3 only, include_stdlib true.
    if include_v3 {
        suite.start_test(10, "foreach", &samples.join("programs/while/foreach.vale"), &[], "resilient-v3", true, None);
        suite.start_test(0,  "stdlib_list",    &stdlib_dir.join("src/collections/list/test"),    &[], "resilient-v3", true, None);
        suite.start_test(0,  "stdlib_hashset", &stdlib_dir.join("src/collections/hashset/test"), &[], "resilient-v3", true, None);
        suite.start_test(0,  "stdlib_hashmap", &stdlib_dir.join("src/collections/hashmap/test"), &[], "resilient-v3", true, None);
    }
}

fn has_region(suite: &TestSuite, name: &str) -> bool {
    suite.opts.include_regions.iter().any(|s| s == name)
}
