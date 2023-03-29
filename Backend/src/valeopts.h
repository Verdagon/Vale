
#ifndef valeopts_h
#define valeopts_h

#include <string>
#include <stdint.h>
#include <stddef.h>

enum class RegionOverride {
//  ASSIST,
  NAIVE_RC,
//  RESILIENT_V0,
//  RESILIENT_V1,
//  RESILIENT_V2,
  RESILIENT_V3,
  SAFE_BASELINE,
  SAFE,
  FAST
};

// Compiler options
struct ValeOptions {
//    std::string srcpath;    // Full path
//    std::string srcDir;    // Just the directory
//    std::string srcNameNoExt;    // Just the name of the file, without extension
//    std::string srcDirAndNameNoExt;    // Just the name of the file, without extension

    std::string outputDir;

    std::string triple;
    std::string cpu;
    std::string features;

    void* data = nullptr; // User-defined data for unit test callbacks

    // Boolean flags
    bool wasm = false;        // 1=WebAssembly
    int optLevel = 0;   // O0-O3
    bool library = false;    // 1=generate a C-API compatible static library
    bool pic = false;        // Compile using position independent code
    bool verify = false;        // Verify LLVM IR
    bool debug = false;
    bool print_asm = false;        // Print out assembly file
    bool print_llvmir = false;    // Print out LLVM IR
    bool docs = false;            // Generate code documentation
    bool census = false;    // Enable census checking
    bool flares = false;    // Enable flare output
    bool fastCrash = false;    // Enable single-instruction crash, a bit faster
    bool elideChecksForKnownLive = false;    // Enables generational heap
    bool includeBoundsChecks = true;
    bool overrideKnownLiveTrue = false;    // Enables generational heap
    bool printMemOverhead = false;    // Enables generational heap
    bool enableReplaying = false;    // Enables deterministic replaying
    bool enableSideCalling = false;    // Enables side calling, used for fearless FFI

    RegionOverride regionOverride = RegionOverride::RESILIENT_V3;
};

int valeOptSet(ValeOptions *opt, int *argc, char **argv);

#endif
