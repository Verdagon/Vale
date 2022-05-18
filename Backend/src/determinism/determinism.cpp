#include <function/expressions/expressions.h>
#include <region/rcimm/rcimm.h>
#include <region/linear/linear.h>
#include <utils/definefunction.h>
#include <utils/branch.h>
#include <utils/flags.h>
#include <utils/counters.h>
#include <function/boundary.h>
#include "determinism.h"

static const uint64_t RECORDING_FILE_CONSTANT = 0x4a1e133713371337ULL;

const std::string VALE_REPLAY_FLAG = "--vale_replay";
const std::string VALE_RECORD_FLAG = "--vale_record";

static const std::string recordedRefToReplayedRefMapTypeName = "__vale_replayed__RecordedRefToReplayedRefMap";
static const std::string recordedRefToReplayedRefMapInstanceName = "__vale_replayed__recordedRefToReplayedRefMap";
static const std::string functionsMapTypeName = "__vale_replayer__FunctionsMap";
static const std::string functionsMapInstanceName = "__vale_replayer__functionsMap";
static const std::string replayerFuncPrefix = "__vale_replayer__";
static const std::string maybeStartDeterministicModeFuncName = "__vale_determinism_maybe_start";
static const std::string startRecordingFuncName = "__vale_determinism_start_recording";
static const std::string startReplayingFuncName = "__vale_determinism_start_replaying";
static const std::string writeCallBeginToFileFuncName = "__vale_determinism_record_call_begin";
static const std::string writeRefToFileFuncName = "__vale_determinism_record_ref";
//static const std::string writeValueToFileFuncName = "__vale_determinism_record_value";
static const std::string recordCallEndFuncName = "__vale_determinism_record_call_end";
static const std::string matchCallFromRecordingFileFuncName = "__vale_determinism_replay_call_begin";
static const std::string mapRefFromRecordingFileFuncName = "__vale_determinism_replay_map_ref";
//static const std::string readValueFromFileFuncName = "__vale_determinism_replay_read_value";
//static const std::string getNextExportCallStringFuncName = "__vale_determinism_replay_get_next_export_call";
static const std::string getMaybeReplayerFuncForNextExportNameFuncName = "__vale_determinism_get_maybe_replayer_func_for_next_export";


static const std::string replayerMapName = "__vale_export_func_name_to_replayer_func_map";


LLVMValueRef calcPaddedStrLen(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthNotIncludingNullTerminatorLE) {
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int64Size = LLVMABISizeOfType(globalState->dataLayout, int64LT);

  auto lengthIncludingNullTerminatorLE =
      LLVMBuildAdd(builder, lengthNotIncludingNullTerminatorLE, constI64LE(globalState, 1), "");
  auto lengthIncludingNullTerminatorAndPaddingLE =
      roundUp(
          globalState, builder, int64Size, lengthIncludingNullTerminatorLE);
  return lengthIncludingNullTerminatorAndPaddingLE;
}

LLVMTypeRef makeReplayerFuncLT(GlobalState* globalState) {
  // No params, it gets the FILE* from a global / thread local.
  return LLVMFunctionType(LLVMVoidTypeInContext(globalState->context), nullptr, 0, false);
}

Determinism::Determinism(GlobalState* globalState_) :
    globalState(globalState_),
    functionsMap(PrototypeNameSimpleStringHasher{}, PrototypeNameSimpleStringEquator{}),
    exportNameToReplayerFunctionMapGlobalLE(nullptr),
    recordedRefToReplayedRefMapGlobalLE(nullptr) {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int1LT = LLVMInt1TypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int8PtrPtrLT = LLVMPointerType(int8PtrLT, 0);
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  auto voidFuncPtrLT = LLVMPointerType(LLVMFunctionType(voidLT, nullptr, 0, false), 0);

  fileDescriptorPtrGlobalLE =
      LLVMAddGlobal(globalState->mod, int8PtrLT, "__vale_determinism_file");
  LLVMSetLinkage(fileDescriptorPtrGlobalLE, LLVMExternalLinkage);
  LLVMSetInitializer(fileDescriptorPtrGlobalLE, LLVMConstNull(int8PtrLT));


  std::vector<LLVMTypeRef> hasherParamsLT = {makeEmptyStructType(globalState), int256LT};
  auto hasherFuncLT = LLVMFunctionType(int64LT, hasherParamsLT.data(), hasherParamsLT.size(), false);

  std::vector<LLVMTypeRef> equatorParamsLT = {makeEmptyStructType(globalState), int256LT, int256LT};
  auto equatorFuncLT = LLVMFunctionType(int1LT, equatorParamsLT.data(), equatorParamsLT.size(), false);

  recordedRefToReplayedRefMapLT =
      std::make_unique<LlvmSimpleHashMap>(
          LlvmSimpleHashMap::create(
              globalState,
              recordedRefToReplayedRefMapTypeName,
              int256LT,
              int256LT,
              makeEmptyStructType(globalState),
              makeEmptyStructType(globalState),
              globalState->externs->int256HasherCallLF,
              globalState->externs->int256EquatorCallLF));
  recordedRefToReplayedRefMapGlobalLE =
      LLVMAddGlobal(
          globalState->mod,
          recordedRefToReplayedRefMapLT->getMapType(),
          recordedRefToReplayedRefMapInstanceName.c_str());
  LLVMSetLinkage(recordedRefToReplayedRefMapGlobalLE, LLVMExternalLinkage);
  std::vector<LLVMValueRef> refMapMembers = {
      constI64LE(globalState, 0),
      constI64LE(globalState, 0),
      LLVMConstNull(int8PtrLT),
      LLVMConstNull(LLVMPointerType(recordedRefToReplayedRefMapLT->getNodeType(), 0)),
      makeEmptyStruct(globalState),
      makeEmptyStruct(globalState)
  };
  LLVMSetInitializer(
      recordedRefToReplayedRefMapGlobalLE,
      LLVMConstNamedStruct(
          recordedRefToReplayedRefMapLT->getMapType(),
          refMapMembers.data(),
          refMapMembers.size()));


  functionsMapLT =
      std::make_unique<LlvmSimpleHashMap>(
          LlvmSimpleHashMap::create(
              globalState,
              functionsMapTypeName,
              LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
              LLVMPointerType(makeReplayerFuncLT(globalState), 0),
              makeEmptyStructType(globalState),
              makeEmptyStructType(globalState),
              globalState->externs->strHasherCallLF,
              globalState->externs->strEquatorCallLF));
  exportNameToReplayerFunctionMapGlobalLE =
      LLVMAddGlobal(globalState->mod, functionsMapLT->getMapType(), functionsMapInstanceName.c_str());
  LLVMSetLinkage(exportNameToReplayerFunctionMapGlobalLE, LLVMExternalLinkage);

  maybeStartDeterministicModeLF =
      addFunction(globalState->mod, maybeStartDeterministicModeFuncName, int64LT, {int64LT, int8PtrPtrLT});
  writeCallBeginToFileLF =
      addFunction(globalState->mod, writeCallBeginToFileFuncName, voidLT, {int64LT, int8PtrLT});
  writeRefToFileLF =
      addFunction(globalState->mod, writeRefToFileFuncName, voidLT, {int256LT});
//  writeValueToFileLF =
//      addFunction(globalState->mod, writeValueToFileFuncName, voidLT, {int64LT, int8PtrLT});
  recordCallEndLF =
      addFunction(globalState->mod, recordCallEndFuncName, voidLT, {int64LT, int8PtrLT});
  matchCallFromRecordingFileLF =
      addFunction(globalState->mod, matchCallFromRecordingFileFuncName, voidLT, {int64LT, int8PtrLT});
  mapRefFromRecordingFileLF =
      addFunction(globalState->mod, mapRefFromRecordingFileFuncName, int256LT, {int64LT, voidFuncPtrLT});
//  readValueFromFileLF =
//      addFunction(globalState->mod, readValueFromFileFuncName, voidLT, {int64LT, int8PtrLT});
//  getNextExportCallStringLF =
//      addFunction(globalState->mod, getNextExportCallStringFuncName, int8PtrLT, {});
  getMaybeReplayerFuncForNextExportNameLF =
      addFunction(globalState->mod, getMaybeReplayerFuncForNextExportNameFuncName, voidFuncPtrLT, {});
  startRecordingLF =
      addFunction(globalState->mod, startRecordingFuncName, voidLT, {int8PtrLT});
  startReplayingLF =
      addFunction(globalState->mod, startReplayingFuncName, voidLT, {int8PtrLT});
}

void Determinism::registerFunction(Prototype* prototype) {
  assert(!finalizedFunctions);
  functionsMap.add(prototype, std::make_tuple());
}

void Determinism::finalizeFunctionsMap() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int1LT = LLVMInt1TypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int8PtrPtrLT = LLVMPointerType(int8PtrLT, 0);
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);

//
//  std::vector<LLVMTypeRef> nodeTypesLT = { int8PtrLT, LLVMPointerType(replayerFuncPtrLT, 0) };
//  auto nodeLT = LLVMStructTypeInContext(globalState->context, nodeTypesLT.data(), nodeTypesLT.size(), false);
//  auto nodeArrayLT = LLVMArrayType(nodeLT, functionsMap.capacity);

//  CppSimpleHashMap<Prototype*, std::tuple<>, AddressHasher<Prototype*>, AddressEquator<Prototype*>>
  assert(!finalizedFunctions);
  finalizedFunctions = true;

  functionsMapLT->setInitializerForGlobalConstSimpleHashMap<Prototype*, std::tuple<>, PrototypeNameSimpleStringHasher, PrototypeNameSimpleStringEquator>(
      functionsMap,
      [this](Prototype* const& prototype, const std::tuple<>& value) -> std::tuple<LLVMValueRef, LLVMValueRef> {

        // TODO: Use exported names instead of regular function names, see URFNIEN.
        auto strLE = globalState->getOrMakeStringConstant(prototype->name->name);
        auto strLenLE = constI64LE(globalState, prototype->name->name.length());

        auto replayerFuncLE = makeFuncToReplayExportCall(prototype);
        return std::make_tuple(strLE, replayerFuncLE);
      },
      exportNameToReplayerFunctionMapGlobalLE,
      replayerMapName,
      makeEmptyStruct(globalState),
      makeEmptyStruct(globalState));

  makeFuncToMaybeStartDeterministicMode();
  makeFuncToWriteCallBeginToFile();
//  makeFuncToGetNextExportCallString();
  makeFuncToWriteRefToFile();
//  makeFuncToWriteValueToFile();
  makeFuncToRecordCallEnd();
  makeFuncToMatchCallFromRecordingFile();
//  makeFuncToReadValueFromFile();
  makeFuncToMapRefFromRecordingFile();
  makeFuncToStartReplaying();
  makeFuncToStartRecording();
  makeFuncToGetReplayerFuncForExportName();
}

void Determinism::makeFuncToWriteCallBeginToFile() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int8PtrPtrLT = LLVMPointerType(int8PtrLT, 0);
  defineFunctionBody(
      globalState->context,
      writeCallBeginToFileLF,
      voidLT,
      writeCallBeginToFileFuncName,
      [this](FunctionState* functionState, LLVMBuilderRef builder){
        auto nameLenLE = LLVMGetParam(functionState->containingFuncL, 0);
        auto nameI8PtrLE = LLVMGetParam(functionState->containingFuncL, 1);
        writeStringToFile(functionState, builder, nameLenLE, nameI8PtrLE);
        LLVMBuildRetVoid(builder);
      });
}

void Determinism::makeFuncToWriteRefToFile() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int8PtrPtrLT = LLVMPointerType(int8PtrLT, 0);
  defineFunctionBody(
      globalState->context,
      writeRefToFileLF,
      voidLT,
      writeRefToFileFuncName,
      [this, int256LT](FunctionState* functionState, LLVMBuilderRef builder){
        auto refI256LE = LLVMGetParam(functionState->containingFuncL, 0);
        assert(LLVMTypeOf(refI256LE) == int256LT);
        writeI256ToFile(functionState, builder, refI256LE);
        LLVMBuildRetVoid(builder);
      });
}

void Determinism::makeFuncToRecordCallEnd() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  defineFunctionBody(
      globalState->context, recordCallEndLF, voidLT, recordCallEndFuncName,
      [this](FunctionState *functionState, LLVMBuilderRef builder) {
        writeI64ToFile(functionState, builder, constI64LE(globalState, 0));
        LLVMBuildRetVoid(builder);
      });
}

void Determinism::makeFuncToMatchCallFromRecordingFile() {
  auto int1LT = LLVMInt1TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  defineFunctionBody(
      globalState->context, matchCallFromRecordingFileLF, voidLT, matchCallFromRecordingFileFuncName,
      [this, int1LT, int8LT](FunctionState *functionState, LLVMBuilderRef builder) {
        auto replayingCalledFuncNameLenLE = LLVMGetParam(functionState->containingFuncL, 0);
        auto replayingCalledFuncNamePtrLE = LLVMGetParam(functionState->containingFuncL, 1);

        auto bufferPtrLE = LLVMBuildArrayAlloca(builder, int8LT, constI64LE(globalState, 1024), "");
        auto recordedCalledFuncNamePtrLE = ptrToVoidPtrLE(globalState, builder, bufferPtrLE);
        auto recordedCalledFuncNameLenLE = readI64FromFile(functionState, builder);
        auto recordedCalledFuncNameWithPaddingLenLE = calcPaddedStrLen(globalState, builder, recordedCalledFuncNameLenLE);
        readLimitedStringFromFile(functionState, builder, recordedCalledFuncNameWithPaddingLenLE, recordedCalledFuncNamePtrLE);

        auto lengthsDifferentLE = LLVMBuildICmp(builder, LLVMIntNE, recordedCalledFuncNameLenLE, replayingCalledFuncNameLenLE, "lengthsDifferent");
        buildIfNever(
            globalState, functionState->containingFuncL, builder, lengthsDifferentLE,
            [this, recordedCalledFuncNamePtrLE, replayingCalledFuncNamePtrLE](LLVMBuilderRef builder){
              buildPrint(globalState, builder, "Recording file expected a call to ");
              buildPrint(globalState, builder, recordedCalledFuncNamePtrLE);
              buildPrint(globalState, builder, " but this execution is calling ");
              buildPrint(globalState, builder, replayingCalledFuncNamePtrLE);
              buildPrint(globalState, builder, ", aborting!\n");
              buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
            });
        auto stringsDifferentIntLE =
            buildSimpleCall(
                builder, globalState->externs->strncmp,
                {recordedCalledFuncNamePtrLE, replayingCalledFuncNamePtrLE, recordedCalledFuncNameLenLE});
        auto stringsDifferentLE = LLVMBuildTrunc(builder, stringsDifferentIntLE, int1LT, "stringsDifferent");
        buildIfNever(
            globalState, functionState->containingFuncL, builder, stringsDifferentLE,
            [this, recordedCalledFuncNamePtrLE, replayingCalledFuncNamePtrLE](LLVMBuilderRef builder){
              buildPrint(globalState, builder, "Recording file expected a call to ");
              buildPrint(globalState, builder, recordedCalledFuncNamePtrLE);
              buildPrint(globalState, builder, " but this execution is calling ");
              buildPrint(globalState, builder, replayingCalledFuncNamePtrLE);
              buildPrint(globalState, builder, ", aborting!\n");
              buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
            });
        LLVMBuildRetVoid(builder);
      });
  // take in a name argument
  // compare it to the file. i think thats it.
}

//void Determinism::makeFuncToReadValueFromFile() {
//  implement
//  // make a function that will recursively read from a slab.
//  // we'll need to more or less reverse the linear writing thing.
//
//  auto voidLT = LLVMVoidTypeInContext(globalState->context);
//  defineFunctionBody(
//      globalState, startRecordingLF, a raw gen ref see URFPL, startRecordingFuncName,
//      [this](FunctionState* functionState, LLVMBuilderRef builder) {
//
//      });
//}

void Determinism::makeFuncToMapRefFromRecordingFile() {
  // this happens when we get an export called.
  // it also happens when an extern returns a ref.
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  defineFunctionBody(
      globalState->context, mapRefFromRecordingFileLF, int256LT, mapRefFromRecordingFileFuncName,
      [this, int256LT](FunctionState* functionState, LLVMBuilderRef builder) {
        auto fatRefLE = readI256FromFile(functionState, builder);
        auto indexLE =
            recordedRefToReplayedRefMapLT->buildFindIndexOf(
                builder, recordedRefToReplayedRefMapGlobalLE, fatRefLE);
        auto keyFoundLE =
            LLVMBuildICmp(builder, LLVMIntSGT, indexLE, constI64LE(globalState, 0), "keyFound");
        auto resultLE =
            buildIfElse(
                globalState, functionState, builder, int256LT, keyFoundLE,
                [this, indexLE](LLVMBuilderRef builder) {
                  return recordedRefToReplayedRefMapLT->buildGetValueAtIndex(
                      builder, recordedRefToReplayedRefMapGlobalLE, indexLE);
                },
                [this](LLVMBuilderRef builder) {
                  return constI256LEFromI64(globalState, 0);
                });
        LLVMBuildRet(builder, resultLE);
      });
}

void Determinism::makeFuncToStartReplaying() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  defineFunctionBody(
      globalState->context, startReplayingLF, voidLT, startReplayingFuncName,
      [this](FunctionState* functionState, LLVMBuilderRef builder){
        auto recordingFilenameLE = LLVMGetParam(functionState->containingFuncL, 0);
        auto fileLE = openFile(functionState, builder, recordingFilenameLE, FileOpenMode::READ);
        LLVMBuildStore(builder, fileLE, fileDescriptorPtrGlobalLE);
        writeI64ToFile(functionState, builder, constI64LE(globalState, RECORDING_FILE_CONSTANT));
        LLVMBuildRetVoid(builder);
      });
}

void Determinism::makeFuncToStartRecording() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  defineFunctionBody(
      globalState->context, startRecordingLF, voidLT, startRecordingFuncName,
      [this](FunctionState* functionState, LLVMBuilderRef builder){
        auto recordingFilenameLE = LLVMGetParam(functionState->containingFuncL, 0);
        auto fileLE = openFile(functionState, builder, recordingFilenameLE, FileOpenMode::WRITE);
        LLVMBuildStore(builder, fileLE, fileDescriptorPtrGlobalLE);
        writeI64ToFile(functionState, builder, constI64LE(globalState, RECORDING_FILE_CONSTANT));
        LLVMBuildRetVoid(builder);
      });
}

void Determinism::writeBytesToFile(
    FunctionState* functionState, LLVMBuilderRef builder, LLVMValueRef sizeLE, LLVMValueRef i8PtrLE) {
  auto resultLE =
      buildSimpleCall(
          builder, globalState->externs->fwrite,
          {
              i8PtrLE,
              sizeLE,
              constI64LE(globalState, 1),
              LLVMBuildLoad(builder, fileDescriptorPtrGlobalLE, ""),
          });
  buildIfNever(
      globalState, functionState->containingFuncL, builder,
      LLVMBuildICmp(builder, LLVMIntULT, resultLE, constI64LE(globalState, 1), ""),
      [this](LLVMBuilderRef builder){
        buildPrint(globalState, builder, "Couldn't write to recording file.");
        buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
      });
}

LLVMValueRef Determinism::openFile(FunctionState* functionState, LLVMBuilderRef builder, LLVMValueRef pathI8PtrLE, FileOpenMode mode) {
  LLVMValueRef modeStrLE = nullptr;
  switch (mode) {
    case FileOpenMode::READ:
      modeStrLE = globalState->getOrMakeStringConstant("r");
      break;
    case FileOpenMode::WRITE:
      modeStrLE = globalState->getOrMakeStringConstant("w");
      break;
    default:
      assert(false);
  }
  auto fileLE = buildSimpleCall(builder, globalState->externs->fopen, {pathI8PtrLE, modeStrLE});
  auto fileAsI64LE = ptrToIntLE(globalState, builder, fileLE);
  buildIfNever(
      globalState, functionState->containingFuncL, builder,
      LLVMBuildICmp(builder, LLVMIntNE, fileAsI64LE, constI64LE(globalState, 0), ""),
      [this](LLVMBuilderRef builder){
        buildPrint(globalState, builder, "Couldn't open recording file.");
        buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
      });
  return fileLE;
}

void Determinism::writeI64ToFile(
    FunctionState* functionState, LLVMBuilderRef builder, LLVMValueRef i64LE) {
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  assert(LLVMTypeOf(i64LE) == int64LT);
  auto i64PtrLE = makeBackendLocal(functionState, builder, int64LT, "", i64LE);
  writeBytesToFile(
      functionState,
      builder,
      constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, int64LT)),
      ptrToVoidPtrLE(globalState, builder, i64PtrLE));
}

void Determinism::writeI256ToFile(
    FunctionState* functionState, LLVMBuilderRef builder, LLVMValueRef i256LE) {
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  assert(LLVMTypeOf(i256LE) == int256LT);
  auto i256PtrLE = makeBackendLocal(functionState, builder, int256LT, "", i256LE);
  writeBytesToFile(
      functionState,
      builder,
      constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, int256LT)),
      ptrToVoidPtrLE(globalState, builder, i256PtrLE));
}

LLVMValueRef Determinism::readI64FromFile(
    FunctionState* functionState, LLVMBuilderRef builder) {
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int64Size = LLVMABISizeOfType(globalState->dataLayout, int64LT);

  auto i64PtrLE = makeBackendLocal(functionState, builder, int64LT, "", constI64LE(globalState, 0));

  auto resultLE =
      buildSimpleCall(
          builder, globalState->externs->fread,
          {
              ptrToVoidPtrLE(globalState, builder, i64PtrLE),
              constI64LE(globalState, int64Size),
              constI64LE(globalState, 1),
              LLVMBuildLoad(builder, fileDescriptorPtrGlobalLE, "")
          });
  buildIf(
      globalState, functionState->containingFuncL, builder,
      LLVMBuildICmp(builder, LLVMIntULT, resultLE, constI64LE(globalState, 1), ""),
      [this](LLVMBuilderRef builder){
        buildPrint(globalState, builder, "Couldn't read from recording file.");
        buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
      });

  return LLVMBuildLoad(builder, i64PtrLE, "int64FromFile");
}

LLVMValueRef Determinism::readI256FromFile(
    FunctionState* functionState, LLVMBuilderRef builder) {
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  auto int256Size = LLVMABISizeOfType(globalState->dataLayout, int256LT);

  auto i256PtrLE = makeBackendLocal(functionState, builder, int256LT, "", constI256LEFromI64(globalState, 0));

  auto resultLE =
      buildSimpleCall(
          builder, globalState->externs->fread,
          {
              ptrToVoidPtrLE(globalState, builder, i256PtrLE),
              constI64LE(globalState, int256Size),
              constI64LE(globalState, 1),
              LLVMBuildLoad(builder, fileDescriptorPtrGlobalLE, "")
          });
  buildIf(
      globalState, functionState->containingFuncL, builder,
      LLVMBuildICmp(builder, LLVMIntULT, resultLE, constI64LE(globalState, 1), ""),
      [this](LLVMBuilderRef builder){
        buildPrint(globalState, builder, "Couldn't read from recording file.");
        buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
      });

  return LLVMBuildLoad(builder, i256PtrLE, "int256FromFile");
}

LLVMValueRef calcNumTrailingZeroes(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthNotIncludingNullTerminatorLE) {
  auto lengthIncludingNullTerminatorAndPaddingLE =
      calcPaddedStrLen(globalState, builder, lengthNotIncludingNullTerminatorLE);
  auto numZeroesAtEndLE =
      LLVMBuildSub(
          builder,
          lengthIncludingNullTerminatorAndPaddingLE,
          lengthNotIncludingNullTerminatorLE,
          "");
  return numZeroesAtEndLE;
}

void Determinism::readLimitedStringFromFile(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef maxSizeLE,
    LLVMValueRef bufferPtrLE) {
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int64Size = LLVMABISizeOfType(globalState->dataLayout, int64LT);

  auto resultLE =
      buildSimpleCall(
          builder, globalState->externs->fread,
          {
              bufferPtrLE,
              maxSizeLE,
              constI64LE(globalState, 1),
              LLVMBuildLoad(builder, fileDescriptorPtrGlobalLE, "")
          });
  buildIf(
      globalState, functionState->containingFuncL, builder,
      LLVMBuildICmp(builder, LLVMIntULT, resultLE, constI64LE(globalState, 1), ""),
      [this](LLVMBuilderRef builder){
        buildPrint(globalState, builder, "Couldn't read from recording file.");
        buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
      });
}

//LLVMValueRef Determinism::readStringFromFile(
//    FunctionState* functionState, LLVMBuilderRef builder) {
//  auto int64LT = LLVMInt64TypeInContext(globalState->context);
//  auto int64Size = LLVMABISizeOfType(globalState->dataLayout, int64LT);
//
//  auto i64PtrLE = makeBackendLocal(functionState, builder, int64LT, "", constI64LE(globalState, 0));
//
//  auto strLenLE = readI64FromFile(functionState, builder);
//  auto numZeroesLE = calcNumTrailingZeroes(globalState, builder, strLenLE);
//
//
//
//  auto resultLE =
//      buildSimpleCall(
//          builder, globalState->externs->fread,
//          {
//              ptrToVoidPtrLE(globalState, builder, i64PtrLE),
//              constI64LE(globalState, int64Size),
//              constI64LE(globalState, 1),
//              LLVMBuildLoad(builder, fileDescriptorPtrGlobalLE, "")
//          });
//  buildIf(
//      globalState, functionState->containingFuncL, builder,
//      LLVMBuildICmp(builder, LLVMIntULT, resultLE, constI64LE(globalState, 1), ""),
//      [this](LLVMBuilderRef builder){
//        buildPrint(globalState, builder, "Couldn't read from recording file.");
//        buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
//      });
//
//  return i64PtrLE;
//}

void Determinism::writeStringToFile(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthNotIncludingNullTerminatorLE,
    LLVMValueRef strLE) {
  auto int64LT = LLVMInt64TypeInContext(globalState->context);

  writeI64ToFile(functionState, builder, lengthNotIncludingNullTerminatorLE);

  writeBytesToFile(functionState, builder, lengthNotIncludingNullTerminatorLE, strLE);

  auto numZeroesAtEndLE = calcNumTrailingZeroes(globalState, builder, lengthNotIncludingNullTerminatorLE);

  auto zeroI64PtrLE = makeBackendLocal(functionState, builder, int64LT, "", constI64LE(globalState, 0));
  writeBytesToFile(functionState, builder, numZeroesAtEndLE, ptrToVoidPtrLE(globalState, builder, zeroI64PtrLE));
}

void Determinism::makeFuncToGetReplayerFuncForExportName() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int64Size = LLVMABISizeOfType(globalState->dataLayout, int64LT);
  auto voidFuncPtrLT = LLVMPointerType(LLVMFunctionType(voidLT, nullptr, 0, false), 0);
  auto replayerFuncLT = makeReplayerFuncLT(globalState);

  defineFunctionBody(
      globalState->context,
      getMaybeReplayerFuncForNextExportNameLF,
      voidFuncPtrLT,
      getMaybeReplayerFuncForNextExportNameFuncName,
      [this, int8LT, replayerFuncLT](FunctionState* functionState, LLVMBuilderRef builder){
        auto bufferPtrLE = LLVMBuildArrayAlloca(builder, int8LT, constI64LE(globalState, 1024), "");
        auto bufferI8PtrLE = ptrToVoidPtrLE(globalState, builder, bufferPtrLE);
        auto strLenLE = readI64FromFile(functionState, builder);
        auto strWithPaddingLenLE = calcPaddedStrLen(globalState, builder, strLenLE);
        readLimitedStringFromFile(functionState, builder, strWithPaddingLenLE, bufferI8PtrLE);

        auto foundIndexLE =
            functionsMapLT->buildFindIndexOf(builder, exportNameToReplayerFunctionMapGlobalLE, bufferI8PtrLE);
        auto notFoundLE = LLVMBuildICmp(builder, LLVMIntSLT, foundIndexLE, constI64LE(globalState, 0), "");
        auto resultLE =
          buildIfElse(
              globalState, functionState, builder, LLVMPointerType(replayerFuncLT, 0), notFoundLE,
              [this, replayerFuncLT](LLVMBuilderRef builder) -> LLVMValueRef {
  //              buildPrint(globalState, builder, "Error: Replay contained a call to ");
  //              buildPrint(globalState, builder, bufferI8PtrLE);
  //              buildPrint(globalState, builder, " at this point, but that function doesn't exist anymore.\n");
  //              buildSimpleCall(builder, globalState->externs->exit, { constI64LE(globalState, 1) });
                return LLVMConstNull(LLVMPointerType(replayerFuncLT, 0));
              },
              [this, foundIndexLE](LLVMBuilderRef builder) -> LLVMValueRef {
                return functionsMapLT->buildGetValueAtIndex(
                    builder, exportNameToReplayerFunctionMapGlobalLE, foundIndexLE);
              });
        LLVMBuildRet(builder, resultLE);
      });
}

void Determinism::makeFuncToMaybeStartDeterministicMode() {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int8PtrPtrLT = LLVMPointerType(int8PtrLT, 0);

  defineFunctionBody(
      globalState->context,
      maybeStartDeterministicModeLF,
      voidLT,
      maybeStartDeterministicModeFuncName,
      [this, int8PtrLT, int64LT](FunctionState* functionState, LLVMBuilderRef builder){
        auto mainArgsCountLE = LLVMGetParam(functionState->containingFuncL, 0);
        auto mainArgsLE = LLVMGetParam(functionState->containingFuncL, 1);
        auto numConsumedArgsLE =
            processFlag(
                globalState, functionState, builder, VALE_RECORD_FLAG, mainArgsCountLE, mainArgsLE,
                [this](LLVMBuilderRef builder, LLVMValueRef recordingFilenameStrLE){
                  buildStartRecording(builder, recordingFilenameStrLE);
                });

        // We're returning the number of args we've consumed.
        auto resultLE =
            buildIfElse(
                globalState, functionState, builder, int64LT,
                LLVMBuildICmp(builder, LLVMIntEQ, numConsumedArgsLE, constI64LE(globalState, 0), ""),
                [this, functionState, mainArgsCountLE, mainArgsLE](LLVMBuilderRef builder) {
                  return processFlag(
                      globalState, functionState, builder, VALE_REPLAY_FLAG, mainArgsCountLE, mainArgsLE,
                      [this](LLVMBuilderRef builder, LLVMValueRef replayingFilenameStrLE) {
                        buildStartReplaying(builder, replayingFilenameStrLE);
                      });
                },
                [this](LLVMBuilderRef builder) {
                  return constI64LE(globalState, 0);
                });
        LLVMBuildRet(builder, resultLE);
      });

//        set up that global to be extern linked
//        set up startDeterministicMode
//        write the c code that opens the file
//
//        auto workedLE =
//            LLVMBuildICmp(
//                thenBuilder,
//                LLVMIntNE,
//                LLVMBuildLoad(thenBuilder, globalState->recordingModePtrLE.value(), "isRecording"),
//                constI64LE(globalState, 0),
//                "deterministicStarted");
//        buildAssert(globalState, entryFunctionL, thenBuilder, workedLE, "Deterministic mode failed to start!");
}

void Determinism::buildStartReplaying(LLVMBuilderRef builder, LLVMValueRef recordingFilename) {
  assert(startReplayingLF);
  buildSimpleCall(builder, startReplayingLF, {recordingFilename});
}

void Determinism::buildStartRecording(LLVMBuilderRef builder, LLVMValueRef recordingFilename) {
  assert(startRecordingLF);
  buildSimpleCall(builder, startRecordingLF, {recordingFilename});
}

void Determinism::buildWriteCallBeginToFile(LLVMBuilderRef builder, Prototype* prototype) {
  assert(writeCallBeginToFileLF);

  // TODO: Use exported names instead of regular function names, see URFNIEN.
  auto strLE = globalState->getOrMakeStringConstant(prototype->name->name);
  auto strLenLE = constI64LE(globalState, prototype->name->name.length());

  buildSimpleCall(builder, writeCallBeginToFileLF, {strLenLE, strLE});
}

//LLVMValueRef Determinism::buildGetNextExportCallString(LLVMBuilderRef builder) {
//  assert(getNextExportCallStringLF);
//  buildSimpleCall(builder, getNextExportCallStringLF, {});
//}

void Determinism::buildWriteValueToFile(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(maybeStartDeterministicModeLF);

  // we'll need to memcpy from the temporary linear region into the file.
  // for sending into C thisll be easy.
  // however, when receiving into C, we might need to invoke it manually.
  // also, we may need to have it subtract the start from the pointer, so
  // itll be offset relative to the linear region begin.

  auto int1LT = LLVMInt1TypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto floatLT = LLVMDoubleTypeInContext(globalState->context);

  assert(sourceRefMT->ownership == Ownership::SHARE); // not implemented for owns
  auto hostRefMT = globalState->linearRegion->linearizeReference(sourceRefMT);

  auto sourceRefLE =
      globalState->getRegion(sourceRefMT)
          ->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceRef);

  if (dynamic_cast<Int*>(sourceRefMT->kind)) {
    writeI64ToFile(functionState, builder, sourceRefLE);
  } else if (dynamic_cast<Bool*>(sourceRefMT->kind)) {
    auto boolAsI64LE = LLVMBuildZExt(builder, sourceRefLE, int64LT, "boolAsI64");
    writeI64ToFile(functionState, builder, boolAsI64LE);
  } else if (dynamic_cast<Float*>(sourceRefMT->kind)) {
    auto floatAsI64LE = LLVMBuildBitCast(builder, sourceRefLE, floatLT, "floatFromRecording");
    writeI64ToFile(functionState, builder, floatAsI64LE);
  } else if (dynamic_cast<StructKind*>(sourceRefMT->kind)) {

    auto valeRegionInstanceRef =
        // At some point, look up the actual region instance, perhaps from the FunctionState?
        globalState->getRegion(sourceRefMT)->createRegionInstanceLocal(functionState, builder);
    auto useOffsetsLE = constI1LE(globalState, 1);
    auto fileOffsetLE = LLVMBuildLoad(builder, fileOffsetPtrGlobalLE, "fileOffset");
    auto hostRegionInstanceRef =
        globalState->linearRegion->createRegionInstanceLocal(
            functionState, builder, useOffsetsLE, fileOffsetLE);

    LLVMValueRef hostRefLE = nullptr;
    LLVMValueRef sizeLE = nullptr;
    std::tie(hostRefLE, sizeLE) =
        sendValeObjectIntoHost(
            globalState, functionState, builder, valeRegionInstanceRef, hostRegionInstanceRef,
            sourceRefMT, hostRefMT, sourceRef);

    writeI64ToFile(functionState, builder, sizeLE);
    writeBytesToFile(functionState, builder, sizeLE, hostRefLE);

    buildSimpleCall(builder, globalState->externs->free, {hostRefLE});
  } else {
    assert(false);
  }
}


LLVMValueRef Determinism::buildMaybeStartDeterministicMode(
    LLVMBuilderRef builder, LLVMValueRef mainArgsLE, LLVMValueRef argcLE) {
  assert(maybeStartDeterministicModeLF);
  return buildSimpleCall(builder, maybeStartDeterministicModeLF, {mainArgsLE, argcLE});
}


void Determinism::buildWriteRefToFile(LLVMBuilderRef builder, LLVMValueRef refI256LE) {
  assert(writeRefToFileLF);
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  assert(LLVMTypeOf(refI256LE) == int256LT);
  buildSimpleCall(builder, writeRefToFileLF, {refI256LE});
}

void Determinism::buildRecordCallEnd(LLVMBuilderRef builder, Prototype* prototype) {
  assert(recordCallEndLF);

  // TODO: Use exported names instead of regular function names, see URFNIEN.
  auto strLE = globalState->getOrMakeStringConstant(prototype->name->name);
  auto strLenLE = constI64LE(globalState, prototype->name->name.length());

  buildSimpleCall(builder, recordCallEndLF, {strLenLE, strLE});
}

void Determinism::buildMatchCallFromRecordingFile(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Prototype* prototype) {
  assert(matchCallFromRecordingFileLF);
  // TODO: Use exported names instead of regular function names, see URFNIEN.
  auto strLE = globalState->getOrMakeStringConstant(prototype->name->name);
  auto strLenLE = constI64LE(globalState, prototype->name->name.length());
  buildSimpleCall(builder, matchCallFromRecordingFileLF, {strLenLE, strLE});
}

Ref Determinism::buildReadValueFromFile(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* targetRefMT) {
  auto int1LT = LLVMInt1TypeInContext(globalState->context);
  auto floatLT = LLVMDoubleTypeInContext(globalState->context);

  assert(targetRefMT->ownership == Ownership::SHARE); // not implemented for owns
  auto hostRefMT = globalState->linearRegion->linearizeReference(targetRefMT);

  if (dynamic_cast<Int*>(targetRefMT->kind)) {
    auto intLE = readI64FromFile(functionState, builder);
    return wrap(globalState->getRegion(targetRefMT), targetRefMT, intLE);
  } else if (dynamic_cast<Bool*>(targetRefMT->kind)) {
    auto boolLE = LLVMBuildTrunc(builder, readI64FromFile(functionState, builder), int1LT, "boolFromRecording");
    return wrap(globalState->getRegion(targetRefMT), targetRefMT, boolLE);
  } else if (dynamic_cast<Float*>(targetRefMT->kind)) {
    auto floatLE = LLVMBuildBitCast(builder, readI64FromFile(functionState, builder), floatLT, "floatFromRecording");
    return wrap(globalState->getRegion(targetRefMT), targetRefMT, floatLE);
  } else if (dynamic_cast<StructKind*>(targetRefMT->kind)) {
    auto valueSizeLE = readI64FromFile(functionState, builder);
    auto tempBufferPtrLE = buildSimpleCall(builder, globalState->externs->malloc, {valueSizeLE});

    auto freadResultLE =
        buildSimpleCall(
            builder, globalState->externs->fread,
            {
                tempBufferPtrLE,
                valueSizeLE,
                constI64LE(globalState, 1),
                LLVMBuildLoad(builder, fileDescriptorPtrGlobalLE, "")
            });
    buildIf(
        globalState, functionState->containingFuncL, builder,
        LLVMBuildICmp(builder, LLVMIntULT, freadResultLE, constI64LE(globalState, 1), ""),
        [this](LLVMBuilderRef builder){
          buildPrint(globalState, builder, "Couldn't read from recording file.");
          buildSimpleCall(builder, globalState->externs->exit, {constI64LE(globalState, 1)});
        });

    auto valeRegionInstanceRef =
        // At some point, look up the actual region instance, perhaps from the FunctionState?
        globalState->getRegion(targetRefMT)->createRegionInstanceLocal(functionState, builder);
    auto useOffsetsLE = constI1LE(globalState, 1);
    auto fileOffsetLE = LLVMBuildLoad(builder, fileOffsetPtrGlobalLE, "fileOffset");
    auto hostRegionInstanceRef =
        globalState->linearRegion->createRegionInstanceLocal(
            functionState, builder, useOffsetsLE, fileOffsetLE);

    auto valeRef =
        receiveHostObjectIntoVale(
            globalState, functionState, builder, hostRegionInstanceRef, valeRegionInstanceRef,
            hostRefMT, targetRefMT, tempBufferPtrLE);

    buildSimpleCall(builder, globalState->externs->free, {tempBufferPtrLE});

    return valeRef;
  } else {
    assert(false);
  }
}

Ref Determinism::buildMapRefFromRecordingFile(LLVMBuilderRef builder, Reference* refMT) {
  assert(mapRefFromRecordingFileLF);
  auto refLE = buildSimpleCall(builder, mapRefFromRecordingFileLF, {});
  return wrap(globalState->getRegion(refMT), refMT, refLE);
}

LLVMValueRef Determinism::buildGetMaybeReplayedFuncForNextExportCall(LLVMBuilderRef builder) {
  assert(getMaybeReplayerFuncForNextExportNameLF);
  return buildSimpleCall(builder, getMaybeReplayerFuncForNextExportNameLF, {});
}

Ref Determinism::i256ToRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    LLVMValueRef refLE) {
  auto refLT = globalState->getRegion(refMT)->translateType(refMT);
  assert(LLVMABISizeOfType(globalState->dataLayout, refLT) <= 32);
  assert(LLVMGetTypeKind(refLT) != LLVMPointerTypeKind);
  auto refFrom256LE = LLVMBuildBitCast(builder, refLE, refLT, "refFrom256");
  return wrap(globalState->getRegion(refMT), refMT, refFrom256LE);
}

LLVMValueRef Determinism::refToI256(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  auto int256LT = LLVMIntTypeInContext(globalState->context, 256);
  auto refLT = globalState->getRegion(refMT)->translateType(refMT);
  assert(LLVMABISizeOfType(globalState->dataLayout, refLT) <= 32);
  assert(LLVMGetTypeKind(refLT) != LLVMPointerTypeKind);
  auto refLE = globalState->getRegion(refMT)->checkValidReference(FL(), functionState, builder, refMT, ref);
  return LLVMBuildBitCast(builder, refLE, int256LT, "refFrom256");
}

LLVMValueRef Determinism::makeFuncToReplayExportCall(Prototype* prototype) {
  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int64LT = LLVMInt64TypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int64Size = LLVMABISizeOfType(globalState->dataLayout, int64LT);
  auto replayerFuncLT = makeReplayerFuncLT(globalState);

  // TODO: Use exported names instead of regular function names, see URFNIEN.
  auto replayerFuncName = replayerFuncPrefix + prototype->name->name;

  auto functionLF = addFunction(globalState->mod, replayerFuncName, voidLT, {});

  defineFunctionBody(
      globalState->context,
      functionLF,
      voidLT,
      replayerFuncName,
      [this, int8LT, replayerFuncLT](FunctionState* functionState, LLVMBuilderRef builder){
        buildPrint(globalState, builder, "Implement makeFuncToReplayExportCall");
        LLVMBuildRetVoid(builder);
      });

  return functionLF;
}