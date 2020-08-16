#include "utils/fileio.h"
#include "heap.h"
#include "function/expressions/shared/members.h"
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/controlblock.h"
#include "function/expressions/shared/string.h"

LLVMValueRef allocateStruct(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    LLVMTypeRef structL) {
  adjustCounter(builder, globalState->liveHeapObjCounter, 1);

  LLVMValueRef resultPtrLE = nullptr;
  if (structTypeM->location == Location::INLINE) {
    resultPtrLE = LLVMBuildAlloca(builder, structL, "newstruct");
  } else if (structTypeM->location == Location::YONDER) {
    size_t sizeBytes = LLVMABISizeOfType(globalState->dataLayout, structL);
    LLVMValueRef sizeLE = LLVMConstInt(LLVMInt64Type(), sizeBytes, false);

    auto newStructLE =
        LLVMBuildCall(builder, globalState->malloc, &sizeLE, 1, "");

    resultPtrLE =
        LLVMBuildBitCast(
            builder, newStructLE, LLVMPointerType(structL, 0), "newstruct");
  } else {
    assert(false);
    return nullptr;
  }

  if (globalState->opt->census) {
    LLVMValueRef resultAsVoidPtrLE =
        LLVMBuildBitCast(
            builder, resultPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
  }
  return resultPtrLE;
}

LLVMValueRef mallocUnknownSizeArray(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMTypeRef usaWrapperLT,
    LLVMTypeRef usaElementLT,
    LLVMValueRef lengthLE) {
  auto sizeBytesLE =
      LLVMBuildAdd(
          builder,
          constI64LE(LLVMABISizeOfType(globalState->dataLayout, usaWrapperLT)),
          LLVMBuildMul(
              builder,
              constI64LE(LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(usaElementLT, 1))),
              lengthLE,
              ""),
          "usaMallocSizeBytes");

  auto newWrapperPtrLE =
      LLVMBuildCall(builder, globalState->malloc, &sizeBytesLE, 1, "");

  adjustCounter(builder, globalState->liveHeapObjCounter, 1);

  if (globalState->opt->census) {
    LLVMValueRef resultAsVoidPtrLE =
        LLVMBuildBitCast(
            builder, newWrapperPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
  }

  return LLVMBuildBitCast(
      builder,
      newWrapperPtrLE,
      LLVMPointerType(usaWrapperLT, 0),
      "newstruct");
}


