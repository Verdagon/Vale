#include <iostream>
#include "function/expressions/shared/controlblock.h"
#include "function/expressions/shared/string.h"

#include "function/expressions/shared/shared.h"
#include "regions/shared/heap.h"

LLVMValueRef translateConstantStr(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    ConstantStr* constantStr) {
  auto globalStringPtrLE = globalState->getOrMakeStringConstant(constantStr->value);
  auto stringRefLE =
      functionState->defaultRegion->constructString(
          globalState, functionState, builder, constI64LE(constantStr->value.length()));
  auto stringBytesPtrLE = functionState->defaultRegion->getStringBytesPtr(builder, stringRefLE);
  std::vector<LLVMValueRef> args =
      { stringBytesPtrLE, globalStringPtrLE, constI64LE(constantStr->value.length()) };
  LLVMBuildCall(builder, globalState->strncpy, args.data(), args.size(), "");
  return stringRefLE;
}
