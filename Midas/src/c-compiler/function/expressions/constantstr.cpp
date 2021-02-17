#include <iostream>
#include "region/common/controlblock.h"
#include "function/expressions/shared/string.h"

#include "function/expressions/shared/shared.h"
#include "region/common/heap.h"

Ref translateConstantStr(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    ConstantStr* constantStr) {
  auto strRef =
      buildConstantVStr(globalState, functionState, builder, constantStr->value);
  globalState->getRegion(globalState->metalCache->strRef)->alias(FL(), functionState, builder, globalState->metalCache->strRef, strRef);
  return strRef;
}
