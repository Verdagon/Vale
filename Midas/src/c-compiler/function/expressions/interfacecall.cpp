#include <iostream>
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/controlblock.h"

#include "translatetype.h"

#include "function/expression.h"

LLVMValueRef translateInterfaceCall(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    InterfaceCall* call) {
  auto argExprsLE =
      translateExpressions(globalState, functionState, blockState, builder, call->argExprs);

  auto argsLE = std::vector<LLVMValueRef>{};
  argsLE.reserve(call->argExprs.size());
  for (int i = 0; i < call->argExprs.size(); i++) {
    auto argLE = translateExpression(globalState, functionState, blockState, builder, call->argExprs[i]);
    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->functionType->params[i], argLE);
    argsLE.push_back(argLE);
  }

  auto resultLE =
      buildInterfaceCall(
          functionState->defaultRegion, builder, argExprsLE, call->virtualParamIndex, call->indexInEdge);
  functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->functionType->returnType, resultLE);

  if (call->functionType->returnType->referend == globalState->metalCache.never) {
    return LLVMBuildRet(builder, LLVMGetUndef(functionState->returnTypeL));
  } else {
    return resultLE;
  }
}
