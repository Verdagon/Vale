#include <iostream>
#include <function/expressions/shared/controlblock.h>
#include "function/expressions/shared/members.h"
#include "regions/shared/heap.h"

#include "translatetype.h"

#include "function/expression.h"
#include "function/expressions/shared/shared.h"

LLVMValueRef translateDestructure(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Destroy* destructureM) {
  auto structType = destructureM->structType;
  auto structExpr = destructureM->structExpr;

  auto structLE =
      translateExpression(
          globalState, functionState, blockState, builder, structExpr);
  checkValidReference(FL(), globalState, functionState, builder, structType, structLE);

  auto membersLE =
      functionState->defaultRegion->destructure(
          globalState, functionState, blockState, builder, structType, structLE);

  for (int i = 0; i < membersLE.size(); i++) {
    makeLocal(
        globalState,
        functionState,
        blockState,
        builder,
        destructureM->localIndices[i],
        membersLE[i]);
  }

  return makeConstExpr(builder, makeNever());
}
