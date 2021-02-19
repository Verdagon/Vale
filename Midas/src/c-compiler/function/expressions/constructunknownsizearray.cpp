#include <iostream>
#include "region/common/controlblock.h"
#include "function/expressions/shared/elements.h"

#include "translatetype.h"

#include "function/expressions/shared/members.h"
#include "function/expression.h"
#include "function/expressions/shared/shared.h"
#include "region/common/heap.h"

Ref translateConstructUnknownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    ConstructUnknownSizeArray* constructUnknownSizeArray) {

  auto generatorType = constructUnknownSizeArray->generatorType;
  auto generatorExpr = constructUnknownSizeArray->generatorExpr;
  auto sizeReferend = constructUnknownSizeArray->sizeReferend;
  auto sizeExpr = constructUnknownSizeArray->sizeExpr;
  auto sizeType = constructUnknownSizeArray->sizeType;
  auto elementType = constructUnknownSizeArray->elementType;

  auto unknownSizeArrayMT = dynamic_cast<UnknownSizeArrayT*>(constructUnknownSizeArray->arrayRefType->referend);

  auto usaWrapperPtrLT = globalState->getRegion(constructUnknownSizeArray->arrayRefType)->translateType(constructUnknownSizeArray->arrayRefType);
  auto usaElementLT = globalState->getRegion(elementType)->translateType(elementType);

  auto sizeLE = translateExpression(globalState, functionState, blockState, builder, sizeExpr);

  auto generatorLE = translateExpression(globalState, functionState, blockState, builder, generatorExpr);
  globalState->getRegion(constructUnknownSizeArray->generatorType)->checkValidReference(FL(), functionState, builder,
      constructUnknownSizeArray->generatorType, generatorLE);

  // If we get here, arrayLT is a pointer to our counted struct.
  auto usaRef =
      globalState->getRegion(constructUnknownSizeArray->arrayRefType)->constructUnknownSizeArrayCountedStruct(
          functionState,
          builder,
          constructUnknownSizeArray->arrayRefType,
          unknownSizeArrayMT,
          generatorType,
          constructUnknownSizeArray->generatorMethod,
          generatorLE,
          usaElementLT,
          sizeLE,
          unknownSizeArrayMT->name->name);
  globalState->getRegion(constructUnknownSizeArray->arrayRefType)->checkValidReference(FL(), functionState, builder,
      constructUnknownSizeArray->arrayRefType, usaRef);

  globalState->getRegion(sizeType)->dealias(AFL("ConstructUSA"), functionState, builder, sizeType, sizeLE);
  globalState->getRegion(generatorType)->dealias(AFL("ConstructUSA"), functionState, builder, generatorType, generatorLE);

  return usaRef;
}
