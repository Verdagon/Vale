#include <iostream>
#include "function/expressions/shared/controlblock.h"
#include "function/expressions/shared/elements.h"

#include "translatetype.h"

#include "function/expressions/shared/members.h"
#include "function/expression.h"
#include "function/expressions/shared/shared.h"
#include "regions/shared/heap.h"

void fillUnknownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* generatorType,
    LLVMValueRef originalGeneratorLE,
    LLVMValueRef sizeLE,
    LLVMValueRef usaElementsPtrLE) {

  foreachArrayElement(
      functionState, builder, sizeLE, usaElementsPtrLE,
      [globalState, functionState, generatorType, usaElementsPtrLE, originalGeneratorLE](LLVMValueRef indexLE, LLVMBuilderRef bodyBuilder) {
        auto aliasedGeneratorLE = functionState->defaultRegion->alias(
            AFL("ConstructUSA generate iteration"),
            globalState, functionState, bodyBuilder, generatorType, generatorType, originalGeneratorLE);

        std::vector<LLVMValueRef> indices = { constI64LE(0), indexLE };
        auto elementPtrLE =
            LLVMBuildGEP(
                bodyBuilder, usaElementsPtrLE, indices.data(), indices.size(), "elementPtr");
        std::vector<LLVMValueRef> argExprsLE = { aliasedGeneratorLE, indexLE };
        auto elementLE = buildInterfaceCall(functionState->defaultRegion, bodyBuilder, argExprsLE, 0, 0);
        LLVMBuildStore(bodyBuilder, elementLE, elementPtrLE);
      });
}

LLVMValueRef constructKnownSizeArrayCountedStruct(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* generatorType,
    LLVMValueRef generatorLE,
    LLVMTypeRef usaWrapperPtrLT,
    LLVMTypeRef usaElementLT,
    LLVMValueRef sizeLE,
    const std::string& typeName) {
  auto usaWrapperPtrLE = functionState->defaultRegion->constructUnknownSizeArray(globalState, functionState, builder, usaWrapperPtrLT, usaElementLT, sizeLE, typeName);
  LLVMBuildStore(builder, sizeLE, LLVMBuildStructGEP(builder, usaWrapperPtrLE, 1, "lenPtr"));
  fillUnknownSizeArray(
      globalState,
      functionState,
      blockState,
      builder,
      generatorType,
      generatorLE,
      sizeLE,
      functionState->defaultRegion->getUnknownSizeArrayElementsPtr(builder, usaWrapperPtrLE));
  return usaWrapperPtrLE;
}

LLVMValueRef translateConstructUnknownSizeArray(
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

  auto unknownSizeArrayMT = dynamic_cast<UnknownSizeArrayT*>(constructUnknownSizeArray->arrayRefType->referend);

  auto usaWrapperPtrLT = translateType(globalState, functionState->defaultRegion, constructUnknownSizeArray->arrayRefType);
  auto usaElementLT = translateType(globalState, functionState->defaultRegion, unknownSizeArrayMT->rawArray->elementType);

  auto sizeLE = translateExpression(globalState, functionState, blockState, builder, sizeExpr);

  auto generatorLE = translateExpression(globalState, functionState, blockState, builder, generatorExpr);
  functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder,
      constructUnknownSizeArray->generatorType, generatorLE);

  // If we get here, arrayLT is a pointer to our counted struct.
  auto unknownSizeArrayCountedStructLT =
      functionState->defaultRegion->getUnknownSizeArrayRefType(
          globalState, constructUnknownSizeArray->arrayRefType, unknownSizeArrayMT);
  auto resultLE =
      constructKnownSizeArrayCountedStruct(
          globalState,
      functionState,
      blockState,
      builder,
          generatorType,
          generatorLE,
          unknownSizeArrayCountedStructLT,
          usaElementLT,
          sizeLE,
          unknownSizeArrayMT->name->name);
  functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder,
      constructUnknownSizeArray->arrayRefType, resultLE);

  functionState->defaultRegion->dealias(AFL("ConstructUSA"), globalState, functionState, blockState, builder, sizeType, sizeLE);
  functionState->defaultRegion->dealias(AFL("ConstructUSA"), globalState, functionState, blockState, builder, generatorType, generatorLE);

  return resultLE;
}
