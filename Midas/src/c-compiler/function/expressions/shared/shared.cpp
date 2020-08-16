#include "shared.h"

#include "translatetype.h"
#include "controlblock.h"
#include "branch.h"
#include "elements.h"

// A "Never" is something that should never be read.
// This is useful in a lot of situations, for example:
// - The return type of Panic()
// - The result of the Discard node
LLVMTypeRef makeNeverType() {
  // We arbitrarily use a zero-len array of i57 here because it's zero sized and
  // very unlikely to be used anywhere else.
  // We could use an empty struct instead, but this'll do.
  return LLVMArrayType(LLVMIntType(NEVER_INT_BITS), 0);
}
LLVMValueRef makeNever() {
  LLVMValueRef empty[1] = {};
  // We arbitrarily use a zero-len array of i57 here because it's zero sized and
  // very unlikely to be used anywhere else.
  // We could use an empty struct instead, but this'll do.
  return LLVMConstArray(LLVMIntType(NEVER_INT_BITS), empty, 0);
}

void makeLocal(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Local* local,
    LLVMValueRef valueToStore) {
  auto localAddr =
      LLVMBuildAlloca(
          builder,
          translateType(globalState, local->type),
          local->id->maybeName.c_str());
  blockState->addLocal(local->id, localAddr);
  LLVMBuildStore(builder, valueToStore, localAddr);
}

LLVMValueRef adjustCounter(
    LLVMBuilderRef builder,
    LLVMValueRef counterPtrLE,
    int adjustAmount) {
  auto prevValLE = LLVMBuildLoad(builder, counterPtrLE, "counterPrevVal");
  auto newValLE =
      LLVMBuildAdd(
          builder, prevValLE, LLVMConstInt(LLVMInt64Type(), adjustAmount, true), "counterNewVal");
  LLVMBuildStore(builder, newValLE, counterPtrLE);

  return newValLE;
}


LLVMValueRef getTablePtrFromInterfaceRef(
    LLVMBuilderRef builder,
    LLVMValueRef interfaceRefLE) {
  return LLVMBuildExtractValue(builder, interfaceRefLE, 1, "itablePtr");
}

LLVMValueRef isZeroLE(LLVMBuilderRef builder, LLVMValueRef intLE) {
  return LLVMBuildICmp(
      builder,
      LLVMIntEQ,
      intLE,
      LLVMConstInt(LLVMTypeOf(intLE), 0, false),
      "strongRcIsZero");
}

LLVMValueRef isNonZeroLE(LLVMBuilderRef builder, LLVMValueRef intLE) {
  return LLVMBuildICmp(
      builder,
      LLVMIntNE,
      intLE,
      LLVMConstInt(LLVMTypeOf(intLE), 0, false),
      "rcIsNonZero");
}

void buildPrint(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    const std::string& first) {
  auto s = globalState->getOrMakeStringConstant(first);
  LLVMBuildCall(builder, globalState->printCStr, &s, 1, "");
}

void buildPrint(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef exprLE) {
  if (LLVMTypeOf(exprLE) == LLVMInt64Type()) {
    LLVMBuildCall(builder, globalState->printInt, &exprLE, 1, "");
  } else if (LLVMTypeOf(exprLE) == LLVMPointerType(LLVMInt8Type(), 0)) {
    LLVMBuildCall(builder, globalState->printCStr, &exprLE, 1, "");
  } else {
    buildPrint(
        globalState,
        builder,
        LLVMBuildPointerCast(builder, exprLE, LLVMInt64Type(), ""));
  }
}

void buildPrint(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    int num) {
  buildPrint(globalState, builder, LLVMConstInt(LLVMInt64Type(), num, false));
}

// We'll assert if conditionLE is false.
void buildAssert(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef conditionLE,
    const std::string& failMessage) {
  buildIf(
      functionState, builder, isZeroLE(builder, conditionLE),
      [from, globalState, functionState, failMessage](LLVMBuilderRef thenBuilder) {
        buildPrint(globalState, thenBuilder, failMessage + " Exiting!\n");
        auto exitCodeIntLE = LLVMConstInt(LLVMInt8Type(), 255, false);
        LLVMBuildCall(thenBuilder, globalState->exit, &exitCodeIntLE, 1, "");
      });
}

LLVMValueRef buildInterfaceCall(
    LLVMBuilderRef builder,
    std::vector<LLVMValueRef> argExprsLE,
    int virtualParamIndex,
    int indexInEdge) {
  auto virtualArgLE = argExprsLE[virtualParamIndex];
  auto objPtrLE =
      LLVMBuildPointerCast(
          builder,
          functionState->defaultRegion->getInterfaceControlBlockPtr(builder, virtualArgLE),
          LLVMPointerType(LLVMVoidType(), 0),
          "objAsVoidPtr");
  auto itablePtrLE = getTablePtrFromInterfaceRef(builder, virtualArgLE);
  assert(LLVMGetTypeKind(LLVMTypeOf(itablePtrLE)) == LLVMPointerTypeKind);
  auto funcPtrPtrLE =
      LLVMBuildStructGEP(
          builder, itablePtrLE, indexInEdge, "methodPtrPtr");
  auto funcPtrLE = LLVMBuildLoad(builder, funcPtrPtrLE, "methodPtr");

  argExprsLE[virtualParamIndex] = objPtrLE;

  return LLVMBuildCall(
      builder, funcPtrLE, argExprsLE.data(), argExprsLE.size(), "");
}

LLVMValueRef makeConstExpr(LLVMBuilderRef builder, LLVMValueRef constExpr) {
  auto localAddr = LLVMBuildAlloca(builder, LLVMTypeOf(constExpr), "");
  LLVMBuildStore(builder,constExpr,localAddr);
  return LLVMBuildLoad(builder, localAddr, "");
}

LLVMValueRef makeConstIntExpr(LLVMBuilderRef builder, LLVMTypeRef type, int value) {
  return makeConstExpr(builder, LLVMConstInt(type, value, false));
}

void buildAssertCensusContains(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef refLE) {
  LLVMValueRef resultAsVoidPtrLE =
      LLVMBuildBitCast(
          builder, refLE, LLVMPointerType(LLVMVoidType(), 0), "");
  auto isRegisteredIntLE = LLVMBuildCall(builder, globalState->censusContains, &resultAsVoidPtrLE, 1, "");
  auto isRegisteredBoolLE = LLVMBuildTruncOrBitCast(builder,  isRegisteredIntLE, LLVMInt1Type(), "");
  buildAssert(checkerAFL, globalState, functionState, builder, isRegisteredBoolLE, "Object not registered with census!");
}

LLVMValueRef buildCall(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Prototype* prototype,
    std::vector<LLVMValueRef> argsLE) {
  auto funcIter = globalState->functions.find(prototype->name->name);
  assert(funcIter != globalState->functions.end());
  auto funcL = funcIter->second;

  auto resultLE = LLVMBuildCall(builder, funcL, argsLE.data(), argsLE.size(), "");
  checkValidReference(FL(), globalState, functionState, builder, prototype->returnType, resultLE);

  if (prototype->returnType->referend == globalState->metalCache.never) {
    return LLVMBuildRet(builder, LLVMGetUndef(functionState->returnTypeL));
  } else {
    return resultLE;
  }
}

LLVMValueRef upcast2(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructTypeM,
    StructReferend* sourceStructReferendM,
    LLVMValueRef sourceStructLE,

    Reference* targetInterfaceTypeM,
    InterfaceReferend* targetInterfaceReferendM) {
  assert(sourceStructTypeM->location != Location::INLINE);

  auto interfaceRefLT =
      globalState->getInterfaceRefStruct(
          targetInterfaceReferendM->fullName);

  auto interfaceRefLE = LLVMGetUndef(interfaceRefLT);
  interfaceRefLE =
      LLVMBuildInsertValue(
          builder,
          interfaceRefLE,
          getControlBlockPtr(builder, sourceStructLE, sourceStructTypeM),
          0,
          "interfaceRefWithOnlyObj");
  interfaceRefLE =
      LLVMBuildInsertValue(
          builder,
          interfaceRefLE,
          globalState->getInterfaceTablePtr(
              globalState->program->getStruct(sourceStructReferendM->fullName)
                  ->getEdgeForInterface(targetInterfaceReferendM->fullName)),
          1,
          "interfaceRef");

  checkValidReference(
      FL(), globalState, functionState, builder, targetInterfaceTypeM, interfaceRefLE);
  return interfaceRefLE;
}

void foreachArrayElementCallInterface(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    LLVMValueRef arrayPtrLE,
    LLVMValueRef lengthLE,
    Reference* elementType,
    Reference* interfaceType,
    LLVMValueRef interfaceLE,
    int virtualParamIndex,
    int indexInEdge) {
  foreachArrayElement(
      functionState, builder, lengthLE, arrayPtrLE,
      [globalState, functionState, blockState, interfaceType, elementType, arrayPtrLE, interfaceLE, virtualParamIndex, indexInEdge](
          LLVMValueRef indexLE, LLVMBuilderRef bodyBuilder) {
        functionState->defaultRegion->alias(
            AFL("DestroyKSAIntoF consume iteration"),
            globalState, functionState, bodyBuilder, interfaceType, Ownership::BORROW, interfaceLE);

        std::vector<LLVMValueRef> indices = { constI64LE(0), indexLE };
        auto elementPtrLE = LLVMBuildGEP(bodyBuilder, arrayPtrLE, indices.data(), indices.size(), "elementPtr");
        auto elementLE = LLVMBuildLoad(bodyBuilder, elementPtrLE, "element");
        checkValidReference(FL(), globalState, functionState, bodyBuilder, elementType, elementLE);
        std::vector<LLVMValueRef> argExprsLE = { interfaceLE, elementLE };
        buildInterfaceCall(bodyBuilder, argExprsLE, virtualParamIndex, indexInEdge);
      });
}