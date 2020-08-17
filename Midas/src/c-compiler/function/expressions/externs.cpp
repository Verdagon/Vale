#include <iostream>
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/string.h"
#include "function/expressions/shared/controlblock.h"
#include "regions/shared/heap.h"

#include "translatetype.h"

#include "function/expression.h"

LLVMValueRef translateExternCall(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    ExternCall* call) {
  auto name = call->function->name->name;
  if (name == "F(\"__addIntInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    assert(call->argExprs[0] != nullptr);
    assert(call->argExprs[1] != nullptr);
    auto leftLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);
    auto rightLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]);
    auto result = LLVMBuildAdd(builder, leftLE, rightLE,"add");
    return result;
  } else if (name == "F(\"__divideIntInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto leftLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);
    auto rightLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]);
    auto result = LLVMBuildSDiv(builder, leftLE, rightLE,"add");
    return result;
  } else if (name == "F(\"__eqStrStr\",[],[R(*,>,s),R(*,>,s)])") {
    assert(call->argExprs.size() == 2);

    auto leftStrTypeM = call->argTypes[0];
    auto leftStrWrapperPtrLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);
    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->argTypes[0], leftStrWrapperPtrLE);

    auto rightStrTypeM = call->argTypes[1];
    auto rightStrWrapperPtrLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]);
    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->argTypes[1], rightStrWrapperPtrLE);

    std::vector<LLVMValueRef> argsLE = {
        functionState->defaultRegion->getStringBytesPtr(builder, leftStrWrapperPtrLE),
        functionState->defaultRegion->getStringBytesPtr(builder, rightStrWrapperPtrLE)
    };
    auto resultInt8LE =
        LLVMBuildCall(
            builder,
            globalState->eqStr,
            argsLE.data(),
            argsLE.size(),
            "eqStrResult");
    auto resultBoolLE = LLVMBuildICmp(builder, LLVMIntNE, resultInt8LE, LLVMConstInt(LLVMInt8Type(), 0, false), "");

    functionState->defaultRegion->dealias(FL(), globalState, functionState, blockState, builder, leftStrTypeM, leftStrWrapperPtrLE);
    functionState->defaultRegion->dealias(FL(), globalState, functionState, blockState, builder, rightStrTypeM, rightStrWrapperPtrLE);

    return resultBoolLE;
  } else if (name == "F(\"__addFloatFloat\",[],[R(*,<,f),R(*,<,f)])") {
    // VivemExterns.addFloatFloat
    assert(false);
  } else if (name == "F(\"panic\")") {
    auto exitCodeLE = makeConstIntExpr(builder, LLVMInt8Type(), 255);
    LLVMBuildCall(builder, globalState->exit, &exitCodeLE, 1, "");
    return makeConstExpr(builder, makeNever());
  } else if (name == "F(\"__multiplyIntInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    return LLVMBuildMul(
        builder,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "mul");
  } else if (name == "F(\"__subtractIntInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    return LLVMBuildSub(
        builder,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "diff");
  } else if (name == "F(\"__addStrStr\",[],[R(*,>,s),R(*,>,s)])") {
    assert(call->argExprs.size() == 2);

    auto leftStrTypeM = call->argTypes[0];
    auto leftStrWrapperPtrLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);
    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->argTypes[0], leftStrWrapperPtrLE);
    auto leftStrLenLE = functionState->defaultRegion->getStringLength(builder, leftStrWrapperPtrLE);

    auto rightStrTypeM = call->argTypes[1];
    auto rightStrWrapperPtrLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]);
    auto rightStrLenLE = functionState->defaultRegion->getStringLength(builder, rightStrWrapperPtrLE);
    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->argTypes[1], rightStrWrapperPtrLE);

    auto combinedLenLE =
        LLVMBuildAdd(builder, leftStrLenLE, rightStrLenLE, "lenSum");

    auto destStrWrapperPtrLE =
        functionState->defaultRegion->constructString(
            globalState, functionState, builder, combinedLenLE);

    std::vector<LLVMValueRef> argsLE = {
        functionState->defaultRegion->getStringBytesPtr(builder, leftStrWrapperPtrLE),
        functionState->defaultRegion->getStringBytesPtr(builder, rightStrWrapperPtrLE),
        functionState->defaultRegion->getStringBytesPtr(builder, destStrWrapperPtrLE),
    };
    LLVMBuildCall(builder, globalState->addStr, argsLE.data(), argsLE.size(), "");

    functionState->defaultRegion->dealias(FL(), globalState, functionState, blockState, builder, leftStrTypeM, leftStrWrapperPtrLE);
    functionState->defaultRegion->dealias(FL(), globalState, functionState, blockState, builder, rightStrTypeM, rightStrWrapperPtrLE);

    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->function->returnType, destStrWrapperPtrLE);

    return destStrWrapperPtrLE;
  } else if (name == "F(\"__getch\")") {
    return LLVMBuildCall(builder, globalState->getch, nullptr, 0, "");
  } else if (name == "F(\"__lessThanInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSLT,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__greaterThanInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSGT,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__greaterThanOrEqInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSGE,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__lessThanOrEqInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSLE,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__eqIntInt\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntEQ,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__eqBoolBool\",[],[R(*,<,b),R(*,<,b)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntEQ,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__print\",[],[R(*,>,s)])") {
    assert(call->argExprs.size() == 1);

    auto argStrTypeM = call->argTypes[0];
    auto argStrRefLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);
    functionState->defaultRegion->checkValidReference(FL(), globalState, functionState, builder, call->argTypes[0], argStrRefLE);

    std::vector<LLVMValueRef> argsLE = {
        functionState->defaultRegion->getStringBytesPtr(builder, argStrRefLE),
    };
    LLVMBuildCall(builder, globalState->printStr, argsLE.data(), argsLE.size(), "");

    functionState->defaultRegion->dealias(FL(), globalState, functionState, blockState, builder, argStrTypeM, argStrRefLE);

    return LLVMGetUndef(functionState->defaultRegion->translateType(globalState, call->function->returnType));
  } else if (name == "F(\"__not\",[],[R(*,<,b)])") {
    assert(call->argExprs.size() == 1);
    auto result = LLVMBuildNot(
        builder,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        "");
    return result;
  } else if (name == "F(\"__castIntStr\",[],[R(*,<,i)])") {
    assert(call->argExprs.size() == 1);
    auto intLE =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);

    int bufferSize = 150;
    auto charsPtrLocalLE =
        LLVMBuildAlloca(builder, LLVMArrayType(LLVMInt8Type(), bufferSize), "charsPtrLocal");
    auto itoaDestPtrLE =
        LLVMBuildPointerCast(
            builder, charsPtrLocalLE, LLVMPointerType(LLVMInt8Type(), 0), "itoaDestPtr");
    std::vector<LLVMValueRef> atoiArgsLE = { intLE, itoaDestPtrLE, constI64LE(bufferSize) };
    LLVMBuildCall(builder, globalState->intToCStr, atoiArgsLE.data(), atoiArgsLE.size(), "");

    std::vector<LLVMValueRef> strlenArgsLE = { itoaDestPtrLE };
    auto lengthLE = LLVMBuildCall(builder, globalState->strlen, strlenArgsLE.data(), strlenArgsLE.size(), "");

    auto strWrapperPtrLE = functionState->defaultRegion->constructString(globalState, functionState, builder, lengthLE);
    auto strBytesPtrLE = functionState->defaultRegion->getStringBytesPtr(builder, strWrapperPtrLE);
    std::vector<LLVMValueRef> argsLE = { strBytesPtrLE, itoaDestPtrLE, lengthLE };
    LLVMBuildCall(builder, globalState->strncpy, argsLE.data(), argsLE.size(), "");

    std::vector<LLVMValueRef> indicesLE = { lengthLE };
    LLVMBuildStore(
        builder,
        LLVMConstInt(LLVMInt8Type(), 0, 0),//
        LLVMBuildInBoundsGEP(builder, strBytesPtrLE, indicesLE.data(), indicesLE.size(), ""));

    return strWrapperPtrLE;
  } else if (name == "F(\"__and\",[],[R(*,<,b),R(*,<,b)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildAnd(
        builder,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__or\",[],[R(*,<,b),R(*,<,b)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildOr(
        builder,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else if (name == "F(\"__mod\",[],[R(*,<,i),R(*,<,i)])") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildSRem(
        builder,
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]),
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[1]),
        "");
    return result;
  } else {
    std::cerr << name << std::endl;
    assert(false);
  }
  assert(false);
  return nullptr;
}
