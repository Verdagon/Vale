#include <iostream>
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/string.h"
#include "region/common/controlblock.h"
#include "region/common/heap.h"

#include "translatetype.h"

#include "function/expression.h"

Ref translateExternCall(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    ExternCall* call) {
  auto name = call->function->name->name;
  if (name == "__addIntInt") {
    assert(call->argExprs.size() == 2);
    auto leftLE =
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0]));
    auto rightLE =
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1]));
    auto result = LLVMBuildAdd(builder, leftLE, rightLE,"add");
    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, result);
  } else if (name == "__divideIntInt") {
    assert(call->argExprs.size() == 2);
    auto leftLE =
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0]));
    auto rightLE =
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1]));
    auto result = LLVMBuildSDiv(builder, leftLE, rightLE,"add");
    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, result);
//  } else if (name == "__eqStrStr") {
//    assert(call->argExprs.size() == 2);
//
//    auto leftStrTypeM = call->argTypes[0];
//    auto leftStrRef =
//        translateExpression(
//            globalState, functionState, blockState, builder, call->argExprs[0]);
//
//    auto rightStrTypeM = call->argTypes[1];
//    auto rightStrRef =
//        translateExpression(
//            globalState, functionState, blockState, builder, call->argExprs[1]);
//
//    std::vector<LLVMValueRef> argsLE = {
//        globalState->getRegion(refHere)->getStringBytesPtr(functionState, builder, leftStrRef),
//        globalState->getRegion(refHere)->getStringBytesPtr(functionState, builder, rightStrRef)
//    };
//    auto resultInt8LE =
//        LLVMBuildCall(
//            builder,
//            globalState->eqStr,
//            argsLE.data(),
//            argsLE.size(),
//            "eqStrResult");
//    auto resultBoolLE = LLVMBuildICmp(builder, LLVMIntNE, resultInt8LE, LLVMConstInt(LLVMInt8TypeInContext(globalState->context), 0, false), "");
//
//    globalState->getRegion(refHere)->dealias(FL(), functionState, blockState, builder, globalState->metalCache.strRef, leftStrRef);
//    globalState->getRegion(refHere)->dealias(FL(), functionState, blockState, builder, globalState->metalCache.strRef, rightStrRef);
//
//    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, resultBoolLE);
  } else if (name == "__strLength") {
    assert(call->argExprs.size() == 1);

    auto leftStrRef =
        translateExpression(
            globalState, functionState, blockState, builder, call->argExprs[0]);
    auto resultLenLE = globalState->getRegion(globalState->metalCache.strRef)->getStringLen(functionState, builder, leftStrRef);

    globalState->getRegion(globalState->metalCache.strRef)->dealias(
        FL(), functionState, builder, globalState->metalCache.strRef, leftStrRef);

    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, resultLenLE);
  } else if (name == "__addFloatFloat") {
    // VivemExterns.addFloatFloat
    assert(false);
  } else if (name == "__panic") {
    auto exitCodeLE = makeConstIntExpr(functionState, builder, LLVMInt8TypeInContext(globalState->context), 255);
    LLVMBuildCall(builder, globalState->exit, &exitCodeLE, 1, "");
    LLVMBuildRet(builder, LLVMGetUndef(functionState->returnTypeL));
    return wrap(globalState->getRegion(globalState->metalCache.neverRef), globalState->metalCache.neverRef, globalState->neverPtr);
  } else if (name == "__multiplyIntInt") {
    assert(call->argExprs.size() == 2);
    auto resultIntLE =
        LLVMBuildMul(
            builder,
            globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
                functionState, builder, call->function->params[0],
                translateExpression(
                    globalState, functionState, blockState, builder, call->argExprs[0])),
            globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
                functionState, builder, call->function->params[1],
                translateExpression(
                    globalState, functionState, blockState, builder, call->argExprs[1])),
            "mul");
    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, resultIntLE);
  } else if (name == "__subtractIntInt") {
    assert(call->argExprs.size() == 2);
    auto resultIntLE =
        LLVMBuildSub(
            builder,
            globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
                functionState, builder, call->function->params[0],
                translateExpression(
                    globalState, functionState, blockState, builder, call->argExprs[0])),
            globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
                functionState, builder, call->function->params[1],
                translateExpression(
                    globalState, functionState, blockState, builder, call->argExprs[1])),
            "diff");
    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, resultIntLE);
  } else if (name == "__getch") {
    auto resultIntLE = LLVMBuildCall(builder, globalState->getch, nullptr, 0, "");
    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, resultIntLE);
  } else if (name == "__lessThanInt") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSLT,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__greaterThanInt") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSGT,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__greaterThanOrEqInt") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSGE,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__lessThanOrEqInt") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntSLE,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__eqIntInt") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntEQ,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__eqBoolBool") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildICmp(
        builder,
        LLVMIntEQ,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__not") {
    assert(call->argExprs.size() == 1);
    auto result = LLVMBuildNot(
        builder,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__and") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildAnd(
        builder,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__or") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildOr(
        builder,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.boolRef), globalState->metalCache.boolRef, result);
  } else if (name == "__mod") {
    assert(call->argExprs.size() == 2);
    auto result = LLVMBuildSRem(
        builder,
        globalState->getRegion(call->function->params[0])->checkValidReference(FL(),
            functionState, builder, call->function->params[0],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[0])),
        globalState->getRegion(call->function->params[1])->checkValidReference(FL(),
            functionState, builder, call->function->params[1],
            translateExpression(
                globalState, functionState, blockState, builder, call->argExprs[1])),
        "");
    return wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef, result);
  } else {

    auto args = std::vector<Ref>{};
    args.reserve(call->argExprs.size());
    for (int i = 0; i < call->argExprs.size(); i++) {
      auto argExpr = call->argExprs[i];
      auto argRefMT = call->function->params[i];
      auto argRef = translateExpression(globalState, functionState, blockState, builder, argExpr);
      args.push_back(argRef);
    }

    auto argsLE = std::vector<LLVMValueRef>{};
    argsLE.reserve(call->argExprs.size());
    for (int i = 0; i < call->argExprs.size(); i++) {
      auto argRefMT = call->function->params[i];
      auto arg = args[i];

      auto externalArgRefLE =
          (argRefMT->ownership == Ownership::SHARE ?
            globalState->getRegion(argRefMT)->copyToWild(functionState, builder, argRefMT, arg) :
           globalState->getRegion(argRefMT)->sendRefToWild(functionState, builder, argRefMT, arg));
      argsLE.push_back(externalArgRefLE);
    }

    auto externFuncIter = globalState->externFunctions.find(call->function->name->name);
    assert(externFuncIter != globalState->externFunctions.end());
    auto externFuncL = externFuncIter->second;

    buildFlare(FL(), globalState, functionState, builder, "Suspending function ", functionState->containingFuncName);
    buildFlare(FL(), globalState, functionState, builder, "Calling extern function ", call->function->name->name);

    for (int i = 0; i < call->argExprs.size(); i++) {
      auto argRefMT = call->function->params[i];
      // Dealias any object heading into the outside world, see DEPAR.
      globalState->getRegion(argRefMT)->dealias(FL(), functionState, builder, argRefMT, args[i]);
    }

    auto resultLE = LLVMBuildCall(builder, externFuncL, argsLE.data(), argsLE.size(), "");
//    auto resultRef = wrap(globalState->getRegion(refHere), call->function->returnType, resultLE);
//    globalState->getRegion(refHere)->checkValidReference(FL(), functionState, builder, call->function->returnType, resultRef);

    if (call->function->returnType->referend == globalState->metalCache.never) {
      buildFlare(FL(), globalState, functionState, builder, "Done calling function ", call->function->name->name);
      buildFlare(FL(), globalState, functionState, builder, "Resuming function ", functionState->containingFuncName);
      LLVMBuildRet(builder, LLVMGetUndef(functionState->returnTypeL));
      return wrap(globalState->getRegion(globalState->metalCache.neverRef), globalState->metalCache.neverRef, globalState->neverPtr);
    } else {
      buildFlare(FL(), globalState, functionState, builder, "Done calling function ", call->function->name->name);
      buildFlare(FL(), globalState, functionState, builder, "Resuming function ", functionState->containingFuncName);

      auto internalArgRef =
          (call->function->returnType->ownership == Ownership::SHARE ?
          globalState->getRegion(call->function->returnType)->copyFromWild(functionState, builder, call->function->returnType, resultLE) :
           globalState->getRegion(call->function->returnType)->receiveRefFromWild(functionState, builder, call->function->returnType, resultLE));

      // Alias any object coming from the outside world, see DEPAR.
      globalState->getRegion(call->function->returnType)->alias(
          FL(), functionState, builder, call->function->returnType, internalArgRef);

      return internalArgRef;
    }
  }
  assert(false);
}
