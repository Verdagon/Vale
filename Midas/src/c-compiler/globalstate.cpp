
#include <function/expressions/shared/shared.h>
#include <function/expressions/expressions.h>
#include "globalstate.h"
#include "translatetype.h"
#include "region/linear/linear.h"

std::tuple<std::vector<LLVMTypeRef>, std::vector<LLVMValueRef>>
GlobalState::getEdgeFunctionTypesAndFunctions(Edge* edge) {
  auto interfaceM = program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    auto interfaceFunctionLT =
        translateInterfaceMethodToFunctionType(
            this, getRegion(interfaceM->mutability), interfaceM->methods[i]);
    interfaceFunctionsLT.push_back(interfaceFunctionLT);

    assert(false); // add in the extra methods from the extra edges

    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    auto edgeFunctionL = getFunction(funcName);
    edgeFunctionsL.push_back(edgeFunctionL);
  }
  return std::make_tuple(interfaceFunctionsLT, edgeFunctionsL);
}

IRegion* GlobalState::getRegion(Reference* referenceM) {
  if (referenceM->ownership == Ownership::SHARE) {
    return rcImm;
  } else {
    return mutRegion;
  }
}
IRegion* GlobalState::getRegion(Mutability mutability) {
  if (mutability == Mutability::IMMUTABLE) {
    return rcImm;
  } else {
    return mutRegion;
  }
}
IRegion* GlobalState::getExternRegion(Reference* referenceM) {
  if (referenceM->ownership == Ownership::SHARE) {
    return linearRegion;
  } else {
    return unsafeRegion;
  }
}
IRegion* GlobalState::getExternRegion(Mutability mutability) {
  if (mutability == Mutability::IMMUTABLE) {
    return linearRegion;
  } else {
    return unsafeRegion;
  }
}

LLVMValueRef GlobalState::getFunction(Name* name) {
  auto functionIter = functions.find(name->name);
  assert(functionIter != functions.end());
  return functionIter->second;
}

LLVMValueRef GlobalState::getInterfaceTablePtr(Edge* edge) {
  auto iter = interfaceTablePtrs.find(edge);
  assert(iter != interfaceTablePtrs.end());
  return iter->second;
}
LLVMValueRef GlobalState::getOrMakeStringConstant(const std::string& str) {
  auto iter = stringConstants.find(str);
  if (iter == stringConstants.end()) {

    iter =
        stringConstants.emplace(
                str,
                LLVMBuildGlobalStringPtr(
                    stringConstantBuilder,
                    str.c_str(),
                    (std::string("conststr") + std::to_string(stringConstants.size())).c_str()))
            .first;
  }
  return iter->second;
}

Ref GlobalState::constI64(int64_t x) {
  return wrap(getRegion(Mutability::IMMUTABLE), metalCache.intRef, constI64LE(this, x));
}
Ref GlobalState::buildAdd(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache.intRef;
  auto addPrototype = metalCache.getPrototype(metalCache.getName("__addIntInt"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}
Ref GlobalState::buildMod(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache.intRef;
  auto addPrototype = metalCache.getPrototype(metalCache.getName("__mod"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}
Ref GlobalState::buildDivide(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache.intRef;
  auto addPrototype = metalCache.getPrototype(metalCache.getName("__divideIntInt"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}

Ref GlobalState::buildMultiply(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache.intRef;
  auto addPrototype = metalCache.getPrototype(metalCache.getName("__multiplyIntInt"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}
