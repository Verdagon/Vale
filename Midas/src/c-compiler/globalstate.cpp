
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
        translateInterfaceMethodToFunctionType(this, interfaceM->methods[i]);
    interfaceFunctionsLT.push_back(interfaceFunctionLT);

    assert(false); // add in the extra methods from the extra edges

    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    auto edgeFunctionL = getFunction(funcName);
    edgeFunctionsL.push_back(edgeFunctionL);
  }
  return std::make_tuple(interfaceFunctionsLT, edgeFunctionsL);
}

IRegion* GlobalState::getRegion(Reference* referenceM) {
  return getRegion(referenceM->referend);
}

IRegion* GlobalState::getRegion(Referend* referendM) {
  for (auto regionIdAndRegion : regions) {
    if (regionIdAndRegion.second->containsReferend(referendM)) {
      return regionIdAndRegion.second;
    }
  }
  assert(false);
}

IRegion* GlobalState::getRegion(RegionId* regionId) {
  if (regionId == metalCache->rcImmRegionId) {
    return rcImm;
  } else if (regionId == metalCache->linearRegionId) {
    return linearRegion;
  } else if (regionId == metalCache->unsafeRegionId) {
    return unsafeRegion;
  } else if (regionId == metalCache->assistRegionId) {
    return assistRegion;
  } else {
    assert(false);
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
  return wrap(getRegion(metalCache->intRef), metalCache->intRef, constI64LE(this, x));
}
Ref GlobalState::constI1(bool b) {
  return wrap(getRegion(metalCache->boolRef), metalCache->boolRef, constI1LE(this, b));
}
Ref GlobalState::buildAdd(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache->intRef;
  auto addPrototype = metalCache->getPrototype(metalCache->getName("__addIntInt"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}
Ref GlobalState::buildMod(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache->intRef;
  auto addPrototype = metalCache->getPrototype(metalCache->getName("__mod"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}
Ref GlobalState::buildDivide(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache->intRef;
  auto addPrototype = metalCache->getPrototype(metalCache->getName("__divideIntInt"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}

Ref GlobalState::buildMultiply(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b) {
  auto intMT = metalCache->intRef;
  auto addPrototype = metalCache->getPrototype(metalCache->getName("__multiplyIntInt"), intMT, {intMT, intMT});
  return buildExternCall(this, functionState, builder, addPrototype, { a, b });
}

Name* GlobalState::getReferendName(Referend* referend) {
  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    return structReferend->fullName;
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(referend)) {
    return interfaceReferend->fullName;
  } else if (auto ksaMT = dynamic_cast<KnownSizeArrayT*>(referend)) {
    return ksaMT->name;
  } else if (auto usaMT = dynamic_cast<UnknownSizeArrayT*>(referend)) {
    return usaMT->name;
  } else assert(false);
  return nullptr;
}

Weakability GlobalState::getReferendWeakability(Referend* referend) {
  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    return lookupStruct(structReferend->fullName)->weakability;
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(referend)) {
    return lookupInterface(interfaceReferend->fullName)->weakability;
  } else {
    return Weakability::NON_WEAKABLE;
  }
}
