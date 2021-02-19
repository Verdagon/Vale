
#include <function/expressions/shared/shared.h>
#include <function/expressions/expressions.h>
#include "globalstate.h"
#include "translatetype.h"
#include "region/linear/linear.h"

GlobalState::GlobalState(AddressNumberer* addressNumberer_) :
    addressNumberer(addressNumberer_),
    interfaceTablePtrs(0, addressNumberer->makeHasher<Edge*>()),
    interfaceExtraMethods(0, addressNumberer->makeHasher<InterfaceReferend*>()),
    edgeExtraMethods(0, addressNumberer->makeHasher<Edge*>()),
    extraFunctions(0, addressNumberer->makeHasher<Prototype*>()),
    regions(0, addressNumberer->makeHasher<RegionId*>())
{}

std::vector<LLVMTypeRef> GlobalState::getInterfaceFunctionTypes(InterfaceReferend* referend) {
  auto interfaceDefM = program->getInterface(referend->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  for (auto method : interfaceDefM->methods) {
    auto interfaceFunctionLT = translateInterfaceMethodToFunctionType(this, method);
    interfaceFunctionsLT.push_back(LLVMPointerType(interfaceFunctionLT, 0));
  }
  for (auto interfaceExtraMethod : interfaceExtraMethods[referend]) {
    auto interfaceFunctionLT =
        translateInterfaceMethodToFunctionType(this, interfaceExtraMethod);
    interfaceFunctionsLT.push_back(LLVMPointerType(interfaceFunctionLT, 0));
  }

  return interfaceFunctionsLT;
}

std::vector<LLVMValueRef> GlobalState::getEdgeFunctions(Edge* edge) {
  auto interfaceM = program->getInterface(edge->interfaceName->fullName);

  assert(edge->structPrototypesByInterfaceMethod.size() == interfaceM->methods.size());
  assert(edgeExtraMethods[edge].size() == interfaceExtraMethods[edge->interfaceName].size());

  std::vector<LLVMValueRef> edgeFunctionsL;
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    assert(edge->structPrototypesByInterfaceMethod[i].first == interfaceM->methods[i]);

    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    auto edgeFunctionL = getFunction(funcName);
    edgeFunctionsL.push_back(edgeFunctionL);
  }

  auto& extraInterfaceMethods = interfaceExtraMethods[edge->interfaceName];
  auto& extraEdgeMethods = edgeExtraMethods[edge];
  assert(extraInterfaceMethods.size() == extraEdgeMethods.size());
  for (int i = 0; i < extraInterfaceMethods.size(); i++) {
    assert(extraEdgeMethods[i].first == extraInterfaceMethods[i]);

    auto prototype = extraEdgeMethods[i].second;
    auto edgeFunctionL = extraFunctions.find(prototype)->second;
    edgeFunctionsL.push_back(edgeFunctionL);
  }

  return edgeFunctionsL;
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
