#include <iostream>
#include "region/common/heap.h"

#include "translatetype.h"

std::vector<LLVMTypeRef> translateTypes(
    IRegion* region,
    std::vector<Reference*> referencesM) {
  std::vector<LLVMTypeRef> result;
  for (auto referenceM : referencesM) {
    result.push_back(region->translateType(referenceM));
  }
  return result;
}

Mutability ownershipToMutability(Ownership ownership) {
  switch (ownership) {
    case Ownership::SHARE:
      return Mutability::IMMUTABLE;
    case Ownership::BORROW:
    case Ownership::OWN:
    case Ownership::WEAK:
      return Mutability::MUTABLE;
    default:
      assert(false);
  }
}

LLVMTypeRef translatePrototypeToFunctionType(
    IRegion* region,
    Prototype* prototype) {
  auto returnLT = region->translateType(prototype->returnType);
  auto paramsLT = translateTypes(region, prototype->params);
  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
}

LLVMTypeRef translateInterfaceMethodToFunctionType(
    IRegion* region,
    InterfaceMethod* method) {
  auto returnMT = method->prototype->returnType;
  auto paramsMT = method->prototype->params;
  auto returnLT = region->translateType(returnMT);
  auto paramsLT = translateTypes(region, paramsMT);
  paramsLT[method->virtualParamIndex] =
      region->getInterfaceMethodVirtualParamAnyType(
          paramsMT[method->virtualParamIndex]);
  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
}
