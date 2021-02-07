#include <iostream>
#include "region/common/heap.h"

#include "translatetype.h"

std::vector<LLVMTypeRef> translateTypes(
    GlobalState* globalState,
    std::vector<Reference*> referencesM) {
  std::vector<LLVMTypeRef> result;
  for (auto referenceM : referencesM) {
    result.push_back(globalState->getRegion(referenceM)->translateType(referenceM));
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
    GlobalState* globalState,
    IRegion* region,
    Prototype* prototype) {
  auto returnLT = region->translateType(prototype->returnType);
  auto paramsLT = translateTypes(globalState, prototype->params);
  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
}