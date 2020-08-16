#include <iostream>
#include <regions/iregion.h>
#include "regions/shared/heap.h"

#include "translatetype.h"

LLVMTypeRef translateType(GlobalState* globalState, IRegion* region, Reference* referenceM) {
  if (dynamic_cast<Int*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMInt64Type();
  } else if (dynamic_cast<Bool*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMInt1Type();
  } else if (dynamic_cast<Str*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return region->getStringRefType();
  } else if (dynamic_cast<Never*>(referenceM->referend) != nullptr) {
    return LLVMArrayType(LLVMIntType(NEVER_INT_BITS), 0);
  } else if (auto knownSizeArrayMT =
      dynamic_cast<KnownSizeArrayT*>(referenceM->referend)) {
    return region->getKnownSizeArrayRefType(globalState, referenceM, knownSizeArrayMT);
  } else if (auto unknownSizeArrayMT =
      dynamic_cast<UnknownSizeArrayT*>(referenceM->referend)) {
    auto knownSizeArrayCountedStructLT =
        region->getUnknownSizeArrayRefType(globalState, referenceM, unknownSizeArrayMT);
    return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
  } else if (auto structReferend =
      dynamic_cast<StructReferend*>(referenceM->referend)) {

    auto structM = globalState->program->getStruct(structReferend->fullName);
    if (structM->mutability == Mutability::MUTABLE) {
      auto countedStructL = globalState->getCountedStruct(structReferend->fullName);
      if (referenceM->ownership == Ownership::OWN) {
        return LLVMPointerType(countedStructL, 0);
      } else if (referenceM->ownership == Ownership::BORROW) {
        return LLVMPointerType(countedStructL, 0);
      } else if (referenceM->ownership == Ownership::WEAK) {
        return globalState->getStructWeakRefStruct(structM->name);
      } else {
        assert(false);
      }
    } else {
      auto innerStructL = globalState->getInnerStruct(structReferend->fullName);
      if (referenceM->location == Location::INLINE) {
        return globalState->getInnerStruct(structReferend->fullName);
      } else {
        auto countedStructL = globalState->getCountedStruct(structReferend->fullName);
        return LLVMPointerType(countedStructL, 0);
      }
    }
  } else if (auto interfaceReferend =
      dynamic_cast<InterfaceReferend*>(referenceM->referend)) {
    auto interfaceM = globalState->program->getInterface(interfaceReferend->fullName);
    auto interfaceRefStructL =
        globalState->getInterfaceRefStruct(interfaceReferend->fullName);
    if (interfaceM->mutability == Mutability::MUTABLE) {
      if (referenceM->ownership == Ownership::OWN) {
        return interfaceRefStructL;
      } else if (referenceM->ownership == Ownership::BORROW) {
        return interfaceRefStructL;
      } else if (referenceM->ownership == Ownership::WEAK) {
        return globalState->getInterfaceWeakRefStruct(interfaceM->name);
      } else {
        assert(false);
      }
    } else {
      return interfaceRefStructL;
    }
  } else {
    std::cerr << "Unimplemented type: " << typeid(*referenceM->referend).name() << std::endl;
    assert(false);
    return nullptr;
  }
}

std::vector<LLVMTypeRef> translateTypes(GlobalState* globalState, IRegion* region, std::vector<Reference*> referencesM) {
  std::vector<LLVMTypeRef> result;
  for (auto referenceM : referencesM) {
    result.push_back(translateType(globalState, region, referenceM));
  }
  return result;
}

Mutability ownershipToMutability(Ownership ownership) {
  switch (ownership) {
    case Ownership::SHARE:
      return Mutability::IMMUTABLE;
    case Ownership::BORROW:
    case Ownership::OWN:
      return Mutability::MUTABLE;
  }
  assert(false);
  return Mutability::MUTABLE;
}

Mutability getMutability(GlobalState* globalState, Reference* referenceM) {
  if (dynamic_cast<Int*>(referenceM->referend) ||
      dynamic_cast<Bool*>(referenceM->referend) ||
      dynamic_cast<Float*>(referenceM->referend)) {
    return Mutability::IMMUTABLE;
  } else if (
      auto structRnd = dynamic_cast<StructReferend*>(referenceM->referend)) {
    auto structM = globalState->program->getStruct(structRnd->fullName);
    return structM->mutability;
  } else if (
      auto interfaceRnd = dynamic_cast<InterfaceReferend*>(referenceM->referend)) {
    auto interfaceM = globalState->program->getInterface(interfaceRnd->fullName);
    return interfaceM->mutability;
  } else if (
      auto knownSizeArrayMT = dynamic_cast<KnownSizeArrayT*>(referenceM->referend)) {
    return knownSizeArrayMT->rawArray->mutability;
  } else {
    std::cerr << typeid(*referenceM->referend).name() << std::endl;
    assert(false);
    return Mutability::MUTABLE;
  }
}

LLVMTypeRef translatePrototypeToFunctionType(
    GlobalState* globalState,
    IRegion* region,
    Prototype* prototype) {
  auto returnLT = translateType(globalState, region, prototype->returnType);
  auto paramsLT = translateTypes(globalState, region, prototype->params);
  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
}