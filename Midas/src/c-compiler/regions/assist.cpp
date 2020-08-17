#include <translatetype.h>
#include <regions/shared/struct.h>
#include <regions/shared/census.h>
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/members.h"
#include "function/expressions/shared/branch.h"
#include "regions/shared/heap.h"
#include "function/expressions/shared/controlblock.h"
#include "assist.h"


constexpr int controlBlockTypeStrIndex = 0;
constexpr int controlBlockObjIdIndex = 1;
constexpr int controlBlockRcMemberIndex = 2;
constexpr int controlBlockWrciMemberIndex = 3;

LLVMValueRef allocateStruct(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    LLVMTypeRef structL) {
  adjustCounter(builder, globalState->liveHeapObjCounter, 1);

  LLVMValueRef resultPtrLE = nullptr;
  if (structTypeM->location == Location::INLINE) {
    resultPtrLE = LLVMBuildAlloca(builder, structL, "newstruct");
  } else if (structTypeM->location == Location::YONDER) {
    size_t sizeBytes = LLVMABISizeOfType(globalState->dataLayout, structL);
    LLVMValueRef sizeLE = LLVMConstInt(LLVMInt64Type(), sizeBytes, false);

    auto newStructLE =
        LLVMBuildCall(builder, globalState->malloc, &sizeLE, 1, "");

    resultPtrLE =
        LLVMBuildBitCast(
            builder, newStructLE, LLVMPointerType(structL, 0), "newstruct");
  } else {
    assert(false);
    return nullptr;
  }

  if (globalState->opt->census) {
    LLVMValueRef resultAsVoidPtrLE =
        LLVMBuildBitCast(
            builder, resultPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
  }
  return resultPtrLE;
}

LLVMValueRef mallocUnknownSizeArray(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMTypeRef usaWrapperLT,
    LLVMTypeRef usaElementLT,
    LLVMValueRef lengthLE) {
  auto sizeBytesLE =
      LLVMBuildAdd(
          builder,
          constI64LE(LLVMABISizeOfType(globalState->dataLayout, usaWrapperLT)),
          LLVMBuildMul(
              builder,
              constI64LE(LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(usaElementLT, 1))),
              lengthLE,
              ""),
          "usaMallocSizeBytes");

  auto newWrapperPtrLE =
      LLVMBuildCall(builder, globalState->malloc, &sizeBytesLE, 1, "");

  adjustCounter(builder, globalState->liveHeapObjCounter, 1);

  if (globalState->opt->census) {
    LLVMValueRef resultAsVoidPtrLE =
        LLVMBuildBitCast(
            builder, newWrapperPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
  }

  return LLVMBuildBitCast(
      builder,
      newWrapperPtrLE,
      LLVMPointerType(usaWrapperLT, 0),
      "newstruct");
}



LLVMTypeRef AssistRegion::makeInnerKnownSizeArrayLT(GlobalState* globalState, KnownSizeArrayT* knownSizeArrayMT) {
  auto elementLT = translateType(globalState, knownSizeArrayMT->rawArray->elementType);
  return LLVMArrayType(elementLT, knownSizeArrayMT->size);
}

// This gives the actual struct, *not* a pointer to a struct, which you sometimes
// might need instead. For that, use translateType.
LLVMTypeRef AssistRegion::translateKnownSizeArrayToWrapperStruct(
    GlobalState* globalState,
    KnownSizeArrayT* knownSizeArrayMT) {
  auto innerArrayLT = makeInnerKnownSizeArrayLT(globalState, knownSizeArrayMT);

  auto iter = knownSizeArrayCountedStructs.find(knownSizeArrayMT->name);
  if (iter == knownSizeArrayCountedStructs.end()) {
    auto countedStruct = LLVMStructCreateNamed(LLVMGetGlobalContext(), knownSizeArrayMT->name->name.c_str());
    std::vector<LLVMTypeRef> elementsL;
    elementsL.push_back(nonWeakableControlBlockStructL);
    elementsL.push_back(innerArrayLT);
    LLVMStructSetBody(countedStruct, elementsL.data(), elementsL.size(), false);

    iter = knownSizeArrayCountedStructs.emplace(knownSizeArrayMT->name, countedStruct).first;
  }

  return iter->second;
}

LLVMTypeRef AssistRegion::makeInnerUnknownSizeArrayLT(GlobalState* globalState, UnknownSizeArrayT* unknownSizeArrayMT) {
  auto elementLT = translateType(globalState, unknownSizeArrayMT->rawArray->elementType);
  return LLVMArrayType(elementLT, 0);
}

LLVMTypeRef AssistRegion::getKnownSizeArrayType(
    GlobalState* globalState,
    KnownSizeArrayT* knownSizeArrayMT) {
  if (knownSizeArrayMT->rawArray->mutability == Mutability::MUTABLE) {
    assert(false);
    return nullptr;
  } else {
    auto innerArrayLT = makeInnerKnownSizeArrayLT(globalState, knownSizeArrayMT);
    auto knownSizeArrayCountedStructLT =
        translateKnownSizeArrayToWrapperStruct(
            globalState, knownSizeArrayMT);
    return knownSizeArrayCountedStructLT;
  }
}

LLVMTypeRef AssistRegion::getKnownSizeArrayRefType(
    GlobalState* globalState,
    Reference* referenceM,
    KnownSizeArrayT* knownSizeArrayMT) {
  if (knownSizeArrayMT->rawArray->mutability == Mutability::MUTABLE) {
    assert(false);
    return nullptr;
  } else {
    auto innerArrayLT = makeInnerKnownSizeArrayLT(globalState,
        knownSizeArrayMT);
    if (referenceM->location == Location::INLINE) {
      return innerArrayLT;
    } else {
      auto knownSizeArrayCountedStructLT =
          translateKnownSizeArrayToWrapperStruct(
              globalState, knownSizeArrayMT);

      return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
    }
  }
}

LLVMTypeRef AssistRegion::getUnknownSizeArrayRefType(
    GlobalState* globalState,
    Reference* referenceM,
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto innerArrayLT = makeInnerUnknownSizeArrayLT(globalState, unknownSizeArrayMT);

  auto iter = unknownSizeArrayCountedStructs.find(unknownSizeArrayMT->name);
  if (iter == unknownSizeArrayCountedStructs.end()) {
    auto countedStruct = LLVMStructCreateNamed(LLVMGetGlobalContext(), (unknownSizeArrayMT->name->name + "rc").c_str());
    std::vector<LLVMTypeRef> elementsL;
    elementsL.push_back(getControlBlockStructForUnknownSizeArray(unknownSizeArrayMT));
    elementsL.push_back(LLVMInt64Type());
    elementsL.push_back(innerArrayLT);
    LLVMStructSetBody(countedStruct, elementsL.data(), elementsL.size(), false);

    iter = unknownSizeArrayCountedStructs.emplace(unknownSizeArrayMT->name, countedStruct).first;
  }

  return iter->second;
}

LLVMValueRef AssistRegion::getControlBlockPtr(
    LLVMBuilderRef builder,
    // This will be a pointer if a mutable struct, or a fat ref if an interface.
    LLVMValueRef referenceLE,
    Reference* refM) {
  if (dynamic_cast<InterfaceReferend*>(refM->referend)) {
    return getInterfaceControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<StructReferend*>(refM->referend)) {
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<KnownSizeArrayT*>(refM->referend)) {
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<UnknownSizeArrayT*>(refM->referend)) {
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<Str*>(refM->referend)) {
    return getStringControlBlockPtr(builder, referenceLE);
  } else {
    std::cerr << "Unknown: " << typeid(*refM->referend).name() << std::endl;
    assert(false);
    return nullptr;
  }
}

void AssistRegion::flareAdjustStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef controlBlockPtr,
    LLVMValueRef oldAmount,
    LLVMValueRef newAmount) {
  buildFlare(
      from,
      globalState,
      functionState,
      builder,
      typeid(*refM->referend).name(),
      " ",
      getTypeNameStrPtrFromControlBlockPtr(globalState, builder, controlBlockPtr),
      getObjIdFromControlBlockPtr(globalState, builder, controlBlockPtr),
      ", ",
      oldAmount,
      "->",
      newAmount);
}

// Returns the new RC
LLVMValueRef AssistRegion::adjustStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef exprLE,
    Reference* refM,
    int amount) {
  auto controlBlockPtrLE = getControlBlockPtr(builder, exprLE, refM);
  auto rcPtrLE = getStrongRcPtrFromControlBlockPtr(builder, controlBlockPtrLE);
  auto oldRc = LLVMBuildLoad(builder, rcPtrLE, "oldRc");
  auto newRc = adjustCounter(builder, rcPtrLE, amount);
  if (refM->ownership != Ownership::SHARE) {
    adjustCounter(builder, globalState->mutRcAccessCounter, 1);
  }
  flareAdjustStrongRc(from, globalState, functionState, builder, refM, controlBlockPtrLE, oldRc, newRc);
  return newRc;
}

LLVMValueRef AssistRegion::strongRcIsZero(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef exprLE,
    Reference* refM) {
  auto controlBlockPtr = getControlBlockPtr(builder, exprLE, refM);
  return isZeroLE(builder, getStrongRcFromControlBlockPtr(globalState, builder, controlBlockPtr));
}

LLVMValueRef AssistRegion::constructUnknownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMTypeRef usaWrapperPtrLT,
    LLVMTypeRef usaElementLT,
    LLVMValueRef sizeLE,
    const std::string& typeName) {
  auto usaWrapperPtrLE =
      mallocUnknownSizeArray(
          globalState, builder, usaWrapperPtrLT, usaElementLT, sizeLE);
  fillControlBlock(
      globalState,
      builder,
      getConcreteControlBlockPtr(builder, usaWrapperPtrLE),
      false,
      typeName);
  return usaWrapperPtrLE;
}

LLVMValueRef AssistRegion::getConcreteControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef concretePtrLE) {
  // Control block is always the 0th element of every concrete struct.
  return LLVMBuildStructGEP(builder, concretePtrLE, 0, "controlPtr");
}

LLVMValueRef AssistRegion::getInterfaceControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef interfaceRefLE) {
  // Interface fat pointer's first element points directly at the control block,
  // and we dont have to cast it. We would have to cast if we were accessing the
  // actual object though.
  return LLVMBuildExtractValue(builder, interfaceRefLE, 0, "controlPtr");
}

// See CRCISFAORC for why we don't take in a mutability.
LLVMValueRef AssistRegion::getStrongRcPtrFromControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr) {
  return LLVMBuildStructGEP(
      builder,
      controlBlockPtr,
      controlBlockRcMemberIndex,
      "rcPtr");
}

LLVMValueRef AssistRegion::getWrciFromControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockWrciMemberIndex) {
  auto wrciPtrLE =
      LLVMBuildStructGEP(
          builder,
          controlBlockPtr,
          controlBlockWrciMemberIndex,
          "wrciPtr");
  return LLVMBuildLoad(builder, wrciPtrLE, "wrci");
}

LLVMValueRef AssistRegion::getObjIdFromControlBlockPtr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr) {
  return LLVMBuildLoad(
      builder,
      LLVMBuildStructGEP(
          builder,
          controlBlockPtr,
          controlBlockObjIdIndex,
          "objIdPtr"),
      "objId");
}

LLVMValueRef AssistRegion::getTypeNameStrPtrFromControlBlockPtr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr) {
  return LLVMBuildLoad(
      builder,
      LLVMBuildStructGEP(
          builder,
          controlBlockPtr,
          controlBlockTypeStrIndex,
          "typeNameStrPtrPtr"),
      "typeNameStrPtr");
}

LLVMValueRef AssistRegion::getStrongRcFromControlBlockPtr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef structExpr) {
  auto rcPtrLE = getStrongRcPtrFromControlBlockPtr(builder, structExpr);
  return LLVMBuildLoad(builder, rcPtrLE, "rc");
}

LLVMValueRef AssistRegion::getIsAliveFromWeakRef(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE) {
  auto wrciLE = getWrciFromWeakRef(builder, weakRefLE);
  return LLVMBuildCall(builder, globalState->wrcIsLive, &wrciLE, 1, "isAlive");
}

LLVMValueRef AssistRegion::getConstraintRefFromWeakRef(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE,
    Reference* constraintRefM) {
  auto refLE = getObjPtrFromWeakRef(builder, weakRefLE);
  checkValidReference(FL(), globalState, functionState, builder, constraintRefM, refLE);

  Reference* targetType =
      globalState->metalCache.getReference(
          constraintRefM->referend,
          constraintRefM->location,
          Ownership::BORROW);
  alias(FL(), globalState, functionState, builder, constraintRefM, targetType, refLE);
  return refLE;
}

void AssistRegion::fillInnerStruct(
    LLVMBuilderRef builder,
    StructDefinition* structM,
    std::vector<LLVMValueRef> membersLE,
    LLVMValueRef innerStructPtrLE) {
  for (int i = 0; i < membersLE.size(); i++) {
    auto memberName = structM->members[i]->name;
    auto ptrLE =
        LLVMBuildStructGEP(builder, innerStructPtrLE, i, memberName.c_str());
    LLVMBuildStore(builder, membersLE[i], ptrLE);
  }
}

LLVMValueRef AssistRegion::constructCountedStruct(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMTypeRef structL,
    Reference* structTypeM,
    StructDefinition* structM,
    std::vector<LLVMValueRef> membersLE) {
  LLVMValueRef newStructPtrLE = allocateStruct(globalState, builder, structTypeM, structL);
  auto objIdLE =
      fillControlBlock(
          globalState,
          builder,
          getConcreteControlBlockPtr(builder, newStructPtrLE),
          structM->weakable,
          structM->name->name);
  fillInnerStruct(
      builder, structM, membersLE,
      getStructContentsPtr(builder, newStructPtrLE));
  buildFlare(from, globalState, functionState, builder, "Allocating ", structM->name->name, objIdLE);
  return newStructPtrLE;
}

LLVMValueRef AssistRegion::constructInnerStruct(
    LLVMBuilderRef builder,
    StructDefinition* structM,
    LLVMTypeRef valStructL,
    const std::vector<LLVMValueRef>& membersLE) {

  // We always start with an undef, and then fill in its fields one at a
  // time.
  LLVMValueRef structValueBeingInitialized = LLVMGetUndef(valStructL);
  for (int i = 0; i < membersLE.size(); i++) {
    auto memberName = structM->members[i]->name;
    // Every time we fill in a field, it actually makes a new entire
    // struct value, and gives us a LLVMValueRef for the new value.
    // So, `structValueBeingInitialized` contains the latest one.
    structValueBeingInitialized =
        LLVMBuildInsertValue(
            builder,
            structValueBeingInitialized,
            membersLE[i],
            i,
            memberName.c_str());
  }
  return structValueBeingInitialized;
}

// Returns object ID
LLVMValueRef AssistRegion::fillControlBlock(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    bool weakable,
    const std::string& typeName) {

  auto objIdLE = adjustCounter(builder, globalState->objIdCounter, 1);

  LLVMValueRef newControlBlockLE = nullptr;
  if (weakable) {
    newControlBlockLE = LLVMGetUndef(weakableControlBlockStructL);
  } else {
    newControlBlockLE = LLVMGetUndef(nonWeakableControlBlockStructL);
  }
  newControlBlockLE =
      LLVMBuildInsertValue(
          builder,
          newControlBlockLE,
          // Start at 1, 0 would mean it's dead.
          LLVMConstInt(LLVMInt64Type(), 1, false),
          controlBlockRcMemberIndex,
          "controlBlockWithRc");
  newControlBlockLE =
      LLVMBuildInsertValue(
          builder,
          newControlBlockLE,
          objIdLE,
          controlBlockObjIdIndex,
          "controlBlockWithRcAndObjId");
  newControlBlockLE =
      LLVMBuildInsertValue(
          builder,
          newControlBlockLE,
          globalState->getOrMakeStringConstant(typeName),
          controlBlockTypeStrIndex,
          "controlBlockComplete");
  if (weakable) {
    auto wrciLE = LLVMBuildCall(builder, globalState->allocWrc, nullptr, 0, "");
    newControlBlockLE =
        LLVMBuildInsertValue(
            builder,
            newControlBlockLE,
            wrciLE,
            controlBlockWrciMemberIndex,
            "controlBlockComplete");
  }
  LLVMBuildStore(
      builder,
      newControlBlockLE,
      controlBlockPtrLE);
  return objIdLE;
}

LLVMTypeRef makeWeakableControlBlockStruct() {
  auto voidLT = LLVMVoidType();
  auto voidPtrLT = LLVMPointerType(voidLT, 0);
  auto int1LT = LLVMInt1Type();
  auto int8LT = LLVMInt8Type();
  auto int64LT = LLVMInt64Type();
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int64PtrLT = LLVMPointerType(int64LT, 0);

  auto controlBlockStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
  std::vector<LLVMTypeRef> memberTypesL;

  assert(controlBlockTypeStrIndex == memberTypesL.size());
  memberTypesL.push_back(int8PtrLT);

  assert(controlBlockObjIdIndex == memberTypesL.size());
  memberTypesL.push_back(int64LT);

  assert(controlBlockRcMemberIndex == memberTypesL.size());
  memberTypesL.push_back(int64LT);

  assert(controlBlockWrciMemberIndex == memberTypesL.size());
  memberTypesL.push_back(int64LT);

  LLVMStructSetBody(
      controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
  return controlBlockStructL;
}

LLVMTypeRef makeNonWeakableControlBlockStruct() {
  auto voidLT = LLVMVoidType();
  auto voidPtrLT = LLVMPointerType(voidLT, 0);
  auto int1LT = LLVMInt1Type();
  auto int8LT = LLVMInt8Type();
  auto int64LT = LLVMInt64Type();
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int64PtrLT = LLVMPointerType(int64LT, 0);

  auto controlBlockStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
  std::vector<LLVMTypeRef> memberTypesL;

  assert(memberTypesL.size() == controlBlockTypeStrIndex); // should match weakable
  memberTypesL.push_back(int8PtrLT);

  assert(memberTypesL.size() == controlBlockObjIdIndex); // should match weakable
  memberTypesL.push_back(int64LT);

  assert(memberTypesL.size() == controlBlockRcMemberIndex); // should match weakable
  memberTypesL.push_back(int64LT);

  LLVMStructSetBody(
      controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
  return controlBlockStructL;
}

AssistRegion::AssistRegion() :
    weakableControlBlockStructL(makeWeakableControlBlockStruct()),
    nonWeakableControlBlockStructL(makeNonWeakableControlBlockStruct()),
    immMixin() {
}

LLVMValueRef AssistRegion::allocate(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* desiredReference,
    const std::vector<LLVMValueRef>& membersLE) {
  auto structReferend =
      dynamic_cast<StructReferend*>(desiredReference->referend);
  assert(structReferend);

  auto structM = globalState->program->getStruct(structReferend->fullName);

  switch (structM->mutability) {
    case Mutability::MUTABLE: {
      auto countedStructL = getCountedStruct(structReferend->fullName);
      return constructCountedStruct(
          from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
    }
    case Mutability::IMMUTABLE: {
      if (desiredReference->location == Location::INLINE) {
        auto valStructL =
            getInnerStruct(structReferend->fullName);
        return constructInnerStruct(
            builder, structM, valStructL, membersLE);
      } else {
        auto countedStructL =
            getCountedStruct(structReferend->fullName);
        return constructCountedStruct(
            from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
      }
    }
    default:
      assert(false);
      return nullptr;
  }
  assert(false);
}

LLVMValueRef AssistRegion::castOwnership(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* sourceType,
    Ownership targetOwnership,
    LLVMValueRef sourceRefLE) {
  auto sourceOwnership = sourceType->ownership;
  if (sourceOwnership == Ownership::SHARE) {
    if (sourceType->location == Location::INLINE) {
      return sourceRefLE;
    } else {
      return sourceRefLE;
    }
  } else if (sourceOwnership == Ownership::OWN) {
    if (targetOwnership == Ownership::OWN) {
      // Cant load an owning reference from a owning local. That would require an unstackify.
      assert(false);
    } else if (targetOwnership == Ownership::BORROW) {
      // We do the same thing for inline and yonder muts, the only difference is
      // where the memory lives.
      return sourceRefLE;
    } else if (targetOwnership == Ownership::WEAK) {
      // Now we need to package it up into a weak ref.
      if (auto structReferend = dynamic_cast<StructReferend*>(sourceType->referend)) {
        auto controlBlockPtr = getConcreteControlBlockPtr(builder, sourceRefLE);
        auto weakRefStructL = getStructWeakRefStruct(structReferend->fullName);
        return assembleStructWeakRef(
                globalState, builder, sourceType, structReferend, weakRefStructL, sourceRefLE, controlBlockWrciMemberIndex, controlBlockPtr);
      } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(sourceType->referend)) {
        assert(false); // impl
      } else assert(false);
    } else {
      assert(false);
    }
  } else if (sourceOwnership == Ownership::BORROW) {
    if (targetOwnership == Ownership::OWN) {
      assert(false); // Cant load an owning reference from a constraint ref local.
    } else if (targetOwnership == Ownership::BORROW) {
      // We do the same thing for inline and yonder muts, the only difference is
      // where the memory lives.
      return sourceRefLE;
    } else if (targetOwnership == Ownership::WEAK) {
      // Making a weak ref from a constraint ref local.

      if (auto structReferendM = dynamic_cast<StructReferend*>(sourceType->referend)) {
        auto controlBlockPtr = getConcreteControlBlockPtr(builder, sourceRefLE);
        // We do the same thing for inline and yonder muts, the only difference is
        // where the memory lives.
        auto weakRefStructL = getStructWeakRefStruct(structReferendM->fullName);
        return
            assembleStructWeakRef(
                globalState, builder, sourceType, structReferendM, weakRefStructL, sourceRefLE, controlBlockWrciMemberIndex, controlBlockPtr);
      } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(sourceType->referend)) {
        assert(false); // impl
      } else assert(false);
    } else {
      assert(false);
    }
  } else if (sourceOwnership == Ownership::WEAK) {
    if (targetOwnership == Ownership::OWN) {
      assert(false); // Cant load an owning reference from a weak ref local.
    } else if (targetOwnership == Ownership::BORROW) {
      assert(false); // Can't implicitly make a constraint ref from a weak ref.
    } else if (targetOwnership == Ownership::WEAK) {
      // We do the same thing for inline and yonder muts, the only difference is
      // where the memory lives.
      return sourceRefLE;
    } else {
      assert(false);
    }
  } else {
    assert(false);
  }
  assert(false);
}

LLVMValueRef AssistRegion::alias(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    Reference* targetRef,
    LLVMValueRef sourceLE) {
  // The only difference between sourceRef and targetRef is the ownership.
  // Otherwise, we'd have to upgrade this function a bit.
  assert(sourceRef->referend == targetRef->referend);
  assert(sourceRef->location == targetRef->location);

  if (targetRef->ownership == Ownership::SHARE) {
    checkValidReference(from, globalState, functionState, builder, targetRef, sourceLE);
    return immMixin.alias(from, globalState, functionState, builder, targetRef, sourceLE);
  } else {
    auto sourceRnd = sourceRef->referend;

    auto expr = castOwnership(globalState, builder, sourceRef, targetRef->ownership, sourceLE);

    if (targetRef->ownership == Ownership::BORROW) {
      adjustStrongRc(from, globalState, functionState, builder, expr, targetRef, 1);
    } else if (targetRef->ownership == Ownership::WEAK) {
      incrementWeakRc(from, globalState, functionState, builder, targetRef, expr);
    } else if (targetRef->ownership == Ownership::OWN) {
      if (dynamic_cast<InterfaceReferend *>(sourceRnd)) {
        // We should never acquire an owning reference.
        // If you trip this, perhaps you're trying to borrow, and you handed in
        // the wrong thing for resultRef?
        assert(false);
      } else if (dynamic_cast<StructReferend *>(sourceRnd) ||
          dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
          dynamic_cast<UnknownSizeArrayT *>(sourceRnd)) {
        // We might be loading a member as an own if we're destructuring.
        // Don't adjust the RC, since we're only moving it.
      } else assert(false);
    } else assert(false);

    return expr;
  }
}

void AssistRegion::dealias(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    LLVMValueRef expr) {
  auto sourceRnd = sourceRef->referend;

  if (sourceRef->ownership == Ownership::SHARE) {
    sharingDecrementStrongRc(from, globalState, functionState, blockState, builder, sourceRef, expr);
  } else if (sourceRef->ownership == Ownership::OWN) {
    if (auto interfaceRnd = dynamic_cast<InterfaceReferend*>(sourceRnd)) {
      // We can't discard owns, they must be destructured.
      assert(false); // impl
    } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
        dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
        dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
      // We can't discard owns, they must be destructured.
      assert(false);
    } else assert(false);
  } else if (sourceRef->ownership == Ownership::BORROW) {
    nonOwningDecrementStrongRc(from, globalState, functionState, builder, sourceRef, expr);
  } else if (sourceRef->ownership == Ownership::WEAK) {
    decrementWeakRc(from, globalState, functionState, builder, sourceRef, expr);
  } else assert(false);
}

LLVMValueRef AssistRegion::loadMember(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefM,
    LLVMValueRef structExpr,
    Mutability mutability,
    Reference* memberType,
    int memberIndex,
    const std::string& memberName) {
  if (mutability == Mutability::IMMUTABLE) {
    if (structRefM->location == Location::INLINE) {
      return LLVMBuildExtractValue(
          builder, structExpr, memberIndex, memberName.c_str());
    } else {
      LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder, structExpr);
      auto memberPtrLE =
          LLVMBuildStructGEP(
              builder, innerStructPtrLE, memberIndex, memberName.c_str());
      auto resultLE =
          LLVMBuildLoad(
              builder,
              memberPtrLE,
              memberName.c_str());
      auto targetOwnership = memberType->ownership == Ownership::SHARE ? Ownership::SHARE : Ownership::BORROW;
      Reference* targetType =
          globalState->metalCache.getReference(
              memberType->referend,
              memberType->location,
              targetOwnership);
      alias(from, globalState, functionState, builder, memberType, targetType, resultLE);
      return resultLE;
    }
  } else if (mutability == Mutability::MUTABLE) {
    LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder, structExpr);
    auto memberPtrLE =
        LLVMBuildStructGEP(
            builder, innerStructPtrLE, memberIndex, memberName.c_str());
    auto resultLE =
        LLVMBuildLoad(
            builder,
            memberPtrLE,
            memberName.c_str());
    auto targetOwnership = memberType->ownership == Ownership::SHARE ? Ownership::SHARE : Ownership::BORROW;
    Reference* targetType =
        globalState->metalCache.getReference(
            memberType->referend,
            memberType->location,
            targetOwnership);
    alias(from, globalState, functionState, builder, memberType, targetType, resultLE);
    return resultLE;
  } else {
    assert(false);
    return nullptr;
  }
}

std::vector<LLVMValueRef> AssistRegion::destructure(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* structType,
    LLVMValueRef structLE) {

  auto structReferend =
      dynamic_cast<StructReferend *>(structType->referend);
  assert(structReferend);

  auto structM = globalState->program->getStruct(structReferend->fullName);

  auto controlBlockPtrLE = getControlBlockPtr(builder, structLE, structType);

  if (structM->weakable) {
    auto wrciLE = getWrciFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockWrciMemberIndex);
    LLVMBuildCall(builder, globalState->markWrcDead, &wrciLE, 1, "");
  }

  std::vector<LLVMValueRef> membersLE =
      getMemberPtrsLE(globalState, functionState, builder, structM, structLE);

  if (structType->ownership == Ownership::OWN) {
    adjustStrongRc(
        AFL("Destroy decrementing the owning ref"),
        globalState, functionState, builder, structLE, structType, -1);
  } else if (structType->ownership == Ownership::SHARE) {
    // We dont decrement anything here, we're only here because we already hit zero.
  } else {
    assert(false);
  }

  checkStrongRcZero(
      AFL("Destroy freeing"),
      globalState,
      functionState,
      builder,
      controlBlockPtrLE,
      controlBlockRcMemberIndex);

  freeConcrete(
      globalState,
      functionState,
      builder,
      getControlBlockPtr(builder, structLE, structType),
      structType);

  return membersLE;
}

LLVMValueRef AssistRegion::storeMember(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* structRefM,
    LLVMValueRef structExpr,
    Mutability mutability,
    Reference* memberType,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef sourceLE) {

  assert(mutability == Mutability::MUTABLE);
  if (structRefM->ownership == Ownership::BORROW) {
    adjustCounter(builder, globalState->mutDerefCounter, 1);
  }

  auto oldMemberLE =
      swapMember(
          builder, structExpr, memberIndex, memberName, sourceLE);
  checkValidReference(from, globalState, functionState, builder, memberType, oldMemberLE);
  functionState->defaultRegion->dealias(
      AFL("MemberStore discard struct"), globalState, functionState, blockState, builder,
      structRefM, structExpr);

  return oldMemberLE;
}

void AssistRegion::destroyArray(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* arrayType,
    LLVMValueRef arrayWrapperLE) {
  if (arrayType->ownership == Ownership::OWN) {
    adjustStrongRc(
        AFL("Destroy decrementing the owning ref"),
        globalState, functionState, builder, arrayWrapperLE, arrayType, -1);
  } else if (arrayType->ownership == Ownership::SHARE) {
    // We dont decrement anything here, we're only here because we already hit zero.
  } else {
    assert(false);
  }

  checkStrongRcZero(
      AFL("Destroy freeing"),
      globalState,
      functionState,
      builder,
      arrayWrapperLE,
      controlBlockRcMemberIndex);

  freeConcrete(
      globalState,
      functionState,
      builder,
      getControlBlockPtr(builder, arrayWrapperLE, arrayType),
      arrayType);
}

LLVMTypeRef AssistRegion::getControlBlockStructForStruct(StructDefinition* structM) {
  if (structM->weakable) {
    return weakableControlBlockStructL;
  } else {
    return nonWeakableControlBlockStructL;
  }
}

LLVMTypeRef AssistRegion::getControlBlockStructForInterface(InterfaceDefinition* interfaceM) {
  if (interfaceM->weakable) {
    return weakableControlBlockStructL;
  } else {
    return nonWeakableControlBlockStructL;
  }
}

LLVMTypeRef AssistRegion::getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) {
  return nonWeakableControlBlockStructL;
}

LLVMValueRef AssistRegion::constructKnownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    KnownSizeArrayT* referendM,
    const std::vector<LLVMValueRef>& membersLE) {
  auto structLT = getKnownSizeArrayType(globalState, referendM);
  auto newStructLE = allocateStruct(globalState, builder, structTypeM, structLT);
  fillControlBlock(
      globalState,
      builder,
      getConcreteControlBlockPtr(builder, newStructLE),
      false,
      referendM->name->name);
  return newStructLE;
}


LLVMValueRef AssistRegion::getKnownSizeArrayElementsPtr(
    LLVMBuilderRef builder, LLVMValueRef knownSizeArrayWrapperPtrLE) {
  return LLVMBuildStructGEP(
      builder,
      knownSizeArrayWrapperPtrLE,
      1, // Array is after the control block.
      "ksaElemsPtr");
}

LLVMValueRef AssistRegion::getUnknownSizeArrayLength(
    LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) {
  auto resultLE =
      LLVMBuildStructGEP(
          builder,
          unknownSizeArrayWrapperPtrLE,
          1, // Length is after the control block and before contents.
          "usaLenPtr");
  assert(LLVMTypeOf(resultLE) == LLVMPointerType(LLVMInt64Type(), 0));
  return LLVMBuildLoad(builder, resultLE, "usaLen");
}

LLVMValueRef AssistRegion::getUnknownSizeArrayElementsPtr(
    LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) {
  return LLVMBuildStructGEP(
      builder,
      unknownSizeArrayWrapperPtrLE,
      2, // Array is after the control block and length.
      "usaElemsPtr");
}

LLVMValueRef AssistRegion::loadInnerArrayMember(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef elemsPtrLE,
    Reference* elementRefM,
    LLVMValueRef indexLE) {
  assert(LLVMGetTypeKind(LLVMTypeOf(elemsPtrLE)) == LLVMPointerTypeKind);
  LLVMValueRef indices[2] = {
      constI64LE(0),
      indexLE
  };
  auto resultLE =
      LLVMBuildLoad(
          builder,
          LLVMBuildGEP(
              builder, elemsPtrLE, indices, 2, "indexPtr"),
          "index");

  return resultLE;
}

LLVMValueRef AssistRegion::storeInnerArrayMember(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef elemsPtrLE,
    Reference* elementRefM,
    LLVMValueRef indexLE,
    LLVMValueRef sourceLE) {
  assert(LLVMGetTypeKind(LLVMTypeOf(elemsPtrLE)) == LLVMPointerTypeKind);
  LLVMValueRef indices[2] = {
      constI64LE(0),
      indexLE
  };
  auto resultLE =
      LLVMBuildStore(
          builder,
          sourceLE,
          LLVMBuildGEP(
              builder, elemsPtrLE, indices, 2, "indexPtr"));

  return resultLE;
}

LLVMValueRef AssistRegion::loadElement(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* structRefM,
    Reference* elementRefM,
    LLVMValueRef sizeLE,
    LLVMValueRef arrayPtrLE,
    Mutability mutability,
    LLVMValueRef indexLE) {

  auto isNonNegativeLE = LLVMBuildICmp(builder, LLVMIntSGE, indexLE, constI64LE(0), "isNonNegative");
  auto isUnderLength = LLVMBuildICmp(builder, LLVMIntSLT, indexLE, sizeLE, "isUnderLength");
  auto isWithinBounds = LLVMBuildAnd(builder, isNonNegativeLE, isUnderLength, "isWithinBounds");
  buildAssert(AFL("Bounds check"), globalState, functionState, builder, isWithinBounds, "Index out of bounds!");

  if (mutability == Mutability::IMMUTABLE) {
    if (structRefM->location == Location::INLINE) {
      assert(false);
//      return LLVMBuildExtractValue(builder, structExpr, indexLE, "index");
      return nullptr;
    } else {
      return loadInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE);
    }
  } else if (mutability == Mutability::MUTABLE) {
    return loadInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE);
  } else {
    assert(false);
    return nullptr;
  }
}


LLVMValueRef AssistRegion::storeElement(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* arrayRefM,
    Reference* elementRefM,
    LLVMValueRef sizeLE,
    LLVMValueRef arrayPtrLE,
    Mutability mutability,
    LLVMValueRef indexLE,
    LLVMValueRef sourceLE) {

  auto isNonNegativeLE = LLVMBuildICmp(builder, LLVMIntSGE, indexLE, constI64LE(0), "isNonNegative");
  auto isUnderLength = LLVMBuildICmp(builder, LLVMIntSLT, indexLE, sizeLE, "isUnderLength");
  auto isWithinBounds = LLVMBuildAnd(builder, isNonNegativeLE, isUnderLength, "isWithinBounds");
  buildAssert(AFL("Bounds check"), globalState, functionState, builder, isWithinBounds, "Index out of bounds!");

  if (mutability == Mutability::IMMUTABLE) {
    if (arrayRefM->location == Location::INLINE) {
      assert(false);
//      return LLVMBuildExtractValue(builder, structExpr, indexLE, "index");
      return nullptr;
    } else {
      return storeInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE, sourceLE);
    }
  } else if (mutability == Mutability::MUTABLE) {
    return storeInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE, sourceLE);
  } else {
    assert(false);
    return nullptr;
  }
}


void AssistRegion::nonOwningDecrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef expr) {
  auto sourceRnd = refM->referend;

  if (dynamic_cast<Int*>(sourceRnd) ||
      dynamic_cast<Bool*>(sourceRnd) ||
      dynamic_cast<Float*>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (dynamic_cast<InterfaceReferend*>(sourceRnd)) {
    if (refM->location == Location::INLINE) {
      assert(false); // impl
    } else {
      adjustStrongRc(from, globalState, functionState, builder, expr, refM, -1);
    }
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    if (refM->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      adjustStrongRc(from, globalState, functionState, builder, expr, refM, -1);
    }
  } else {
    std::cerr << "Unimplemented type in incrementStrongRc: "
        << typeid(*refM->referend).name() << std::endl;
    assert(false);
  }
}

void AssistRegion::sharingDecrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    LLVMValueRef expr) {
  auto sourceRnd = sourceRef->referend;

  if (dynamic_cast<Int*>(sourceRnd) ||
      dynamic_cast<Bool*>(sourceRnd) ||
      dynamic_cast<Float*>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend*>(sourceRnd)) {
    if (sourceRef->location == Location::INLINE) {
      assert(false); // impl
    } else {
      auto rcLE = adjustStrongRc(from, globalState, functionState, builder, expr, sourceRef, -1);
      buildIf(
          functionState,
          builder,
          isZeroLE(builder, rcLE),
          [globalState, functionState, expr, interfaceRnd, sourceRef](LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceRef->referend);

            auto interfaceM = globalState->program->getInterface(interfaceRnd->fullName);
            int indexInEdge = -1;
            for (int i = 0; i < interfaceM->methods.size(); i++) {
              if (interfaceM->methods[i]->prototype == immDestructor) {
                indexInEdge = i;
              }
            }
            assert(indexInEdge >= 0);

            std::vector<LLVMValueRef> argExprsL = { expr };
            buildInterfaceCall(functionState->defaultRegion, thenBuilder, argExprsL, 0, indexInEdge);
          });
    }
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    if (sourceRef->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      auto rcLE = adjustStrongRc(from, globalState, functionState, builder, expr, sourceRef, -1);
      buildIf(
          functionState,
          builder,
          isZeroLE(builder, rcLE),
          [from, globalState, functionState, expr, sourceRef](LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceRef->referend);
            auto funcL = globalState->getFunction(immDestructor->name);
            std::vector<LLVMValueRef> argExprsL = { expr };
            return LLVMBuildCall(thenBuilder, funcL, argExprsL.data(), argExprsL.size(), "");
          });
    }
  } else if (dynamic_cast<Str*>(sourceRnd)) {
    auto rcLE = adjustStrongRc(from, globalState, functionState, builder, expr, sourceRef, -1);
    auto controlBlockPtrLE = getControlBlockPtr(builder, expr, sourceRef);
    buildIf(
        functionState,
        builder,
        isZeroLE(builder, rcLE),
        [this, from, globalState, functionState, blockState, controlBlockPtrLE, sourceRef](LLVMBuilderRef thenBuilder) {
          freeConcrete(globalState, functionState, thenBuilder, controlBlockPtrLE, sourceRef);
        });
  } else {
    std::cerr << "Unimplemented type in discard: "
        << typeid(*sourceRef->referend).name() << std::endl;
    assert(false);
  }
}

void AssistRegion::incrementWeakRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef expr) {
  auto sourceRnd = refM->referend;

  if (dynamic_cast<InterfaceReferend*>(sourceRnd)) {
    assert(false);
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    auto structReferend = dynamic_cast<StructReferend*>(sourceRnd);
    assert(structReferend);
    auto wrciLE = getWrciFromWeakRef(builder, expr);
    LLVMBuildCall(builder, globalState->incrementWrc, &wrciLE, 1, "");
  } else assert(false);
}


void AssistRegion::decrementWeakRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef expr) {
  auto sourceRnd = refM->referend;

  if (auto interfaceRnd = dynamic_cast<InterfaceReferend*>(sourceRnd)) {
    assert(false);
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    auto structReferend = dynamic_cast<StructReferend*>(sourceRnd);
    assert(structReferend);
    auto wrciLE = getWrciFromWeakRef(builder, expr);
    LLVMBuildCall(builder, globalState->decrementWrc, &wrciLE, 1, "");
  } else assert(false);
}

void AssistRegion::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef refLE) {
  auto expectedTypeLT = translateType(globalState, refM);
  assert(LLVMTypeOf(refLE) == expectedTypeLT);

  if (dynamic_cast<Str*>(refM->referend)) {
    assert(LLVMTypeOf(refLE) == immMixin.getStringRefType());
  }

  censusCheckValid(
      checkerAFL, globalState, functionState, builder, refM, refLE,
    [this](LLVMBuilderRef builder, Reference* refM, LLVMValueRef refLE) {
      return getControlBlockPtr(builder, refLE, refM);
    });
}


LLVMValueRef AssistRegion::constructString(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  return immMixin.constructString(globalState, functionState, builder, lengthLE);
}

LLVMTypeRef AssistRegion::getStringRefType() const {
  return immMixin.getStringRefType();
}

LLVMValueRef AssistRegion::getStringControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef stringRefLE) {
  return immMixin.getStringControlBlockPtr(builder, stringRefLE);
}

LLVMValueRef AssistRegion::getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
  return immMixin.getStringBytesPtr(builder, stringRefLE);
}

LLVMValueRef AssistRegion::getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
  return immMixin.getStringLength(builder, stringRefLE);
}

LLVMValueRef AssistRegion::getConcreteRefFromInterfaceRef(
    LLVMBuilderRef builder, LLVMValueRef refLE) {
  return LLVMBuildPointerCast(
      builder,
      getInterfaceControlBlockPtr(builder, refLE),
  LLVMPointerType(LLVMVoidType(), 0),
  "objAsVoidPtr");
}

LLVMValueRef makeInterfaceRef(
    LLVMBuilderRef builder,
    Reference* sourceStructTypeM,
    LLVMTypeRef interfaceRefStructLT,
    LLVMValueRef controlBlockPtrLE,
    LLVMValueRef itablePtrLE) {
  assert(sourceStructTypeM->location != Location::INLINE);

  auto interfaceRefLE = LLVMGetUndef(interfaceRefStructLT);
  interfaceRefLE =
      LLVMBuildInsertValue(
          builder,
          interfaceRefLE,
          controlBlockPtrLE,
          0,
          "interfaceRefWithOnlyObj");
  interfaceRefLE =
      LLVMBuildInsertValue(
          builder,
          interfaceRefLE,
          itablePtrLE,
          1,
          "interfaceRef");

  return interfaceRefLE;
}

LLVMValueRef AssistRegion::upcast2(
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
      getInterfaceRefStruct(
          targetInterfaceReferendM->fullName);
  auto resultLE =
      makeInterfaceRef(
          builder, sourceStructTypeM, interfaceRefLT,
          getControlBlockPtr(builder, sourceStructLE, sourceStructTypeM),
          getInterfaceTablePtr(
              globalState->program->getStruct(sourceStructReferendM->fullName)
                  ->getEdgeForInterface(targetInterfaceReferendM->fullName)));
  checkValidReference(
      FL(), globalState, functionState, builder, targetInterfaceTypeM, resultLE);
  return resultLE;
}


LLVMValueRef AssistRegion::lockWeak(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* constraintRefTypeM,
    bool thenResultIsNever,
    bool elseResultIsNever,
    LLVMTypeRef resultOptTypeL,
    LLVMValueRef sourceWeakRefLE,
    std::function<LLVMValueRef(LLVMBuilderRef, LLVMValueRef)> buildThen,
    std::function<LLVMValueRef(LLVMBuilderRef)> buildElse) {

  auto isAliveLE = getIsAliveFromWeakRef(globalState, builder, sourceWeakRefLE);

  adjustCounter(builder, globalState->mutDerefCounter, 1);

  return buildIfElse(functionState, builder, isAliveLE, resultOptTypeL, thenResultIsNever, elseResultIsNever,
      [this, globalState, functionState, constraintRefTypeM, sourceWeakRefLE, buildThen](LLVMBuilderRef thenBuilder) {
        auto objPtrLE =
            getConstraintRefFromWeakRef(
                globalState,
                functionState,
                thenBuilder,
                sourceWeakRefLE,
                constraintRefTypeM);
        return buildThen(thenBuilder, objPtrLE);
      },
      [buildElse](LLVMBuilderRef elseBuilder) {
        return buildElse(elseBuilder);
      });
}

LLVMTypeRef AssistRegion::getStructRefType(
    GlobalState* globalState,
    Reference* referenceM,
    StructReferend* structReferendM) {
  auto structM = globalState->program->getStruct(structReferendM->fullName);
  if (structM->mutability == Mutability::MUTABLE) {
    auto countedStructL = getCountedStruct(structReferendM->fullName);
    if (referenceM->ownership == Ownership::OWN) {
      return LLVMPointerType(countedStructL, 0);
    } else if (referenceM->ownership == Ownership::BORROW) {
      return LLVMPointerType(countedStructL, 0);
    } else if (referenceM->ownership == Ownership::WEAK) {
      return getStructWeakRefStruct(structM->name);
    } else {
      assert(false);
    }
  } else {
    auto innerStructL = getInnerStruct(structReferendM->fullName);
    if (referenceM->location == Location::INLINE) {
      return getInnerStruct(structReferendM->fullName);
    } else {
      auto countedStructL = getCountedStruct(structReferendM->fullName);
      return LLVMPointerType(countedStructL, 0);
    }
  }
}

void AssistRegion::declareStruct(
    GlobalState* globalState,
    StructDefinition* structM) {
  auto innerStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), structM->name->name.c_str());
  assert(innerStructs.count(structM->name->name) == 0);
  innerStructs.emplace(structM->name->name, innerStructL);

  auto countedStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (structM->name->name + "rc").c_str());
  assert(countedStructs.count(structM->name->name) == 0);
  countedStructs.emplace(structM->name->name, countedStructL);

  auto structWeakRefStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (structM->name->name + "w").c_str());
  assert(structWeakRefStructs.count(structM->name->name) == 0);
  structWeakRefStructs.emplace(structM->name->name, structWeakRefStructL);
}

void AssistRegion::translateStruct(
    GlobalState* globalState,
    StructDefinition* structM) {

  LLVMTypeRef contentsStructL = getInnerStruct(structM->name);
  translateContentsStruct(globalState, this, structM, contentsStructL);

  LLVMTypeRef wrapperStructL = getCountedStruct(structM->name);
  translateWrapperStruct(
      globalState,
      structM,
      wrapperStructL,
      contentsStructL,
      getControlBlockStructForStruct(structM));

  auto structWeakRefStructL = getStructWeakRefStruct(structM->name);
  std::vector<LLVMTypeRef> structWeakRefStructMemberTypesL;
  structWeakRefStructMemberTypesL.push_back(LLVMInt64Type());
  structWeakRefStructMemberTypesL.push_back(LLVMPointerType(wrapperStructL, 0));
  LLVMStructSetBody(structWeakRefStructL, structWeakRefStructMemberTypesL.data(), structWeakRefStructMemberTypesL.size(), false);
}


LLVMTypeRef AssistRegion::translateInterfaceMethodToFunctionType(
    GlobalState* globalState,
    InterfaceMethod* method) {
  auto returnLT = translateType(globalState, method->prototype->returnType);
  auto paramsLT = translateTypes(globalState, this, method->prototype->params);
  paramsLT[method->virtualParamIndex] = LLVMPointerType(LLVMVoidType(), 0);
  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
}

void AssistRegion::translateInterface(
    GlobalState* globalState,
    InterfaceDefinition* interfaceM) {
  LLVMTypeRef itableStruct = getInterfaceTableStruct(interfaceM->name);
  std::vector<LLVMTypeRef> interfaceMethodTypesL;
  for (int i = 0; i < interfaceM->methods.size(); i++) {
    interfaceMethodTypesL.push_back(
        LLVMPointerType(
            translateInterfaceMethodToFunctionType(
                globalState, interfaceM->methods[i]),
            0));
  }
  LLVMStructSetBody(
      itableStruct, interfaceMethodTypesL.data(), interfaceMethodTypesL.size(), false);

  LLVMTypeRef refStructL = getInterfaceRefStruct(interfaceM->name);
  std::vector<LLVMTypeRef> refStructMemberTypesL;
  // The object ptr is the 0th element, so we don't have to add and subtract 1
  // whenever we want to affect its ref count.
  // It points to the any struct, which is a wrapper around a ref count.
  // It makes it easier to increment and decrement ref counts.
  refStructMemberTypesL.push_back(LLVMPointerType(getControlBlockStructForInterface(interfaceM), 0));
  refStructMemberTypesL.push_back(LLVMPointerType(itableStruct, 0));
  LLVMStructSetBody(
      refStructL,
      refStructMemberTypesL.data(),
      refStructMemberTypesL.size(),
      false);

  auto interfaceWeakRefStructL = getInterfaceWeakRefStruct(interfaceM->name);
  std::vector<LLVMTypeRef> interfaceWeakRefStructMemberTypesL;
  interfaceWeakRefStructMemberTypesL.push_back(LLVMPointerType(LLVMInt64Type(), 0));
  interfaceWeakRefStructMemberTypesL.push_back(refStructL);
  LLVMStructSetBody(interfaceWeakRefStructL, interfaceWeakRefStructMemberTypesL.data(), interfaceWeakRefStructMemberTypesL.size(), false);
}


LLVMTypeRef AssistRegion::translateType(GlobalState* globalState, Reference* referenceM) {
  if (dynamic_cast<Int*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMInt64Type();
  } else if (dynamic_cast<Bool*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMInt1Type();
  } else if (dynamic_cast<Str*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return getStringRefType();
  } else if (dynamic_cast<Never*>(referenceM->referend) != nullptr) {
    return LLVMArrayType(LLVMIntType(NEVER_INT_BITS), 0);
  } else if (auto knownSizeArrayMT =
      dynamic_cast<KnownSizeArrayT*>(referenceM->referend)) {
    return getKnownSizeArrayRefType(globalState, referenceM, knownSizeArrayMT);
  } else if (auto unknownSizeArrayMT =
      dynamic_cast<UnknownSizeArrayT*>(referenceM->referend)) {
    auto knownSizeArrayCountedStructLT =
        getUnknownSizeArrayRefType(globalState, referenceM, unknownSizeArrayMT);
    return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
  } else if (auto structReferend =
      dynamic_cast<StructReferend*>(referenceM->referend)) {
    return getStructRefType(globalState, referenceM, structReferend);
  } else if (auto interfaceReferend =
      dynamic_cast<InterfaceReferend*>(referenceM->referend)) {
    auto interfaceM = globalState->program->getInterface(interfaceReferend->fullName);
    auto interfaceRefStructL =
        getInterfaceRefStruct(interfaceReferend->fullName);
    if (interfaceM->mutability == Mutability::MUTABLE) {
      if (referenceM->ownership == Ownership::OWN) {
        return interfaceRefStructL;
      } else if (referenceM->ownership == Ownership::BORROW) {
        return interfaceRefStructL;
      } else if (referenceM->ownership == Ownership::WEAK) {
        return getInterfaceWeakRefStruct(interfaceM->name);
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

void AssistRegion::declareEdge(
    GlobalState* globalState,
    Edge* edge) {

  auto interfaceTableStructL =
      getInterfaceTableStruct(edge->interfaceName->fullName);

  auto edgeName =
      edge->structName->fullName->name + edge->interfaceName->fullName->name;
  auto itablePtr =
      LLVMAddGlobal(globalState->mod, interfaceTableStructL, edgeName.c_str());
  LLVMSetLinkage(itablePtr, LLVMExternalLinkage);

  interfaceTablePtrs.emplace(edge, itablePtr);
}

void AssistRegion::translateEdge(
    GlobalState* globalState,
    Edge* edge) {

  auto interfaceTableStructL =
      getInterfaceTableStruct(edge->interfaceName->fullName);

  auto builder = LLVMCreateBuilder();
  auto itableLE = LLVMGetUndef(interfaceTableStructL);
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    itableLE = LLVMBuildInsertValue(
        builder,
        itableLE,
        globalState->getFunction(funcName),
        i,
        std::to_string(i).c_str());
  }
  LLVMDisposeBuilder(builder);

  auto itablePtr = getInterfaceTablePtr(edge);
  LLVMSetInitializer(itablePtr,  itableLE);
}


void AssistRegion::declareInterface(
    GlobalState* globalState,
    InterfaceDefinition* interfaceM) {

  auto interfaceRefStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), interfaceM->name->name.c_str());
  assert(interfaceRefStructs.count(interfaceM->name->name) == 0);
  interfaceRefStructs.emplace(interfaceM->name->name, interfaceRefStructL);

  auto interfaceTableStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (interfaceM->name->name + "itable").c_str());
  assert(interfaceTableStructs.count(interfaceM->name->name) == 0);
  interfaceTableStructs.emplace(interfaceM->name->name, interfaceTableStructL);

  auto interfaceWeakRefStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (interfaceM->name->name + "w").c_str());
  assert(interfaceWeakRefStructs.count(interfaceM->name->name) == 0);
  interfaceWeakRefStructs.emplace(interfaceM->name->name, interfaceWeakRefStructL);
}
