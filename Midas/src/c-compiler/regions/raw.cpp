#include <translatetype.h>
#include <regions/shared/struct.h>
#include <regions/shared/census.h>
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/members.h"
#include "function/expressions/shared/branch.h"
#include "regions/shared/heap.h"
#include "function/expressions/shared/controlblock.h"
#include "raw.h"

constexpr int mutControlBlockTypeStrIndex = 0;
constexpr int mutControlBlockObjIdIndex = 1;

constexpr int mutWeakableControlBlockWrciMemberIndex = 2;

//LLVMValueRef allocateStruct(
//    GlobalState* globalState,
//    LLVMBuilderRef builder,
//    Reference* structTypeM,
//    LLVMTypeRef structL) {
//  adjustCounter(builder, globalState->liveHeapObjCounter, 1);
//
//  LLVMValueRef resultPtrLE = nullptr;
//  if (structTypeM->location == Location::INLINE) {
//    resultPtrLE = LLVMBuildAlloca(builder, structL, "newstruct");
//  } else if (structTypeM->location == Location::YONDER) {
//    size_t sizeBytes = LLVMABISizeOfType(globalState->dataLayout, structL);
//    LLVMValueRef sizeLE = LLVMConstInt(LLVMInt64Type(), sizeBytes, false);
//
//    auto newStructLE =
//        LLVMBuildCall(builder, globalState->malloc, &sizeLE, 1, "");
//
//    resultPtrLE =
//        LLVMBuildBitCast(
//            builder, newStructLE, LLVMPointerType(structL, 0), "newstruct");
//  } else {
//    assert(false);
//    return nullptr;
//  }
//
//  if (globalState->opt->census) {
//    LLVMValueRef resultAsVoidPtrLE =
//        LLVMBuildBitCast(
//            builder, resultPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
//    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
//  }
//  return resultPtrLE;
//}
//
//LLVMValueRef mallocUnknownSizeArray(
//    GlobalState* globalState,
//    LLVMBuilderRef builder,
//    LLVMTypeRef usaWrapperLT,
//    LLVMTypeRef usaElementLT,
//    LLVMValueRef lengthLE) {
//  auto sizeBytesLE =
//      LLVMBuildAdd(
//          builder,
//          constI64LE(LLVMABISizeOfType(globalState->dataLayout, usaWrapperLT)),
//          LLVMBuildMul(
//              builder,
//              constI64LE(LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(usaElementLT, 1))),
//              lengthLE,
//              ""),
//          "usaMallocSizeBytes");
//
//  auto newWrapperPtrLE =
//      LLVMBuildCall(builder, globalState->malloc, &sizeBytesLE, 1, "");
//
//  adjustCounter(builder, globalState->liveHeapObjCounter, 1);
//
//  if (globalState->opt->census) {
//    LLVMValueRef resultAsVoidPtrLE =
//        LLVMBuildBitCast(
//            builder, newWrapperPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
//    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
//  }
//
//  return LLVMBuildBitCast(
//      builder,
//      newWrapperPtrLE,
//      LLVMPointerType(usaWrapperLT, 0),
//      "newstruct");
//}
//
//

constexpr int WEAK_REF_RCINDEX_MEMBER_INDEX = 0;
constexpr int WEAK_REF_OBJPTR_MEMBER_INDEX = 1;

//LLVMTypeRef RawRegion::makeInnerKnownSizeArrayLT(GlobalState* globalState, KnownSizeArrayT* knownSizeArrayMT) {
//  auto elementLT = translateType(globalState, this, knownSizeArrayMT->rawArray->elementType);
//  return LLVMArrayType(elementLT, knownSizeArrayMT->size);
//}

// This gives the actual struct, *not* a pointer to a struct, which you sometimes
// might need instead. For that, use translateType.
//LLVMTypeRef RawRegion::translateKnownSizeArrayToWrapperStruct(
//    GlobalState* globalState,
//    KnownSizeArrayT* knownSizeArrayMT) {
//  auto innerArrayLT = makeInnerKnownSizeArrayLT(globalState, knownSizeArrayMT);
//
//  auto iter = globalState->knownSizeArrayCountedStructs.find(knownSizeArrayMT->name);
//  if (iter == globalState->knownSizeArrayCountedStructs.end()) {
//    auto countedStruct = LLVMStructCreateNamed(LLVMGetGlobalContext(), knownSizeArrayMT->name->name.c_str());
//    std::vector<LLVMTypeRef> elementsL;
//    elementsL.push_back(nonWeakableControlBlockStructL);
//    elementsL.push_back(innerArrayLT);
//    LLVMStructSetBody(countedStruct, elementsL.data(), elementsL.size(), false);
//
//    iter = globalState->knownSizeArrayCountedStructs.emplace(knownSizeArrayMT->name, countedStruct).first;
//  }
//
//  return iter->second;
//}
//
//LLVMTypeRef RawRegion::makeInnerUnknownSizeArrayLT(GlobalState* globalState, UnknownSizeArrayT* unknownSizeArrayMT) {
//  auto elementLT = translateType(globalState, this, unknownSizeArrayMT->rawArray->elementType);
//  return LLVMArrayType(elementLT, 0);
//}
//
//LLVMTypeRef RawRegion::getKnownSizeArrayType(
//    GlobalState* globalState,
//    KnownSizeArrayT* knownSizeArrayMT) {
//  if (knownSizeArrayMT->rawArray->mutability == Mutability::MUTABLE) {
//    assert(false);
//    return nullptr;
//  } else {
//    auto innerArrayLT = makeInnerKnownSizeArrayLT(globalState, knownSizeArrayMT);
//    auto knownSizeArrayCountedStructLT =
//        translateKnownSizeArrayToWrapperStruct(
//            globalState, knownSizeArrayMT);
//    return knownSizeArrayCountedStructLT;
//  }
//}

LLVMTypeRef RawRegion::getKnownSizeArrayRefType(
    GlobalState* globalState,
    Reference* referenceM,
    KnownSizeArrayT* knownSizeArrayMT) {
//  if (knownSizeArrayMT->rawArray->mutability == Mutability::MUTABLE) {
//    assert(false);
//    return nullptr;
//  } else {
//    auto innerArrayLT = makeInnerKnownSizeArrayLT(globalState,
//        knownSizeArrayMT);
//    if (referenceM->location == Location::INLINE) {
//      return innerArrayLT;
//    } else {
//      auto knownSizeArrayCountedStructLT =
//          translateKnownSizeArrayToWrapperStruct(
//              globalState, knownSizeArrayMT);
//
//      return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
//    }
//  }
  assert(false);
}

LLVMTypeRef RawRegion::getUnknownSizeArrayRefType(
    GlobalState* globalState,
    Reference* referenceM,
    UnknownSizeArrayT* unknownSizeArrayMT) {
//  auto innerArrayLT = makeInnerUnknownSizeArrayLT(globalState, unknownSizeArrayMT);
//
//  auto iter = globalState->unknownSizeArrayCountedStructs.find(unknownSizeArrayMT->name);
//  if (iter == globalState->unknownSizeArrayCountedStructs.end()) {
//    auto countedStruct = LLVMStructCreateNamed(LLVMGetGlobalContext(), (unknownSizeArrayMT->name->name + "rc").c_str());
//    std::vector<LLVMTypeRef> elementsL;
//    elementsL.push_back(getControlBlockStructForUnknownSizeArray(unknownSizeArrayMT));
//    elementsL.push_back(LLVMInt64Type());
//    elementsL.push_back(innerArrayLT);
//    LLVMStructSetBody(countedStruct, elementsL.data(), elementsL.size(), false);
//
//    iter = globalState->unknownSizeArrayCountedStructs.emplace(unknownSizeArrayMT->name, countedStruct).first;
//  }
//
//  return iter->second;
  assert(false);
}

LLVMValueRef RawRegion::getControlBlockPtr(
    LLVMBuilderRef builder,
    // This will be a pointer if a mutable struct, or a fat ref if an interface.
    LLVMValueRef referenceLE,
    Reference* refM) {
  if (refM->ownership == Ownership::SHARE) {
    return immMixin.getControlBlockPtr(builder, referenceLE, refM);
  } else {
    if (dynamic_cast<InterfaceReferend *>(refM->referend)) {
      return getMutInterfaceControlBlockPtr(builder, referenceLE);
    } else if (dynamic_cast<StructReferend *>(refM->referend)) {
      return getMutConcreteControlBlockPtr(builder, referenceLE);
    } else if (dynamic_cast<KnownSizeArrayT *>(refM->referend)) {
      return getMutConcreteControlBlockPtr(builder, referenceLE);
    } else if (dynamic_cast<UnknownSizeArrayT *>(refM->referend)) {
      return getMutConcreteControlBlockPtr(builder, referenceLE);
    } else {
      std::cerr << "Unknown: " << typeid(*refM->referend).name() << std::endl;
      assert(false);
      return nullptr;
    }
  }
}

LLVMValueRef RawRegion::constructUnknownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMTypeRef usaWrapperPtrLT,
    LLVMTypeRef usaElementLT,
    LLVMValueRef sizeLE,
    const std::string& typeName) {
//  auto usaWrapperPtrLE =
//      mallocUnknownSizeArray(
//          globalState, builder, usaWrapperPtrLT, usaElementLT, sizeLE);
//  fillControlBlock(
//      globalState,
//      builder,
//      getConcreteControlBlockPtr(builder, usaWrapperPtrLE),
//      false,
//      typeName);
//  return usaWrapperPtrLE;
  assert(false);
}

LLVMValueRef RawRegion::getMutConcreteControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef concretePtrLE) {
  // Control block is always the 0th element of every concrete struct.
  return LLVMBuildStructGEP(builder, concretePtrLE, 0, "controlPtr");
}

LLVMValueRef RawRegion::getMutInterfaceControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef interfaceRefLE) {
  // Interface fat pointer's first element points directly at the control block,
  // and we dont have to cast it. We would have to cast if we were accessing the
  // actual object though.
  return LLVMBuildExtractValue(builder, interfaceRefLE, 0, "controlPtr");
}

//LLVMValueRef RawRegion::getWrciFromWeakRef(
//    LLVMBuilderRef builder,
//    LLVMValueRef weakRefLE) {
//  return LLVMBuildExtractValue(builder, weakRefLE, WEAK_REF_RCINDEX_MEMBER_INDEX, "wrci");
//}
//
//LLVMValueRef RawRegion::getIsAliveFromWeakRef(
//    GlobalState* globalState,
//    LLVMBuilderRef builder,
//    LLVMValueRef weakRefLE) {
//  auto wrciLE = getWrciFromWeakRef(builder, weakRefLE);
//  return LLVMBuildCall(builder, globalState->wrcIsLive, &wrciLE, 1, "isAlive");
//}
//
//LLVMValueRef RawRegion::getConstraintRefFromWeakRef(
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    LLVMValueRef weakRefLE,
//    Reference* constraintRefM) {
//  auto refLE = LLVMBuildExtractValue(builder, weakRefLE, WEAK_REF_OBJPTR_MEMBER_INDEX, "");
//  checkValidReference(FL(), globalState, functionState, builder, constraintRefM, refLE);
//
//  Reference* targetType =
//      globalState->metalCache.getReference(
//          constraintRefM->referend,
//          constraintRefM->location,
//          Ownership::BORROW);
//  alias(FL(), globalState, functionState, builder, constraintRefM, targetType, refLE);
//  return refLE;
//}
//
//void RawRegion::fillInnerStruct(
//    LLVMBuilderRef builder,
//    StructDefinition* structM,
//    std::vector<LLVMValueRef> membersLE,
//    LLVMValueRef innerStructPtrLE) {
//  for (int i = 0; i < membersLE.size(); i++) {
//    auto memberName = structM->members[i]->name;
//    auto ptrLE =
//        LLVMBuildStructGEP(builder, innerStructPtrLE, i, memberName.c_str());
//    LLVMBuildStore(builder, membersLE[i], ptrLE);
//  }
//}
//
//LLVMValueRef RawRegion::constructCountedStruct(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    LLVMTypeRef structL,
//    Reference* structTypeM,
//    StructDefinition* structM,
//    std::vector<LLVMValueRef> membersLE) {
//  LLVMValueRef newStructPtrLE = allocateStruct(globalState, builder, structTypeM, structL);
//  auto objIdLE =
//      fillControlBlock(
//          globalState,
//          builder,
//          getConcreteControlBlockPtr(builder, newStructPtrLE),
//          structM->weakable,
//          structM->name->name);
//  fillInnerStruct(
//      builder, structM, membersLE,
//      getStructContentsPtr(builder, newStructPtrLE));
//  buildFlare(from, globalState, functionState, builder, "Allocating ", structM->name->name, objIdLE);
//  return newStructPtrLE;
//}
//
//LLVMValueRef RawRegion::constructInnerStruct(
//    LLVMBuilderRef builder,
//    StructDefinition* structM,
//    LLVMTypeRef valStructL,
//    const std::vector<LLVMValueRef>& membersLE) {
//
//  // We always start with an undef, and then fill in its fields one at a
//  // time.
//  LLVMValueRef structValueBeingInitialized = LLVMGetUndef(valStructL);
//  for (int i = 0; i < membersLE.size(); i++) {
//    auto memberName = structM->members[i]->name;
//    // Every time we fill in a field, it actually makes a new entire
//    // struct value, and gives us a LLVMValueRef for the new value.
//    // So, `structValueBeingInitialized` contains the latest one.
//    structValueBeingInitialized =
//        LLVMBuildInsertValue(
//            builder,
//            structValueBeingInitialized,
//            membersLE[i],
//            i,
//            memberName.c_str());
//  }
//  return structValueBeingInitialized;
//}
//
//// Returns object ID
//LLVMValueRef RawRegion::fillControlBlock(
//    GlobalState* globalState,
//    LLVMBuilderRef builder,
//    LLVMValueRef controlBlockPtrLE,
//    bool weakable,
//    const std::string& typeName) {
//
//  auto objIdLE = adjustCounter(builder, globalState->objIdCounter, 1);
//
//  LLVMValueRef newControlBlockLE = nullptr;
//  if (weakable) {
//    newControlBlockLE = LLVMGetUndef(weakableControlBlockStructL);
//  } else {
//    newControlBlockLE = LLVMGetUndef(nonWeakableControlBlockStructL);
//  }
//  newControlBlockLE =
//      LLVMBuildInsertValue(
//          builder,
//          newControlBlockLE,
//          // Start at 1, 0 would mean it's dead.
//          LLVMConstInt(LLVMInt64Type(), 1, false),
//          controlBlockRcMemberIndex,
//          "controlBlockWithRc");
//  newControlBlockLE =
//      LLVMBuildInsertValue(
//          builder,
//          newControlBlockLE,
//          objIdLE,
//          mutControlBlockObjIdIndex,
//          "controlBlockWithRcAndObjId");
//  newControlBlockLE =
//      LLVMBuildInsertValue(
//          builder,
//          newControlBlockLE,
//          globalState->getOrMakeStringConstant(typeName),
//          mutControlBlockTypeStrIndex,
//          "controlBlockComplete");
//  if (weakable) {
//    auto wrciLE = LLVMBuildCall(builder, globalState->allocWrc, nullptr, 0, "");
//    newControlBlockLE =
//        LLVMBuildInsertValue(
//            builder,
//            newControlBlockLE,
//            wrciLE,
//            controlBlockWrciMemberIndex,
//            "controlBlockComplete");
//  }
//  LLVMBuildStore(
//      builder,
//      newControlBlockLE,
//      controlBlockPtrLE);
//  return objIdLE;
//}

LLVMTypeRef makeMutWeakableControlBlockStruct() {
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

  assert(mutControlBlockTypeStrIndex == memberTypesL.size());
  memberTypesL.push_back(int8PtrLT);

  assert(mutControlBlockObjIdIndex == memberTypesL.size());
  memberTypesL.push_back(int64LT);

  assert(mutWeakableControlBlockWrciMemberIndex == memberTypesL.size());
  memberTypesL.push_back(int64LT);

  LLVMStructSetBody(
      controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
  return controlBlockStructL;
}

LLVMTypeRef makeMutNonWeakableControlBlockStruct() {
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

  assert(memberTypesL.size() == mutControlBlockTypeStrIndex); // should match weakable
  memberTypesL.push_back(int8PtrLT);

  assert(memberTypesL.size() == mutControlBlockObjIdIndex); // should match weakable
  memberTypesL.push_back(int64LT);

  LLVMStructSetBody(
      controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
  return controlBlockStructL;
}

RawRegion::RawRegion() :
    immMixin(),
    weakableControlBlockStructL(makeMutWeakableControlBlockStruct()),
    nonWeakableControlBlockStructL(makeMutNonWeakableControlBlockStruct()) {
}

LLVMValueRef RawRegion::allocate(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* desiredReference,
    const std::vector<LLVMValueRef>& membersLE) {
//  auto structReferend =
//      dynamic_cast<StructReferend*>(desiredReference->referend);
//  assert(structReferend);
//
//  auto structM = globalState->program->getStruct(structReferend->fullName);
//
//  switch (structM->mutability) {
//    case Mutability::MUTABLE: {
//      auto countedStructL = globalState->getCountedStruct(structReferend->fullName);
//      return constructCountedStruct(
//          from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
//    }
//    case Mutability::IMMUTABLE: {
//      if (desiredReference->location == Location::INLINE) {
//        auto valStructL =
//            globalState->getInnerStruct(structReferend->fullName);
//        return constructInnerStruct(
//            builder, structM, valStructL, membersLE);
//      } else {
//        auto countedStructL =
//            globalState->getCountedStruct(structReferend->fullName);
//        return constructCountedStruct(
//            from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
//      }
//    }
//    default:
//      assert(false);
//      return nullptr;
//  }
//  assert(false);
  assert(false);
}


LLVMValueRef RawRegion::castOwnership(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* sourceType,
    Ownership targetOwnership,
    LLVMValueRef sourceRefLE) {
  if (sourceType->ownership == targetOwnership) {
    return sourceRefLE;
  } else if (sourceType->ownership == Ownership::OWN && targetOwnership == Ownership::WEAK) {
    if (auto structReferend = dynamic_cast<StructReferend*>(sourceType->referend)) {
      auto controlBlockPtrLE = getControlBlockPtr(builder, sourceRefLE, sourceType);
//      auto weakRefStructL = getStructWeakRefStruct(structReferend->fullName);
//      return assembleStructWeakRef(
//          globalState, builder, sourceType, structReferend, sourceRefLE,
//          mutWeakableControlBlockWrciMemberIndex, controlBlockPtrLE);
      assert(false);
    } else assert(false); // impl
  } else if (sourceType->ownership == Ownership::BORROW && targetOwnership == Ownership::WEAK) {
    if (auto structReferend = dynamic_cast<StructReferend*>(sourceType->referend)) {
//      auto controlBlockPtrLE = getControlBlockPtr(builder, sourceRefLE, sourceType);
//      auto weakRefStructL = getStructWeakRefStruct(structReferend->fullName);
//      return assembleStructWeakRef(
//          globalState, builder, sourceType, structReferend, sourceRefLE,
//          mutWeakableControlBlockWrciMemberIndex, controlBlockPtrLE);
      assert(false);
    } else assert(false); // impl
  } else {
    // Impossible conversion
    assert(false);
  }
}

LLVMValueRef RawRegion::alias(
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

  auto sourceRnd = sourceRef->referend;

  auto expr = castOwnership(globalState, builder, sourceRef, targetRef->ownership, sourceLE);

  if (targetRef->ownership == Ownership::SHARE) {
    return immMixin.alias(from, globalState, functionState, builder, targetRef, expr);
  } else {
    if (targetRef->ownership == Ownership::BORROW) {
      // Do nothing
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

void RawRegion::dealias(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    LLVMValueRef expr) {
  auto sourceRnd = sourceRef->referend;

  if (sourceRef->ownership == Ownership::SHARE) {
    immMixin.sharingDecrementStrongRc(from, globalState, functionState, blockState, builder, sourceRef, expr);
  } else {
    if (sourceRef->ownership == Ownership::OWN) {
      if (auto interfaceRnd = dynamic_cast<InterfaceReferend *>(sourceRnd)) {
        // We can't discard owns, they must be destructured.
        assert(false); // impl
      } else if (dynamic_cast<StructReferend *>(sourceRnd) ||
          dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
          dynamic_cast<UnknownSizeArrayT *>(sourceRnd)) {
        // We can't discard owns, they must be destructured.
        assert(false);
      } else assert(false);
    } else if (sourceRef->ownership == Ownership::BORROW) {
      // Raw mode, so nothing to decrement
    } else if (sourceRef->ownership == Ownership::WEAK) {
      decrementWeakRc(from, globalState, functionState, builder, sourceRef, expr);
    } else assert(false);
  }
}

LLVMValueRef RawRegion::loadMember(
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
//  if (mutability == Mutability::IMMUTABLE) {
//    if (structRefM->location == Location::INLINE) {
//      return LLVMBuildExtractValue(
//          builder, structExpr, memberIndex, memberName.c_str());
//    } else {
//      LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder, structExpr);
//      auto memberPtrLE =
//          LLVMBuildStructGEP(
//              builder, innerStructPtrLE, memberIndex, memberName.c_str());
//      auto resultLE =
//          LLVMBuildLoad(
//              builder,
//              memberPtrLE,
//              memberName.c_str());
//      auto targetOwnership = memberType->ownership == Ownership::SHARE ? Ownership::SHARE : Ownership::BORROW;
//      Reference* targetType =
//          globalState->metalCache.getReference(
//              memberType->referend,
//              memberType->location,
//              targetOwnership);
//      alias(from, globalState, functionState, builder, memberType, targetType, resultLE);
//      return resultLE;
//    }
//  } else if (mutability == Mutability::MUTABLE) {
//    LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder, structExpr);
//    auto memberPtrLE =
//        LLVMBuildStructGEP(
//            builder, innerStructPtrLE, memberIndex, memberName.c_str());
//    auto resultLE =
//        LLVMBuildLoad(
//            builder,
//            memberPtrLE,
//            memberName.c_str());
//    auto targetOwnership = memberType->ownership == Ownership::SHARE ? Ownership::SHARE : Ownership::BORROW;
//    Reference* targetType =
//        globalState->metalCache.getReference(
//            memberType->referend,
//            memberType->location,
//            targetOwnership);
//    alias(from, globalState, functionState, builder, memberType, targetType, resultLE);
//    return resultLE;
//  } else {
//    assert(false);
//    return nullptr;
//  }
  assert(false);
}

std::vector<LLVMValueRef> RawRegion::destructure(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* structType,
    LLVMValueRef structRefLE) {

  auto structReferend =
      dynamic_cast<StructReferend *>(structType->referend);
  assert(structReferend);
  auto structM = globalState->program->getStruct(structReferend->fullName);

  if (structM->weakable) {
    auto controlBlockPtrLE = getControlBlockPtr(builder, structRefLE, structType);
    markWrcDead(globalState, builder, controlBlockPtrLE, mutWeakableControlBlockWrciMemberIndex);
  }

  std::vector<LLVMValueRef> membersLE =
      getMemberPtrsLE(globalState, functionState, builder, structM, structRefLE);

  freeConcrete(
      globalState,
      functionState,
      builder,
      getControlBlockPtr(builder, structRefLE, structType),
      structType);

  return membersLE;
}

LLVMValueRef RawRegion::storeMember(
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
//
//  assert(mutability == Mutability::MUTABLE);
//  if (structRefM->ownership == Ownership::BORROW) {
//    adjustCounter(builder, globalState->mutDerefCounter, 1);
//  }
//
//  auto oldMemberLE =
//      swapMember(
//          builder, structExpr, memberIndex, memberName, sourceLE);
//  checkValidReference(from, globalState, functionState, builder, memberType, oldMemberLE);
//  functionState->defaultRegion->dealias(
//      AFL("MemberStore discard struct"), globalState, functionState, blockState, builder,
//      structRefM, structExpr);
//
//  return oldMemberLE;
  assert(false);
}

void RawRegion::destroyArray(
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* arrayType,
    LLVMValueRef arrayWrapperLE) {
//  if (arrayType->ownership == Ownership::OWN) {
//    adjustStrongRc(
//        AFL("Destroy decrementing the owning ref"),
//        globalState, functionState, builder, arrayWrapperLE, arrayType, -1);
//  } else if (arrayType->ownership == Ownership::SHARE) {
//    // We dont decrement anything here, we're only here because we already hit zero.
//  } else {
//    assert(false);
//  }
//
//  freeConcrete(AFL("DestroyKSAIntoF"), globalState, functionState, blockState, builder,
//      arrayWrapperLE, arrayType);
  assert(false);
}

LLVMTypeRef RawRegion::getControlBlockStructForStruct(StructDefinition* structM) {
  if (structM->mutability == Mutability::IMMUTABLE) {
    return immMixin.getControlBlockStruct();
  } else {
    if (structM->weakable) {
      return weakableControlBlockStructL;
    } else {
      return nonWeakableControlBlockStructL;
    }
  }
}

//LLVMTypeRef RawRegion::getControlBlockStructForInterface(InterfaceDefinition* interfaceM) {
//  if (interfaceM->weakable) {
//    return weakableControlBlockStructL;
//  } else {
//    return nonWeakableControlBlockStructL;
//  }
//}
//
//LLVMTypeRef RawRegion::getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) {
//  return nonWeakableControlBlockStructL;
//}

LLVMValueRef RawRegion::constructKnownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    KnownSizeArrayT* referendM,
    const std::vector<LLVMValueRef>& membersLE) {
//  auto structLT = getKnownSizeArrayType(globalState, referendM);
//  auto newStructLE = allocateStruct(globalState, builder, structTypeM, structLT);
//  fillControlBlock(
//      globalState,
//      builder,
//      getConcreteControlBlockPtr(builder, newStructLE),
//      false,
//      referendM->name->name);
//  return newStructLE;
  assert(false);
}


LLVMValueRef RawRegion::getKnownSizeArrayElementsPtr(
    LLVMBuilderRef builder, LLVMValueRef knownSizeArrayWrapperPtrLE) {
//  return LLVMBuildStructGEP(
//      builder,
//      knownSizeArrayWrapperPtrLE,
//      1, // Array is after the control block.
//      "ksaElemsPtr");
  assert(false);
}

LLVMValueRef RawRegion::getUnknownSizeArrayLength(
    LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) {
//  auto resultLE =
//      LLVMBuildStructGEP(
//          builder,
//          unknownSizeArrayWrapperPtrLE,
//          1, // Length is after the control block and before contents.
//          "usaLenPtr");
//  assert(LLVMTypeOf(resultLE) == LLVMPointerType(LLVMInt64Type(), 0));
//  return LLVMBuildLoad(builder, resultLE, "usaLen");
  assert(false);
}

LLVMValueRef RawRegion::getUnknownSizeArrayElementsPtr(
    LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) {
//  return LLVMBuildStructGEP(
//      builder,
//      unknownSizeArrayWrapperPtrLE,
//      2, // Array is after the control block and length.
//      "usaElemsPtr");
  assert(false);
}

//LLVMValueRef RawRegion::loadInnerArrayMember(
//    GlobalState* globalState,
//    LLVMBuilderRef builder,
//    LLVMValueRef elemsPtrLE,
//    Reference* elementRefM,
//    LLVMValueRef indexLE) {
//  assert(LLVMGetTypeKind(LLVMTypeOf(elemsPtrLE)) == LLVMPointerTypeKind);
//  LLVMValueRef indices[2] = {
//      constI64LE(0),
//      indexLE
//  };
//  auto resultLE =
//      LLVMBuildLoad(
//          builder,
//          LLVMBuildGEP(
//              builder, elemsPtrLE, indices, 2, "indexPtr"),
//          "index");
//
//  return resultLE;
//}

//LLVMValueRef RawRegion::storeInnerArrayMember(
//    GlobalState* globalState,
//    LLVMBuilderRef builder,
//    LLVMValueRef elemsPtrLE,
//    Reference* elementRefM,
//    LLVMValueRef indexLE,
//    LLVMValueRef sourceLE) {
//  assert(LLVMGetTypeKind(LLVMTypeOf(elemsPtrLE)) == LLVMPointerTypeKind);
//  LLVMValueRef indices[2] = {
//      constI64LE(0),
//      indexLE
//  };
//  auto resultLE =
//      LLVMBuildStore(
//          builder,
//          sourceLE,
//          LLVMBuildGEP(
//              builder, elemsPtrLE, indices, 2, "indexPtr"));
//
//  return resultLE;
//}

LLVMValueRef RawRegion::loadElement(
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
//
//  auto isNonNegativeLE = LLVMBuildICmp(builder, LLVMIntSGE, indexLE, constI64LE(0), "isNonNegative");
//  auto isUnderLength = LLVMBuildICmp(builder, LLVMIntSLT, indexLE, sizeLE, "isUnderLength");
//  auto isWithinBounds = LLVMBuildAnd(builder, isNonNegativeLE, isUnderLength, "isWithinBounds");
//  buildAssert(AFL("Bounds check"), globalState, functionState, builder, isWithinBounds, "Index out of bounds!");
//
//  if (mutability == Mutability::IMMUTABLE) {
//    if (structRefM->location == Location::INLINE) {
//      assert(false);
////      return LLVMBuildExtractValue(builder, structExpr, indexLE, "index");
//      return nullptr;
//    } else {
//      return loadInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE);
//    }
//  } else if (mutability == Mutability::MUTABLE) {
//    return loadInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE);
//  } else {
//    assert(false);
//    return nullptr;
//  }
  assert(false);
}


LLVMValueRef RawRegion::storeElement(
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
//
//  auto isNonNegativeLE = LLVMBuildICmp(builder, LLVMIntSGE, indexLE, constI64LE(0), "isNonNegative");
//  auto isUnderLength = LLVMBuildICmp(builder, LLVMIntSLT, indexLE, sizeLE, "isUnderLength");
//  auto isWithinBounds = LLVMBuildAnd(builder, isNonNegativeLE, isUnderLength, "isWithinBounds");
//  buildAssert(AFL("Bounds check"), globalState, functionState, builder, isWithinBounds, "Index out of bounds!");
//
//  if (mutability == Mutability::IMMUTABLE) {
//    if (arrayRefM->location == Location::INLINE) {
//      assert(false);
////      return LLVMBuildExtractValue(builder, structExpr, indexLE, "index");
//      return nullptr;
//    } else {
//      return storeInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE, sourceLE);
//    }
//  } else if (mutability == Mutability::MUTABLE) {
//    return storeInnerArrayMember(globalState, builder, arrayPtrLE, elementRefM, indexLE, sourceLE);
//  } else {
//    assert(false);
//    return nullptr;
//  }
  assert(false);
}


void RawRegion::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef refLE) {
  if (dynamic_cast<Str*>(refM->referend)) {
//    assert(LLVMTypeOf(refLE) == stringRefStructL);
    assert(false);
  }

  censusCheckValid(
      checkerAFL, globalState, functionState, builder, refM, refLE,
      [this](LLVMBuilderRef builder, Reference* refM, LLVMValueRef refLE) {
        return getControlBlockPtr(builder, refLE, refM);
      });
}


LLVMValueRef RawRegion::constructString(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  return immMixin.constructString(globalState, functionState, builder, lengthLE);
}

LLVMTypeRef RawRegion::getStringRefType() const {
  return immMixin.getStringRefType();
}

LLVMValueRef RawRegion::getStringControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef stringRefLE) {
  return immMixin.getStringControlBlockPtr(builder, stringRefLE);
}

LLVMValueRef RawRegion::getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
  return immMixin.getStringBytesPtr(builder, stringRefLE);
}

LLVMValueRef RawRegion::getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
  return immMixin.getStringLength(builder, stringRefLE);
}

LLVMValueRef RawRegion::getConcreteRefFromInterfaceRef(
    LLVMBuilderRef builder, LLVMValueRef refLE) {
//  return LLVMBuildPointerCast(
//      builder,
//      getInterfaceControlBlockPtr(builder, refLE),
//      LLVMPointerType(LLVMVoidType(), 0),
//      "objAsVoidPtr");
  assert(false);
}

LLVMValueRef RawRegion::upcast2(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructTypeM,
    StructReferend* sourceStructReferendM,
    LLVMValueRef sourceStructLE,

    Reference* targetInterfaceTypeM,
    InterfaceReferend* targetInterfaceReferendM) {
//  assert(sourceStructTypeM->location != Location::INLINE);
//
//  auto interfaceRefLT =
//      globalState->getInterfaceRefStruct(
//          targetInterfaceReferendM->fullName);
//
//  auto interfaceRefLE = LLVMGetUndef(interfaceRefLT);
//  interfaceRefLE =
//      LLVMBuildInsertValue(
//          builder,
//          interfaceRefLE,
//          getControlBlockPtr(builder, sourceStructLE, sourceStructTypeM),
//          0,
//          "interfaceRefWithOnlyObj");
//  interfaceRefLE =
//      LLVMBuildInsertValue(
//          builder,
//          interfaceRefLE,
//          globalState->getInterfaceTablePtr(
//              globalState->program->getStruct(sourceStructReferendM->fullName)
//                  ->getEdgeForInterface(targetInterfaceReferendM->fullName)),
//          1,
//          "interfaceRef");
//
//  checkValidReference(
//      FL(), globalState, functionState, builder, targetInterfaceTypeM, interfaceRefLE);
//  return interfaceRefLE;
  assert(false);
}


LLVMValueRef RawRegion::lockWeak(
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
//
//  auto isAliveLE = getIsAliveFromWeakRef(globalState, builder, sourceWeakRefLE);
//
//  adjustCounter(builder, globalState->mutDerefCounter, 1);
//
//  return buildIfElse(functionState, builder, isAliveLE, resultOptTypeL, thenResultIsNever, elseResultIsNever,
//      [this, globalState, functionState, constraintRefTypeM, sourceWeakRefLE, buildThen](LLVMBuilderRef thenBuilder) {
//        auto objPtrLE =
//            getConstraintRefFromWeakRef(
//                globalState,
//                functionState,
//                thenBuilder,
//                sourceWeakRefLE,
//                constraintRefTypeM);
//        return buildThen(thenBuilder, objPtrLE);
//      },
//      [buildElse](LLVMBuilderRef elseBuilder) {
//        return buildElse(elseBuilder);
//      });
  assert(false);
}

void RawRegion::translateStruct(
    GlobalState* globalState,
    StructDefinition* structM) {
  if (structM->mutability == Mutability::IMMUTABLE) {
    immMixin.translateStruct(globalState, this, structM);
  } else if (structM->mutability == Mutability::MUTABLE) {
//    LLVMTypeRef valStructL = getInnerStruct(structM->name);
//    auto contentsStructL = translateContentsStruct(globalState, this, structM);
//
//    auto targetWrapperStructL =
//        translateWrapperStruct(
//            globalState, structM, contentsStructL, getControlBlockStructForStruct(structM));
//
//    makeWeakRefStruct(globalState->getStructWeakRefStruct(structM->name), targetWrapperStructL);
    assert(false);
  } else assert(false);
}


//LLVMTypeRef RawRegion::translateInterfaceMethodToFunctionType(
//    GlobalState* globalState,
//    InterfaceMethod* method) {
//  auto returnLT = translateType(globalState, this, method->prototype->returnType);
//  auto paramsLT = translateTypes(globalState, this, method->prototype->params);
//  paramsLT[method->virtualParamIndex] = LLVMPointerType(LLVMVoidType(), 0);
//  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
//}

void RawRegion::translateInterface(
    GlobalState* globalState,
    InterfaceDefinition* interfaceM) {
//  LLVMTypeRef itableStruct =
//      globalState->getInterfaceTableStruct(interfaceM->name);
//  std::vector<LLVMTypeRef> interfaceMethodTypesL;
//  for (int i = 0; i < interfaceM->methods.size(); i++) {
//    interfaceMethodTypesL.push_back(
//        LLVMPointerType(
//            translateInterfaceMethodToFunctionType(
//                globalState, interfaceM->methods[i]),
//            0));
//  }
//  LLVMStructSetBody(
//      itableStruct, interfaceMethodTypesL.data(), interfaceMethodTypesL.size(), false);
//
//  LLVMTypeRef refStructL = globalState->getInterfaceRefStruct(interfaceM->name);
//  std::vector<LLVMTypeRef> refStructMemberTypesL;
//  // The object ptr is the 0th element, so we don't have to add and subtract 1
//  // whenever we want to affect its ref count.
//  // It points to the any struct, which is a wrapper around a ref count.
//  // It makes it easier to increment and decrement ref counts.
//  refStructMemberTypesL.push_back(LLVMPointerType(getControlBlockStructForInterface(interfaceM), 0));
//  refStructMemberTypesL.push_back(LLVMPointerType(itableStruct, 0));
//  LLVMStructSetBody(
//      refStructL,
//      refStructMemberTypesL.data(),
//      refStructMemberTypesL.size(),
//      false);
//
//  auto interfaceWeakRefStructL = globalState->getInterfaceWeakRefStruct(interfaceM->name);
//  std::vector<LLVMTypeRef> interfaceWeakRefStructMemberTypesL;
//  interfaceWeakRefStructMemberTypesL.push_back(LLVMPointerType(LLVMInt64Type(), 0));
//  interfaceWeakRefStructMemberTypesL.push_back(refStructL);
//  LLVMStructSetBody(interfaceWeakRefStructL, interfaceWeakRefStructMemberTypesL.data(), interfaceWeakRefStructMemberTypesL.size(), false);
  assert(false);
}


LLVMTypeRef RawRegion::translateType(GlobalState* globalState, Reference* referenceM) {
  assert(false);
}
void RawRegion::declareEdge(
    GlobalState* globalState,
    Edge* edge) {
  assert(false);
}
void RawRegion::translateEdge(
    GlobalState* globalState,
    Edge* edge) {
  assert(false);
}
LLVMTypeRef RawRegion::getStructRefType(
    GlobalState* globalState,
    Reference* refM,
    StructReferend* structReferendM) {
  assert(false);
}
void RawRegion::declareStruct(
    GlobalState* globalState,
    StructDefinition* structM) {
  assert(false);
}
void RawRegion::declareInterface(
    GlobalState* globalState,
    InterfaceDefinition* interfaceM) {
  assert(false);
}