#include <translatetype.h>
#include <regions/shared/construct.h>
#include "function/expressions/shared/shared.h"
#include "function/expressions/shared/members.h"
#include "function/expressions/shared/branch.h"
#include "regions/shared/heap.h"
#include "function/expressions/shared/controlblock.h"
#include "assist.h"

constexpr int WEAK_REF_RCINDEX_MEMBER_INDEX = 0;
constexpr int WEAK_REF_OBJPTR_MEMBER_INDEX = 1;

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
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else {
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
      functionState,
      builder,
      getConcreteControlBlockPtr(builder, usaWrapperPtrLE),
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

LLVMValueRef AssistRegion::getWrciFromWeakRef(
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE) {
  return LLVMBuildExtractValue(builder, weakRefLE, WEAK_REF_RCINDEX_MEMBER_INDEX, "wrci");
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
    Reference* weakRefM,
    LLVMValueRef weakRefLE,
    Reference* constraintRefM) {
  auto refLE = LLVMBuildExtractValue(builder, weakRefLE, WEAK_REF_OBJPTR_MEMBER_INDEX, "");
  checkValidReference(FL(), globalState, functionState, builder, constraintRefM, refLE);
  functionState->defaultRegion->alias(FL(), globalState, functionState, builder, constraintRefM, Ownership::BORROW, refLE);
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
          globalState, functionState, builder,
          getConcreteControlBlockPtr(builder, newStructPtrLE), structM->name->name);
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
    FunctionState* functionState,
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

AssistRegion::AssistRegion() {
  auto voidLT = LLVMVoidType();
  auto voidPtrLT = LLVMPointerType(voidLT, 0);
  auto int1LT = LLVMInt1Type();
  auto int8LT = LLVMInt8Type();
  auto int64LT = LLVMInt64Type();
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int64PtrLT = LLVMPointerType(int64LT, 0);

  {
    auto controlBlockStructL =
        LLVMStructCreateNamed(
            LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
    std::vector<LLVMTypeRef> memberTypesL;

    controlBlockTypeStrIndex = memberTypesL.size();
    memberTypesL.push_back(int8PtrLT);

    controlBlockObjIdIndex = memberTypesL.size();
    memberTypesL.push_back(int64LT);

    controlBlockRcMemberIndex = memberTypesL.size();
    memberTypesL.push_back(int64LT);

    controlBlockWrciMemberIndex = memberTypesL.size();
    memberTypesL.push_back(int64LT);

    LLVMStructSetBody(
        controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
    weakableControlBlockStructL = controlBlockStructL;
  }

  {
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
    nonWeakableControlBlockStructL = controlBlockStructL;
  }

//  {
//    stringInnerStructL =
//        LLVMStructCreateNamed(
//            LLVMGetGlobalContext(), "__Str");
//    std::vector<LLVMTypeRef> memberTypesL;
//    memberTypesL.push_back(LLVMInt64Type());
//    memberTypesL.push_back(LLVMArrayType(int8LT, 0));
//    LLVMStructSetBody(
//        stringInnerStructL, memberTypesL.data(), memberTypesL.size(), false);
//  }
//
//  {
//    stringWrapperStructL =
//        LLVMStructCreateNamed(
//            LLVMGetGlobalContext(), "__Str_rc");
//    std::vector<LLVMTypeRef> memberTypesL;
//    memberTypesL.push_back(nonWeakableControlBlockStructL);
//    memberTypesL.push_back(stringInnerStructL);
//    LLVMStructSetBody(
//        stringWrapperStructL, memberTypesL.data(), memberTypesL.size(), false);
//  }
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
      auto countedStructL = globalState->getCountedStruct(structReferend->fullName);
      return constructCountedStruct(
          from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
    }
    case Mutability::IMMUTABLE: {
      if (desiredReference->location == Location::INLINE) {
        auto valStructL =
            globalState->getInnerStruct(structReferend->fullName);
        return constructInnerStruct(
            builder, structM, valStructL, membersLE);
      } else {
        auto countedStructL =
            globalState->getCountedStruct(structReferend->fullName);
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

LLVMValueRef AssistRegion::assembleStructWeakRef(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    StructReferend* structReferendM,
    LLVMValueRef objPtrLE,
    int controlBlockWrciMemberIndex) {
  auto controlBlockPtrLE = getControlBlockPtr(builder, objPtrLE, structTypeM);
  auto wrciLE = getWrciFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockWrciMemberIndex);

  auto weakRefLE = LLVMGetUndef(globalState->getStructWeakRefStruct(structReferendM->fullName));
  weakRefLE = LLVMBuildInsertValue(builder, weakRefLE, wrciLE, WEAK_REF_RCINDEX_MEMBER_INDEX, "");
  weakRefLE = LLVMBuildInsertValue(builder, weakRefLE, objPtrLE, WEAK_REF_OBJPTR_MEMBER_INDEX, "");
  return weakRefLE;
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
        return assembleStructWeakRef(
                globalState, builder, sourceType, structReferend, sourceRefLE, controlBlockWrciMemberIndex);
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
        // We do the same thing for inline and yonder muts, the only difference is
        // where the memory lives.
        return
            assembleStructWeakRef(
                globalState, builder, sourceType, structReferendM, sourceRefLE, controlBlockWrciMemberIndex);
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
}

LLVMValueRef AssistRegion::alias(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    Ownership targetOwnership,
    LLVMValueRef sourceLE) {
  auto sourceRnd = sourceRef->referend;

  auto expr = castOwnership(globalState, builder, sourceRef, targetOwnership, sourceLE);

  if (targetOwnership == Ownership::SHARE) {
    if (sourceRef->location == Location::INLINE) {
      // Do nothing
    } else if (sourceRef->location == Location::YONDER) {
      incrementStrongRc(from, globalState, functionState, builder, sourceRef, expr);
    } else assert(false);
  } else if (targetOwnership == Ownership::BORROW) {
    adjustStrongRc(from, globalState, functionState, builder, expr, sourceRef, 1);
  } else if (targetOwnership == Ownership::WEAK) {
    incrementWeakRc(from, globalState, functionState, builder, sourceRef, expr);
  } else if (targetOwnership == Ownership::OWN) {
    if (dynamic_cast<InterfaceReferend*>(sourceRnd)) {
      // We should never acquire an owning reference.
      // If you trip this, perhaps you're trying to borrow, and you handed in
      // the wrong thing for resultRef?
      assert(false);
    } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
        dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
        dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
      // We might be loading a member as an own if we're destructuring.
      // Don't adjust the RC, since we're only moving it.
    } else assert(false);
  } else assert(false);

  return expr;
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
      alias(from, globalState, functionState, builder, memberType, Ownership::BORROW, resultLE);
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
    alias(from, globalState, functionState, builder, memberType, Ownership::BORROW, resultLE);
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

  auto mutability = ownershipToMutability(structType->ownership);

  auto structReferend =
      dynamic_cast<StructReferend *>(structType->referend);
  assert(structReferend);

  auto structM = globalState->program->getStruct(structReferend->fullName);

  if (structM->weakable) {
    auto controlBlockPtrLE = getControlBlockPtr(builder, structLE, structType);
    auto wrciLE = getWrciFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockWrciMemberIndex);
    LLVMBuildCall(builder, globalState->markWrcDead, &wrciLE, 1, "");
  }

  auto innerStructPtrLE = getStructContentsPtr(builder, structLE);
  std::vector<LLVMValueRef> membersLE =
      getMemberPtrsLE(globalState, functionState, builder, structM, innerStructPtrLE);

  if (structType->ownership == Ownership::OWN) {
    adjustStrongRc(
        AFL("Destroy decrementing the owning ref"),
        globalState, functionState, builder, structLE, structType, -1);
  } else if (structType->ownership == Ownership::SHARE) {
    // We dont decrement anything here, we're only here because we already hit zero.
  } else {
    assert(false);
  }

  freeConcrete(
      AFL("Destroy freeing"),
      globalState,
      functionState,
      blockState,
      builder,
      structLE,
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
  checkValidReference(from, globalState, functionState, builder, memberType, structExpr);
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

  freeConcrete(AFL("DestroyKSAIntoF"), globalState, functionState, blockState, builder,
      arrayWrapperLE, arrayType);
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

LLVMTypeRef AssistRegion::getControlBlockStructForKnownSizeArray(KnownSizeArrayT* arrMT) {
  return nonWeakableControlBlockStructL;
}

LLVMTypeRef AssistRegion::getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) {
  return nonWeakableControlBlockStructL;
}

LLVMValueRef AssistRegion::constructKnownSizeArray(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    LLVMTypeRef structLT,
    const std::vector<LLVMValueRef>& membersLE,
    const std::string& typeName) {
  auto newStructLE = allocateStruct(globalState, builder, structTypeM, structLT);
  fillControlBlock(
      globalState,
      functionState,
      builder,
      getConcreteControlBlockPtr(builder, newStructLE),
      typeName);
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
  return resultLE;
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


void AssistRegion::incrementStrongRc(
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
      adjustStrongRc(from, globalState, functionState, builder, expr, refM, 1);
    }
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    if (refM->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      adjustStrongRc(from, globalState, functionState, builder, expr, refM, 1);
    }
  } else if (dynamic_cast<Str*>(sourceRnd)) {
    assert(refM->location == Location::YONDER);
    adjustStrongRc(from, globalState, functionState, builder, expr, refM, 1);
  } else {
    std::cerr << "Unimplemented type in incrementStrongRc: "
        << typeid(*refM->referend).name() << std::endl;
    assert(false);
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
          [globalState, expr, interfaceRnd, sourceRef](LLVMBuilderRef thenBuilder) {
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
            buildInterfaceCall(thenBuilder, argExprsL, 0, indexInEdge);
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
    buildIf(
        functionState,
        builder,
        isZeroLE(builder, rcLE),
        [from, globalState, functionState, blockState, expr, sourceRef](LLVMBuilderRef thenBuilder) {
          freeConcrete(from, globalState, functionState, blockState, thenBuilder, expr, sourceRef);
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
  if (globalState->opt->census) {
    if (refM->ownership == Ownership::OWN) {
      auto controlBlockPtrLE = getControlBlockPtr(builder, refLE, refM);
      buildAssertCensusContains(checkerAFL, globalState, functionState, builder, controlBlockPtrLE);
    } else if (refM->ownership == Ownership::SHARE) {
      if (refM->location == Location::INLINE) {
        // Nothing to do, there's no control block or ref counts or anything.
      } else if (refM->location == Location::YONDER) {
        auto controlBlockPtrLE = getControlBlockPtr(builder, refLE, refM);

        // We dont check ref count >0 because imm destructors receive with rc=0.
        //      auto rcLE = getRcFromControlBlockPtr(globalState, builder, controlBlockPtrLE);
        //      auto rcPositiveLE = LLVMBuildICmp(builder, LLVMIntSGT, rcLE, constI64LE(0), "");
        //      buildAssert(checkerAFL, globalState, functionState, blockState, builder, rcPositiveLE, "Invalid RC!");

        buildAssertCensusContains(checkerAFL, globalState, functionState, builder, controlBlockPtrLE);
      } else assert(false);
    } else if (refM->ownership == Ownership::BORROW) {
      auto controlBlockPtrLE = getControlBlockPtr(builder, refLE, refM);
      buildAssertCensusContains(checkerAFL, globalState, functionState, builder, controlBlockPtrLE);
    } else assert(false);
  }
}

LLVMValueRef AssistRegion::mallocStr(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {

  // The +1 is for the null terminator at the end, for C compatibility.
  auto sizeBytesLE =
      LLVMBuildAdd(
          builder,
          lengthLE,
          makeConstIntExpr(builder,LLVMInt64Type(),  1 + LLVMABISizeOfType(globalState->dataLayout, LLVMPointerType(LLVMInt8Type(), 0))),
          "strMallocSizeBytes");

  auto destCharPtrLE =
      LLVMBuildCall(builder, globalState->malloc, &sizeBytesLE, 1, "donePtr");

  adjustCounter(builder, globalState->liveHeapObjCounter, 1);

  auto newStrWrapperPtrLE =
      LLVMBuildBitCast(
          builder,
          destCharPtrLE,
          LLVMPointerType(LLVMInt8Type(), 0),
          "newStrWrapperPtr");
  fillControlBlock(
      globalState, functionState, builder, getConcreteControlBlockPtr(builder, newStrWrapperPtrLE), "Str");
  LLVMBuildStore(builder, lengthLE, getLenPtrFromStrWrapperPtr(builder, newStrWrapperPtrLE));

  if (globalState->opt->census) {
    LLVMValueRef resultAsVoidPtrLE =
        LLVMBuildBitCast(
            builder, newStrWrapperPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
  }

  // The caller still needs to initialize the actual chars inside!

  return newStrWrapperPtrLE;
}

LLVMValueRef AssistRegion::getInnerStrPtrFromWrapperPtr(
    LLVMBuilderRef builder,
    LLVMValueRef strWrapperPtrLE) {
  return LLVMBuildStructGEP(
      builder, strWrapperPtrLE, 1, "strInnerStructPtr");
}

LLVMValueRef AssistRegion::getLenPtrFromStrWrapperPtr(
    LLVMBuilderRef builder,
    LLVMValueRef strWrapperPtrLE) {
  auto innerStringPtrLE =
      getInnerStrPtrFromWrapperPtr(builder, strWrapperPtrLE);
  auto lenPtrLE =
      LLVMBuildStructGEP(builder, innerStringPtrLE, 0, "lenPtr");
  return lenPtrLE;
}

LLVMValueRef AssistRegion::getLenFromStrWrapperPtr(
    LLVMBuilderRef builder,
    LLVMValueRef strWrapperPtrLE) {
  return LLVMBuildLoad(builder, getLenPtrFromStrWrapperPtr(builder, strWrapperPtrLE), "len");
}

LLVMValueRef AssistRegion::buildConstantVStr(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    const std::string& contents) {

  auto lengthLE = constI64LE(contents.length());

  auto strWrapperPtrLE = mallocStr(globalState, functionState, builder, lengthLE);

  std::vector<LLVMValueRef> argsLE = {
      getInnerStrPtrFromWrapperPtr(builder, strWrapperPtrLE),
      globalState->getOrMakeStringConstant(contents)
  };
  LLVMBuildCall(builder, globalState->initStr, argsLE.data(), argsLE.size(), "");

  return strWrapperPtrLE;
}
