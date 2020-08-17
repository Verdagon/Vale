#include <translatetype.h>
#include "struct.h"

constexpr int WEAK_REF_RCINDEX_MEMBER_INDEX = 0;
constexpr int WEAK_REF_OBJPTR_MEMBER_INDEX = 1;

void translateContentsStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM,
    LLVMTypeRef contentsStructL) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        region->translateType(globalState, structM->members[i]->type));
  }
  LLVMStructSetBody(
      contentsStructL, innerStructMemberTypesL.data(), innerStructMemberTypesL.size(), false);
}

void translateWrapperStruct(
    GlobalState* globalState,
    StructDefinition* structM,
    LLVMTypeRef wrapperStructL,
    LLVMTypeRef contentsStructL,
    LLVMTypeRef controlBlockStructL) {
  std::vector<LLVMTypeRef> countedStructMemberTypesL;
  // First member is a ref counts struct. We don't include the int directly
  // because we want fat pointers to point to this struct, so they can reach
  // into it and increment without doing any casting.
  countedStructMemberTypesL.push_back(controlBlockStructL);

  countedStructMemberTypesL.push_back(contentsStructL);
  LLVMStructSetBody(
      wrapperStructL, countedStructMemberTypesL.data(), countedStructMemberTypesL.size(), false);
}

LLVMTypeRef makeWeakRefStruct(
    LLVMTypeRef declaredWeakRefStructL,
    LLVMTypeRef targetWrapperStructL) {
  auto structWeakRefStructL = declaredWeakRefStructL;
  std::vector<LLVMTypeRef> structWeakRefStructMemberTypesL;
  structWeakRefStructMemberTypesL.push_back(LLVMInt64Type());
  structWeakRefStructMemberTypesL.push_back(LLVMPointerType(targetWrapperStructL, 0));
  LLVMStructSetBody(structWeakRefStructL, structWeakRefStructMemberTypesL.data(), structWeakRefStructMemberTypesL.size(), false);
  return structWeakRefStructL;
}

LLVMValueRef fillControlBlockObjIdAndTypeStr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    int controlBlockObjIdIndex,
    int controlBlockTypeStrIndex,
    const std::string& typeName,
    LLVMValueRef newControlBlockLE) {
  auto objIdLE = adjustCounter(builder, globalState->objIdCounter, 1);
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
  return newControlBlockLE;
}

LLVMValueRef fillControlBlockStrongRc(
    LLVMBuilderRef builder,
    LLVMValueRef newControlBlockLE,
    int controlBlockRcMemberIndex) {
  return LLVMBuildInsertValue(
      builder,
      newControlBlockLE,
      // Start at 1, 0 would mean it's dead.
      LLVMConstInt(LLVMInt64Type(), 1, false),
      controlBlockRcMemberIndex,
      "controlBlockWithRc");
}

LLVMValueRef fillControlBlockWrci(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef newControlBlockLE,
    int controlBlockWrciMemberIndex) {
  auto wrciLE = LLVMBuildCall(builder, globalState->allocWrc, nullptr, 0, "");
  return LLVMBuildInsertValue(
      builder,
      newControlBlockLE,
      wrciLE,
      controlBlockWrciMemberIndex,
      "controlBlockComplete");
}

LLVMValueRef getWrciFromControlBlockPtr(
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

LLVMValueRef assembleStructWeakRef(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    StructReferend* structReferendM,
    LLVMTypeRef weakRefStructL,
    LLVMValueRef objPtrLE,
    int controlBlockWrciMemberIndex,
    LLVMValueRef controlBlockPtrLE) {
  auto wrciLE = getWrciFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockWrciMemberIndex);

  auto weakRefLE = LLVMGetUndef(weakRefStructL);
  weakRefLE = LLVMBuildInsertValue(builder, weakRefLE, wrciLE, WEAK_REF_RCINDEX_MEMBER_INDEX, "");
  weakRefLE = LLVMBuildInsertValue(builder, weakRefLE, objPtrLE, WEAK_REF_OBJPTR_MEMBER_INDEX, "");
  return weakRefLE;
}

LLVMValueRef getWrciFromWeakRef(
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE) {
  return LLVMBuildExtractValue(builder, weakRefLE, WEAK_REF_RCINDEX_MEMBER_INDEX, "wrci");
}

LLVMValueRef getObjPtrFromWeakRef(
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE) {
  return LLVMBuildExtractValue(builder, weakRefLE, WEAK_REF_OBJPTR_MEMBER_INDEX, "");
}

void incrementWeakRc(
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

LLVMValueRef getObjIdFromControlBlockPtr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockObjIdIndex) {
  return LLVMBuildLoad(
      builder,
      LLVMBuildStructGEP(
          builder,
          controlBlockPtr,
          controlBlockObjIdIndex,
          "objIdPtr"),
      "objId");
}

LLVMValueRef getTypeNameStrPtrFromControlBlockPtr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockTypeStrIndex) {
  return LLVMBuildLoad(
      builder,
      LLVMBuildStructGEP(
          builder,
          controlBlockPtr,
          controlBlockTypeStrIndex,
          "typeNameStrPtrPtr"),
      "typeNameStrPtr");
}

void flareAdjustStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef controlBlockPtr,
    LLVMValueRef oldAmount,
    LLVMValueRef newAmount,
    int controlBlockTypeStrIndex,
    int controlBlockObjIdIndex) {
  buildFlare(
      from,
      globalState,
      functionState,
      builder,
      typeid(*refM->referend).name(),
      " ",
      getTypeNameStrPtrFromControlBlockPtr(globalState, builder, controlBlockPtr, controlBlockTypeStrIndex),
      getObjIdFromControlBlockPtr(globalState, builder, controlBlockPtr, controlBlockObjIdIndex),
      ", ",
      oldAmount,
      "->",
      newAmount);
}
//

// See CRCISFAORC for why we don't take in a mutability.
LLVMValueRef getStrongRcPtrFromControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockRcMemberIndex) {
  return LLVMBuildStructGEP(
      builder,
      controlBlockPtr,
      controlBlockRcMemberIndex,
      "rcPtr");
}

// Returns the new RC
LLVMValueRef adjustStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
    int amount) {
  auto rcPtrLE = getStrongRcPtrFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockRcMemberIndex);
  auto oldRc = LLVMBuildLoad(builder, rcPtrLE, "oldRc");
  auto newRc = adjustCounter(builder, rcPtrLE, amount);
  if (refM->ownership != Ownership::SHARE) {
    adjustCounter(builder, globalState->mutRcAccessCounter, 1);
  }
  if (maybeTypeStrAndObjIdIndex) {
    int controlBlockTypeStrIndex = 0;
    int controlBlockObjIdIndex = 0;
    std::tie(controlBlockTypeStrIndex, controlBlockObjIdIndex) = *maybeTypeStrAndObjIdIndex;
    flareAdjustStrongRc(from, globalState, functionState, builder, refM, controlBlockPtrLE, oldRc, newRc, controlBlockTypeStrIndex, controlBlockObjIdIndex);
  }
  return newRc;
}

LLVMValueRef getStrongRcFromControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockRcMemberIndex) {
  auto rcPtrLE = getStrongRcPtrFromControlBlockPtr(builder, controlBlockPtr, controlBlockRcMemberIndex);
  return LLVMBuildLoad(builder, rcPtrLE, "rc");
}

LLVMValueRef strongRcIsZero(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    int controlBlockRcMemberIndex) {
  return isZeroLE(builder, getStrongRcFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockRcMemberIndex));
}

void incrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
    LLVMValueRef controlBlockPtr) {
  auto sourceRnd = refM->referend;

  if (dynamic_cast<Int*>(sourceRnd) ||
      dynamic_cast<Bool*>(sourceRnd) ||
      dynamic_cast<Float*>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (dynamic_cast<InterfaceReferend*>(sourceRnd)) {
    if (refM->location == Location::INLINE) {
      assert(false); // impl
    } else {
      adjustStrongRc(from, globalState, functionState, builder, controlBlockPtr, refM, controlBlockRcMemberIndex, maybeTypeStrAndObjIdIndex, 1);
    }
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    if (refM->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      adjustStrongRc(from, globalState, functionState, builder, controlBlockPtr, refM, controlBlockRcMemberIndex, maybeTypeStrAndObjIdIndex, 1);
    }
  } else if (dynamic_cast<Str*>(sourceRnd)) {
    assert(refM->location == Location::YONDER);
    adjustStrongRc(from, globalState, functionState, builder, controlBlockPtr, refM, controlBlockRcMemberIndex, maybeTypeStrAndObjIdIndex, 1);
  } else {
    std::cerr << "Unimplemented type in incrementStrongRc: "
        << typeid(*refM->referend).name() << std::endl;
    assert(false);
  }
}

void decrementWeakRc(
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

void nonOwningDecrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
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
      adjustStrongRc(from, globalState, functionState, builder, expr, refM, controlBlockRcMemberIndex, maybeTypeStrAndObjIdIndex, -1);
    }
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    if (refM->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      adjustStrongRc(from, globalState, functionState, builder, expr, refM, controlBlockRcMemberIndex, maybeTypeStrAndObjIdIndex, -1);
    }
  } else {
    std::cerr << "Unimplemented type in incrementStrongRc: "
        << typeid(*refM->referend).name() << std::endl;
    assert(false);
  }
}

void freeConcrete(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    Reference* concreteRefM) {

  if (globalState->opt->census) {
    LLVMValueRef allocationVoidPtrLE =
        LLVMBuildBitCast(
            builder, controlBlockPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    LLVMBuildCall(builder, globalState->censusRemove, &allocationVoidPtrLE, 1,
        "");
  }

  if (concreteRefM->location == Location::INLINE) {
    // Do nothing, it was alloca'd.
  } else if (concreteRefM->location == Location::YONDER) {
    LLVMValueRef allocationVoidPtrLE =
        LLVMBuildBitCast(
            builder, controlBlockPtrLE, LLVMPointerType(LLVMVoidType(), 0), "");
    auto concreteAsCharPtrLE =
        LLVMBuildBitCast(
            builder,
            allocationVoidPtrLE,
            LLVMPointerType(LLVMInt8Type(), 0),
            "concreteCharPtrForFree");
    buildFlare(
        AFL("Freeing: "),
        globalState,
        functionState,
        builder,
        LLVMBuildBitCast(builder, concreteAsCharPtrLE, LLVMPointerType(LLVMInt64Type(), 0), "printthis"));
    LLVMBuildCall(
        builder, globalState->free, &concreteAsCharPtrLE, 1, "");
  }

  adjustCounter(builder, globalState->liveHeapObjCounter, -1);
}

void markWrcDead(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    int controlBlockWrciMemberIndex) {
  auto wrciLE = getWrciFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockWrciMemberIndex);
  LLVMBuildCall(builder, globalState->markWrcDead, &wrciLE, 1, "");
}

void checkStrongRcZero(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockRcMemberIndex) {
  auto rcIsZeroLE = strongRcIsZero(builder, controlBlockPtr, controlBlockRcMemberIndex);
  buildAssert(from, globalState, functionState, builder, rcIsZeroLE,
      "Tried to free concrete that had nonzero RC!");
}
