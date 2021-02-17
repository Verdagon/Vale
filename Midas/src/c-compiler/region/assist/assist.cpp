#include <utils/branch.h>
#include <region/common/fatweaks/fatweaks.h>
#include <region/common/hgm/hgm.h>
#include <region/common/lgtweaks/lgtweaks.h>
#include <region/common/wrcweaks/wrcweaks.h>
#include <translatetype.h>
#include <region/common/common.h>
#include <region/common/heap.h>
#include <sstream>
#include "assist.h"

Assist::Assist(GlobalState* globalState_) :
    globalState(globalState_),
    mutNonWeakableStructs(globalState, makeAssistAndNaiveRCNonWeakableControlBlock(globalState)),
    mutWeakableStructs(
        globalState,
        makeAssistAndNaiveRCWeakableControlBlock(globalState),
        WrcWeaks::makeWeakRefHeaderStruct(globalState)),
    referendStructs(
        globalState,
        [this](Referend* referend) -> IReferendStructsSource* {
          if (globalState->getReferendMutability(referend) == Mutability::IMMUTABLE) {
            assert(false);
          } else {
            if (globalState->getReferendWeakability(referend) == Weakability::NON_WEAKABLE) {
              return &mutNonWeakableStructs;
            } else {
              return &mutWeakableStructs;
            }
          }
        }),
    weakRefStructs(
        [this](Referend* referend) -> IWeakRefStructsSource* {
          if (globalState->getReferendMutability(referend) == Mutability::IMMUTABLE) {
            assert(false);
          } else {
            if (globalState->getReferendWeakability(referend) == Weakability::NON_WEAKABLE) {
              assert(false);
            } else {
              return &mutWeakableStructs;
            }
          }
        }),
    fatWeaks(globalState_, &weakRefStructs),
    wrcWeaks(globalState_, &referendStructs, &weakRefStructs) {
  regionLT = LLVMStructCreateNamed(globalState->context, "__Assist_Region");
  LLVMStructSetBody(regionLT, nullptr, 0, false);
}

RegionId* Assist::getRegionId() {
  return globalState->metalCache->assistRegionId;
}

void Assist::alias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    Ref ref) {
  auto sourceRnd = sourceRef->referend;

  if (dynamic_cast<Int *>(sourceRnd) ||
      dynamic_cast<Bool *>(sourceRnd) ||
      dynamic_cast<Float *>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (dynamic_cast<InterfaceReferend *>(sourceRnd) ||
      dynamic_cast<StructReferend *>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT *>(sourceRnd) ||
      dynamic_cast<Str *>(sourceRnd)) {
    if (sourceRef->ownership == Ownership::OWN) {
      // This can happen if we just allocated something. It's RC is already zero, and we want to
      // bump it to 1 for the owning reference.
      adjustStrongRc(from, globalState, functionState, &referendStructs, builder, ref, sourceRef, 1);
    } else if (sourceRef->ownership == Ownership::BORROW) {
      adjustStrongRc(from, globalState, functionState, &referendStructs, builder, ref, sourceRef, 1);
    } else if (sourceRef->ownership == Ownership::WEAK) {
      aliasWeakRef(from, functionState, builder, sourceRef, ref);
    } else if (sourceRef->ownership == Ownership::SHARE) {
      if (sourceRef->location == Location::INLINE) {
        // Do nothing, we can just let inline structs disappear
      } else {
        adjustStrongRc(from, globalState, functionState, &referendStructs, builder, ref, sourceRef, 1);
      }
    } else
      assert(false);
  } else {
    std::cerr << "Unimplemented type in acquireReference: "
        << typeid(*sourceRef->referend).name() << std::endl;
    assert(false);
  }
}


void Assist::dealias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  if (sourceMT->ownership == Ownership::SHARE) {
    assert(false);
  } else if (sourceMT->ownership == Ownership::OWN) {
    // This can happen if we're sending an owning reference to the outside world, see DEPAR.
    adjustStrongRc(from, globalState, functionState, &referendStructs, builder, sourceRef, sourceMT, -1);
  } else if (sourceMT->ownership == Ownership::BORROW) {
    adjustStrongRc(from, globalState, functionState, &referendStructs, builder, sourceRef, sourceMT, -1);
  } else if (sourceMT->ownership == Ownership::WEAK) {
    discardWeakRef(from, functionState, builder, sourceMT, sourceRef);
  } else assert(false);
}

Ref Assist::lockWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    bool thenResultIsNever,
    bool elseResultIsNever,
    Reference* resultOptTypeM,
    Reference* constraintRefM,
    Reference* sourceWeakRefMT,
    Ref sourceWeakRef,
    bool sourceKnownLive,
    std::function<Ref(LLVMBuilderRef, Ref)> buildThen,
    std::function<Ref(LLVMBuilderRef)> buildElse) {
  assert(sourceWeakRefMT->ownership == Ownership::WEAK);
  auto isAliveLE =
      getIsAliveFromWeakRef(
          functionState, builder, sourceWeakRefMT, sourceWeakRef, sourceKnownLive);
  auto resultOptTypeLE = translateType(resultOptTypeM);
  return regularInnerLockWeak(
      globalState, functionState, builder, thenResultIsNever, elseResultIsNever, resultOptTypeM,
      constraintRefM, sourceWeakRefMT, sourceWeakRef, buildThen, buildElse,
      isAliveLE, resultOptTypeLE, &weakRefStructs, &fatWeaks);
}

LLVMTypeRef Assist::translateType(Reference* referenceM) {
  switch (referenceM->ownership) {
    case Ownership::SHARE:
      assert(false);
    case Ownership::OWN:
    case Ownership::BORROW:
      assert(referenceM->location != Location::INLINE);
      if (referenceM->referend == globalState->metalCache->regionReferend) {
        return LLVMPointerType(regionLT, 0);
      } else {
        return translateReferenceSimple(globalState, &referendStructs, referenceM->referend);
      }
    case Ownership::WEAK:
      assert(referenceM->location != Location::INLINE);
      return translateWeakReference(globalState, &weakRefStructs, referenceM->referend);
    default:
      assert(false);
  }
}

Ref Assist::upcastWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    WeakFatPtrLE sourceRefLE,
    StructReferend* sourceStructReferendM,
    Reference* sourceStructTypeM,
    InterfaceReferend* targetInterfaceReferendM,
    Reference* targetInterfaceTypeM) {
  return wrap(
      this,
      targetInterfaceTypeM,
      wrcWeaks.weakStructPtrToWrciWeakInterfacePtr(
          globalState, functionState, builder, sourceRefLE, sourceStructReferendM,
          sourceStructTypeM, targetInterfaceReferendM, targetInterfaceTypeM));
}

void Assist::declareKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  referendStructs.declareKnownSizeArray(knownSizeArrayMT);
}

void Assist::declareUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  referendStructs.declareUnknownSizeArray(unknownSizeArrayMT);
}

void Assist::translateUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto elementLT =
      globalState->getRegion(unknownSizeArrayMT->rawArray->elementType)
          ->translateType(unknownSizeArrayMT->rawArray->elementType);
  referendStructs.translateUnknownSizeArray(unknownSizeArrayMT, elementLT);
}

void Assist::translateKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto elementLT =
      globalState->getRegion(knownSizeArrayMT->rawArray->elementType)
          ->translateType(knownSizeArrayMT->rawArray->elementType);
  referendStructs.translateKnownSizeArray(knownSizeArrayMT, elementLT);
}

void Assist::declareStruct(
    StructDefinition* structM) {
  referendStructs.declareStruct(structM);
}

void Assist::translateStruct(
    StructDefinition* structM) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        globalState->getRegion(structM->members[i]->type)
            ->translateType(structM->members[i]->type));
  }
  referendStructs.translateStruct(
      structM,
      innerStructMemberTypesL);
}

void Assist::declareEdge(
    Edge* edge) {
  referendStructs.declareEdge(edge);
}

void Assist::translateEdge(
    Edge* edge) {
  auto interfaceM = globalState->program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  std::tie(interfaceFunctionsLT, edgeFunctionsL) =
      globalState->getEdgeFunctionTypesAndFunctions(edge);

  referendStructs.translateEdge(edge, interfaceFunctionsLT, edgeFunctionsL);
}

void Assist::declareInterface(
    InterfaceDefinition* interfaceM) {
  referendStructs.declareInterface(interfaceM);
}

void Assist::translateInterface(
    InterfaceDefinition* interfaceM) {
  assert((uint64_t)interfaceM->referend > 0x10000);
  std::vector<LLVMTypeRef> interfaceMethodTypesL;
  for (int i = 0; i < interfaceM->methods.size(); i++) {
    interfaceMethodTypesL.push_back(
        LLVMPointerType(
            translateInterfaceMethodToFunctionType(globalState, interfaceM->methods[i]),
            0));
  }
  referendStructs.translateInterface(
      interfaceM,
      interfaceMethodTypesL);
}

Ref Assist::weakAlias(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Reference* targetRefMT,
    Ref sourceRef) {
  return regularWeakAlias(globalState, functionState, &referendStructs, &wrcWeaks, builder, sourceRefMT, targetRefMT, sourceRef);
}

void Assist::discardOwningRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  auto exprWrapperPtrLE =
      referendStructs.makeWrapperPtr(
          FL(), functionState, builder, sourceMT,
          checkValidReference(FL(), functionState, builder, sourceMT, sourceRef));

  adjustStrongRc(
      AFL("Destroy decrementing the owning ref"),
      globalState, functionState, &referendStructs, builder, sourceRef, sourceMT, -1);
  // No need to check the RC, we know we're freeing right now.

  // Free it!
  deallocate(AFL("discardOwningRef"), functionState, builder, sourceMT, sourceRef);
}

void Assist::noteWeakableDestroyed(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    ControlBlockPtrLE controlBlockPtrLE) {
  auto rc = referendStructs.getStrongRcFromControlBlockPtr(builder, refM, controlBlockPtrLE);
  buildAssertIntEq(globalState, functionState, builder, rc, constI32LE(globalState, 0),
      "Tried to free concrete that had nonzero RC!");

  if (auto structReferendM = dynamic_cast<StructReferend*>(refM->referend)) {
    auto structM = globalState->program->getStruct(structReferendM->fullName);
    if (structM->weakability == Weakability::WEAKABLE) {
      wrcWeaks.innerNoteWeakableDestroyed(functionState, builder, refM, controlBlockPtrLE);
    }
  } else if (auto interfaceReferendM = dynamic_cast<InterfaceReferend*>(refM->referend)) {
    assert(false); // Do we ever deallocate an interface?
  } else {
    // Do nothing, only structs and interfaces are weakable in assist mode.
  }
}

Ref Assist::loadMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    bool structKnownLive,
    int memberIndex,
    Reference* expectedMemberType,
    Reference* targetType,
    const std::string& memberName) {
  if (structRefMT->ownership == Ownership::SHARE) {
    assert(false);
  } else {
    auto unupgradedMemberLE =
        regularLoadMember(
            globalState, functionState, builder, &referendStructs, structRefMT, structRef,
            memberIndex, expectedMemberType, targetType, memberName);
    return upgradeLoadResultToRefWithTargetOwnership(
        functionState, builder, expectedMemberType, targetType, unupgradedMemberLE);
  }
}

void Assist::storeMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    bool structKnownLive,
    int memberIndex,
    const std::string& memberName,
    Reference* newMemberRefMT,
    Ref newMemberRef) {
  auto newMemberLE =
      globalState->getRegion(newMemberRefMT)->checkValidReference(
          FL(), functionState, builder, newMemberRefMT, newMemberRef);
  switch (structRefMT->ownership) {
    case Ownership::SHARE:
      assert(false);
    case Ownership::OWN:
    case Ownership::BORROW: {
      storeMemberStrong(
          globalState, functionState, builder, &referendStructs, structRefMT, structRef,
          structKnownLive, memberIndex, memberName, newMemberLE);
      break;
    }
    case Ownership::WEAK: {
      storeMemberWeak(
          globalState, functionState, builder, &referendStructs, structRefMT, structRef,
          structKnownLive, memberIndex, memberName, newMemberLE);
      break;
    }
    default:
      assert(false);
  }
}


// Gets the itable PTR and the new value that we should put into the virtual param's slot
// (such as a void* or a weak void ref)
std::tuple<LLVMValueRef, LLVMValueRef> Assist::explodeInterfaceRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* virtualParamMT,
    Ref virtualArgRef) {
  switch (virtualParamMT->ownership) {
    case Ownership::SHARE:
      assert(false);
    case Ownership::OWN:
    case Ownership::BORROW: {
      return explodeStrongInterfaceRef(
          globalState, functionState, builder, &referendStructs, virtualParamMT, virtualArgRef);
    }
    case Ownership::WEAK: {
      return explodeWeakInterfaceRef(
          globalState, functionState, builder, &referendStructs, &fatWeaks, &weakRefStructs,
          virtualParamMT, virtualArgRef,
          [this, functionState, builder, virtualParamMT](WeakFatPtrLE weakFatPtrLE) {
            return wrcWeaks.weakInterfaceRefToWeakStructRef(
                functionState, builder, virtualParamMT, weakFatPtrLE);
          });
    }
    default:
      assert(false);
  }
}

void Assist::aliasWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  return wrcWeaks.aliasWeakRef(from, functionState, builder, weakRefMT, weakRef);
}

void Assist::discardWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  return wrcWeaks.discardWeakRef(from, functionState, builder, weakRefMT, weakRef);
}

Ref Assist::getIsAliveFromWeakRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefM,
    Ref weakRef,
    bool knownLive) {
  return wrcWeaks.getIsAliveFromWeakRef(functionState, builder, weakRefM, weakRef);
}

LLVMValueRef Assist::getStringBytesPtr(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  auto strWrapperPtrLE =
      referendStructs.makeWrapperPtr(
          FL(), functionState, builder,
          globalState->metalCache->strRef,
          checkValidReference(
              FL(), functionState, builder, globalState->metalCache->strRef, ref));
  return referendStructs.getStringBytesPtr(functionState, builder, strWrapperPtrLE);
}

Ref Assist::constructKnownSizeArray(FunctionState *functionState, LLVMBuilderRef builder, Reference *referenceM, KnownSizeArrayT *referendM, const std::vector<Ref> &membersLE) {
  auto resultRef =
      ::constructKnownSizeArray(
          globalState, functionState, builder, referenceM, referendM, membersLE, &referendStructs,
          [this, functionState, referenceM, referendM](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(),
                functionState,
                innerBuilder,
                referenceM->referend,
                referendM->rawArray->mutability,
                controlBlockPtrLE,
                referendM->name->name);
          });
  adjustStrongRc(FL(), globalState, functionState, &referendStructs, builder, resultRef, referenceM, 1);
  return resultRef;
}

Ref Assist::mallocStr(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  assert(false);
}

Ref Assist::allocate(
    Ref regionInstanceRef,
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* desiredReference,
    const std::vector<Ref>& memberRefs) {
  auto structReferend = dynamic_cast<StructReferend*>(desiredReference->referend);
  auto structM = globalState->program->getStruct(structReferend->fullName);
  auto resultRef =
      innerAllocate(
          FL(), globalState, functionState, builder, desiredReference, &referendStructs, memberRefs, Weakability::WEAKABLE,
          [this, functionState, desiredReference, structM](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(), functionState, innerBuilder, desiredReference->referend, structM->mutability,
                controlBlockPtrLE, structM->name->name);
          });
  alias(FL(), functionState, builder, desiredReference, resultRef);
  return resultRef;
}

// Doesn't return a constraint ref, returns a raw ref to the wrapper struct.
WrapperPtrLE Assist::lockWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref weakRefLE,
    bool weakRefKnownLive) {
  switch (refM->ownership) {
    case Ownership::OWN:
    case Ownership::SHARE:
    case Ownership::BORROW:
      assert(false);
      break;
    case Ownership::WEAK: {
      auto weakFatPtrLE =
          weakRefStructs.makeWeakFatPtr(
              refM,
              checkValidReference(FL(), functionState, builder, refM, weakRefLE));
      return referendStructs.makeWrapperPtr(
          FL(), functionState, builder, refM,
          wrcWeaks.lockWrciFatPtr(from, functionState, builder, refM, weakFatPtrLE));
    }
    default:
      assert(false);
      break;
  }
}

Ref Assist::getUnknownSizeArrayLength(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    Ref arrayRef,
    bool arrayKnownLive) {
  return getUnknownSizeArrayLengthStrong(globalState, functionState, builder, &referendStructs, usaRefMT, arrayRef);
}


LLVMValueRef Assist::getCensusObjectId(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref ref) {
  if (refM == globalState->metalCache->intRef) {
    return constI64LE(globalState, -2);
  } else if (refM == globalState->metalCache->boolRef) {
    return constI64LE(globalState, -3);
  } else if (refM == globalState->metalCache->neverRef) {
    return constI64LE(globalState, -4);
  } else if (refM == globalState->metalCache->floatRef) {
    return constI64LE(globalState, -5);
  } else if (refM->location == Location::INLINE) {
    return constI64LE(globalState, -1);
  } else {
    auto controlBlockPtrLE =
        referendStructs.getControlBlockPtr(checkerAFL, functionState, builder, ref, refM);
    auto exprLE =
        referendStructs.getObjIdFromControlBlockPtr(builder, refM->referend, controlBlockPtrLE);
    return exprLE;
  }
}

LLVMValueRef Assist::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref ref) {
  Reference *actualRefM = nullptr;
  LLVMValueRef refLE = nullptr;
  std::tie(actualRefM, refLE) = megaGetRefInnardsForChecking(ref);
  assert(actualRefM == refM);
  assert(refLE != nullptr);
  assert(LLVMTypeOf(refLE) == globalState->getRegion(refM)->translateType(refM));

  if (globalState->opt->census) {
    if (refM->ownership == Ownership::OWN) {
      regularCheckValidReference(checkerAFL, globalState, functionState, builder, &referendStructs, refM, refLE);
    } else if (refM->ownership == Ownership::SHARE) {
      assert(false);
    } else {
      if (refM->ownership == Ownership::BORROW) {
        regularCheckValidReference(checkerAFL, globalState, functionState, builder, &referendStructs, refM, refLE);
      } else if (refM->ownership == Ownership::WEAK) {
        wrcWeaks.buildCheckWeakRef(checkerAFL, functionState, builder, refM, ref);
      } else
        assert(false);
    }
    return refLE;
  } else {
    return refLE;
  }
}

// TODO maybe combine with alias/acquireReference?
// After we load from a local, member, or element, we can feed the result through this
// function to turn it into a desired ownership.
// Example:
// - Can load from an owning ref member to get a constraint ref.
// - Can load from a constraint ref member to get a weak ref.
Ref Assist::upgradeLoadResultToRefWithTargetOwnership(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceType,
    Reference* targetType,
    LoadResult sourceLoad) {
  auto sourceRef = sourceLoad.extractForAliasingInternals();
  auto sourceOwnership = sourceType->ownership;
  auto sourceLocation = sourceType->location;
  auto targetOwnership = targetType->ownership;
  auto targetLocation = targetType->location;
//  assert(sourceLocation == targetLocation); // unimplemented

  if (sourceOwnership == Ownership::SHARE) {
    if (sourceLocation == Location::INLINE) {
      return sourceRef;
    } else {
      return sourceRef;
    }
  } else if (sourceOwnership == Ownership::OWN) {
    if (targetOwnership == Ownership::OWN) {
      // We can never "load" an owning ref from any of these:
      // - We can only get owning refs from locals by unstackifying
      // - We can only get owning refs from structs by destroying
      // - We can only get owning refs from elements by destroying
      // However, we CAN load owning refs by:
      // - Swapping from a local
      // - Swapping from an element
      // - Swapping from a member
      return sourceRef;
    } else if (targetOwnership == Ownership::BORROW) {
      auto resultRef = transmutePtr(globalState, functionState, builder, sourceType, targetType, sourceRef);
      checkValidReference(FL(),
          functionState, builder, targetType, resultRef);
      return resultRef;
    } else if (targetOwnership == Ownership::WEAK) {
      return wrcWeaks.assembleWeakRef(functionState, builder, sourceType, targetType, sourceRef);
    } else {
      assert(false);
    }
  } else if (sourceOwnership == Ownership::BORROW) {
    buildFlare(FL(), globalState, functionState, builder);

    if (targetOwnership == Ownership::OWN) {
      assert(false); // Cant load an owning reference from a constraint ref local.
    } else if (targetOwnership == Ownership::BORROW) {
      return sourceRef;
    } else if (targetOwnership == Ownership::WEAK) {
      // Making a weak ref from a constraint ref local.
      assert(dynamic_cast<StructReferend *>(sourceType->referend) ||
          dynamic_cast<InterfaceReferend *>(sourceType->referend));
      return wrcWeaks.assembleWeakRef(functionState, builder, sourceType, targetType, sourceRef);
    } else {
      assert(false);
    }
  } else if (sourceOwnership == Ownership::WEAK) {
    assert(targetOwnership == Ownership::WEAK);
    return sourceRef;
  } else {
    assert(false);
  }
}

// Returns object ID
void Assist::fillControlBlock(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Referend* referendM,
    Mutability mutability,
    ControlBlockPtrLE controlBlockPtrLE,
    const std::string& typeName) {
  regularFillControlBlock(
      from, globalState, functionState, &referendStructs, builder, referendM, mutability, controlBlockPtrLE,
      typeName, &wrcWeaks);
}

LoadResult Assist::loadElementFromKSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* ksaRefMT,
    KnownSizeArrayT* ksaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef) {
  return regularloadElementFromKSA(
      globalState, functionState, builder, ksaRefMT, ksaMT, arrayRef, arrayKnownLive, indexRef, &referendStructs);
}

LoadResult Assist::loadElementFromUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef) {
  return regularLoadElementFromUSAWithoutUpgrade(
      globalState, functionState, builder, &referendStructs, usaRefMT, usaMT, arrayRef,
      arrayKnownLive, indexRef);
}

Ref Assist::storeElementInUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef,
    Ref elementRef) {
  return regularStoreElementInUSA(
      globalState, functionState, builder, &referendStructs, usaRefMT, usaMT, arrayRef,
      indexRef, elementRef);
}

Ref Assist::upcast(
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructMT,
    StructReferend* sourceStructReferendM,
    Ref sourceRefLE,

    Reference* targetInterfaceTypeM,
    InterfaceReferend* targetInterfaceReferendM) {
  switch (sourceStructMT->ownership) {
    case Ownership::SHARE:
    case Ownership::OWN:
    case Ownership::BORROW: {
      return upcastStrong(globalState, functionState, builder, &referendStructs, sourceStructMT, sourceStructReferendM, sourceRefLE, targetInterfaceTypeM, targetInterfaceReferendM);
    }
    case Ownership::WEAK: {
      return ::upcastWeak(globalState, functionState, builder, &weakRefStructs, sourceStructMT, sourceStructReferendM, sourceRefLE, targetInterfaceTypeM, targetInterfaceReferendM);
    }
    default:
      assert(false);
  }
}


void Assist::deallocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  innerDeallocate(from, globalState, functionState, &referendStructs, builder, refMT, ref);
}

Ref Assist::constructUnknownSizeArrayCountedStruct(
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* usaMT,
    UnknownSizeArrayT* unknownSizeArrayT,
    Reference* generatorType,
    Prototype* generatorMethod,
    Ref generatorRef,
    LLVMTypeRef usaElementLT,
    Ref sizeRef,
    const std::string& typeName) {
  auto usaWrapperPtrLT =
      referendStructs.getUnknownSizeArrayWrapperStruct(unknownSizeArrayT);
  auto resultRef =
      ::constructUnknownSizeArrayCountedStruct(
          globalState, functionState, blockState, builder, &referendStructs, usaMT, unknownSizeArrayT, generatorType, generatorMethod,
          generatorRef, usaWrapperPtrLT, usaElementLT, sizeRef, typeName,
          [this, functionState, unknownSizeArrayT, usaMT, typeName](
              LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(),
                functionState,
                innerBuilder,
                unknownSizeArrayT,
                unknownSizeArrayT->rawArray->mutability,
                controlBlockPtrLE,
                typeName);
          });
  adjustStrongRc(FL(), globalState, functionState, &referendStructs, builder, resultRef, usaMT, 1);
  return resultRef;
}

void Assist::checkInlineStructType(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  auto argLE = checkValidReference(FL(), functionState, builder, refMT, ref);
  auto structReferend = dynamic_cast<StructReferend*>(refMT->referend);
  assert(structReferend);
  assert(LLVMTypeOf(argLE) == referendStructs.getInnerStruct(structReferend));
}

void Assist::generateStructDefsC(std::unordered_map<std::string, std::string>* cByExportedName, StructDefinition* structDefM) {
  if (structDefM->mutability == Mutability::IMMUTABLE) {
    assert(false);
  } else {
    auto baseName = globalState->program->getExportedName(structDefM->referend->fullName);
    auto refTypeName = baseName + "Ref";
    std::stringstream s;
    s << "typedef struct " << refTypeName << " { void* unused; } " << refTypeName << ";" << std::endl;
    cByExportedName->insert(std::make_pair(baseName, s.str()));
  }
}

void Assist::generateInterfaceDefsC(std::unordered_map<std::string, std::string>* cByExportedName, InterfaceDefinition* interfaceDefM) {
  if (interfaceDefM->mutability == Mutability::IMMUTABLE) {
    assert(false);
  } else {
    auto name = globalState->program->getExportedName(interfaceDefM->referend->fullName);
    std::stringstream s;
    s << "typedef struct " << name << "Ref { void* unused1; void* unused2; } " << name << ";";
    cByExportedName->insert(std::make_pair(name, s.str()));
  }
}

std::string Assist::getRefNameC(Reference* refMT) {
  if (refMT->ownership == Ownership::SHARE) {
    assert(false);
  } else if (auto structRefMT = dynamic_cast<StructReferend*>(refMT->referend)) {
    auto structMT = globalState->program->getStruct(structRefMT->fullName);
    auto baseName = globalState->program->getExportedName(structRefMT->fullName);
    if (structMT->mutability == Mutability::MUTABLE) {
      assert(refMT->location != Location::INLINE);
      return baseName + "Ref";
    } else {
      if (refMT->location == Location::INLINE) {
        return baseName + "Inl";
      } else {
        return baseName + "Ref";
      }
    }
  } else if (auto interfaceMT = dynamic_cast<InterfaceReferend*>(refMT->referend)) {
    return globalState->program->getExportedName(interfaceMT->fullName) + "Ref";
  } else {
    assert(false);
  }
}

LLVMTypeRef Assist::getExternalType(
    Reference* refMT) {
  if (refMT->ownership == Ownership::SHARE) {
    assert(false);
  } else {
    if (auto structReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
      if (refMT->location == Location::INLINE) {
        assert(false); // what do we do in this case? malloc an owning thing and send it in?
        // perhaps just disallow sending inl owns?
      }
      return LLVMPointerType(referendStructs.getWrapperStruct(structReferend), 0);
    } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(refMT->referend)) {
      return LLVMPointerType(referendStructs.getInterfaceRefStruct(interfaceReferend), 0);
    } else {
      std::cerr << "Invalid type for extern!" << std::endl;
      assert(false);
    }
  }
  assert(false);
}

Ref Assist::receiveAndDecryptFamiliarReference(
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Reference *sourceRefMT,
    Ref sourceRef) {
  assert(sourceRefMT->ownership != Ownership::SHARE);
  auto sourceRefLE =
      globalState->getRegion(sourceRefMT)
          ->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceRef);

  // When coords contain the region, this line will change.
  auto targetRefMT = sourceRefMT;
  assert(false);

  return wrap(this, targetRefMT, sourceRefLE);
}

LLVMTypeRef Assist::getInterfaceMethodVirtualParamAnyType(Reference* reference) {
  switch (reference->ownership) {
    case Ownership::BORROW:
    case Ownership::OWN:
    case Ownership::SHARE:
      return LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);
    case Ownership::WEAK:
      return mutWeakableStructs.getWeakVoidRefStruct(reference->referend);
    default:
      assert(false);
  }
}

Ref Assist::receiveUnencryptedAlienReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(false);
}

Ref Assist::encryptAndSendFamiliarReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(false);
}
