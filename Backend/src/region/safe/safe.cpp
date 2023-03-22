#include "../common/fatweaks/fatweaks.h"
#include "../common/hgm/hgm.h"
#include "../common/lgtweaks/lgtweaks.h"
#include "../common/wrcweaks/wrcweaks.h"
#include "../../translatetype.h"
#include "../common/common.h"
#include "../../utils/counters.h"
#include "../common/controlblock.h"
#include "../../utils/branch.h"
#include "../common/heap.h"
#include "../../function/expressions/shared/members.h"
#include "../../function/expressions/shared/elements.h"
#include "../../function/expressions/shared/string.h"
#include "safe.h"
#include <sstream>

ControlBlock makeSafeNonWeakableControlBlock(GlobalState* globalState) {
  ControlBlock controlBlock(globalState, LLVMStructCreateNamed(globalState->context, "mutControlBlock"));
  controlBlock.addMember(ControlBlockMember::GENERATION_64B);
  if (globalState->opt->census) {
    controlBlock.addMember(ControlBlockMember::CENSUS_TYPE_STR);
    controlBlock.addMember(ControlBlockMember::CENSUS_OBJ_ID);
  }
  controlBlock.build();
  return controlBlock;
}

ControlBlock makeSafeWeakableControlBlock(GlobalState* globalState) {
  ControlBlock controlBlock(globalState, LLVMStructCreateNamed(globalState->context, "mutControlBlock"));
  controlBlock.addMember(ControlBlockMember::GENERATION_64B);
  // controlBlock.addMember(ControlBlockMember::WEAK_SOMETHING); impl weaks
  if (globalState->opt->census) {
    controlBlock.addMember(ControlBlockMember::CENSUS_TYPE_STR);
    controlBlock.addMember(ControlBlockMember::CENSUS_OBJ_ID);
  }
  controlBlock.build();
  return controlBlock;
}

LLVMTypeRef makeSafeWeakRefHeaderStruct(GlobalState* globalState) {
  auto wrciRefStructL = LLVMStructCreateNamed(globalState->context, "__SafeWeakRef");

  std::vector<LLVMTypeRef> memberTypesL;

  // impl weaks
//  assert(WEAK_REF_HEADER_MEMBER_INDEX_FOR_WRCI == memberTypesL.size());
//  memberTypesL.push_back(LLVMInt32TypeInContext(globalState->context));

  LLVMStructSetBody(wrciRefStructL, memberTypesL.data(), memberTypesL.size(), false);

  return wrciRefStructL;
}

Safe::Safe(GlobalState* globalState_) :
    globalState(globalState_),
    kindStructs(
        globalState,
        makeSafeNonWeakableControlBlock(globalState),
        makeSafeWeakableControlBlock(globalState),
        makeSafeWeakRefHeaderStruct(globalState)),
    fatWeaks(globalState_, &kindStructs) {
  regionKind =
      globalState->metalCache->getStructKind(
          globalState->metalCache->getName(
              globalState->metalCache->builtinPackageCoord, namePrefix + "_Region"));
  regionRefMT =
      globalState->metalCache->getReference(
          Ownership::MUTABLE_BORROW, Location::YONDER, regionKind);
  globalState->regionIdByKind.emplace(regionKind, globalState->metalCache->mutRegionId);
  kindStructs.declareStruct(regionKind, Weakability::NON_WEAKABLE);
  kindStructs.defineStruct(regionKind, {
      // This region doesnt need anything
  });
}

Reference* Safe::getRegionRefType() {
  return regionRefMT;
}

void Safe::mainSetup(FunctionState* functionState, LLVMBuilderRef builder) {
//  wrcWeaks.mainSetup(functionState, builder);
}

void Safe::mainCleanup(FunctionState* functionState, LLVMBuilderRef builder) {
//  wrcWeaks.mainCleanup(functionState, builder);
}

RegionId* Safe::getRegionId() {
  return globalState->metalCache->mutRegionId;
}

LiveRef Safe::constructStaticSizedArray(
    Ref regionInstanceRef,
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Reference *referenceM,
    StaticSizedArrayT *kindM) {
  auto ssaDef = globalState->program->getStaticSizedArray(kindM);
  auto resultRef =
      ::constructStaticSizedArray(
          globalState, functionState, builder, referenceM, kindM, &kindStructs,
          [this, functionState, referenceM, kindM](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(),
                functionState,
                innerBuilder,
                referenceM->kind,
                controlBlockPtrLE,
                kindM->name->name);
          });
  return resultRef;
}

Ref Safe::mallocStr(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE,
    LLVMValueRef sourceCharsPtrLE) {
  assert(false);
  exit(1);
}

Ref Safe::allocate(
    Ref regionInstanceRef,
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* desiredReference,
    const std::vector<Ref>& memberRefs) {
  auto structKind = dynamic_cast<StructKind*>(desiredReference->kind);
  auto structM = globalState->program->getStruct(structKind);
  auto resultRef =
      innerAllocate(
          FL(), globalState, functionState, builder, desiredReference, &kindStructs, memberRefs, Weakability::WEAKABLE,
          [this, functionState, desiredReference, structM](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(), functionState, innerBuilder, desiredReference->kind,
                controlBlockPtrLE, structM->name->name);
          });
  return resultRef;
}

void Safe::alias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    Ref expr) {
  auto sourceRnd = sourceRef->kind;

  if (dynamic_cast<Int *>(sourceRnd) ||
      dynamic_cast<Bool *>(sourceRnd) ||
      dynamic_cast<Float *>(sourceRnd) ||
      dynamic_cast<Void *>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (dynamic_cast<InterfaceKind *>(sourceRnd) ||
             dynamic_cast<StructKind *>(sourceRnd) ||
             dynamic_cast<StaticSizedArrayT *>(sourceRnd) ||
             dynamic_cast<RuntimeSizedArrayT *>(sourceRnd) ||
             dynamic_cast<Str *>(sourceRnd)) {
    if (sourceRef->ownership == Ownership::OWN) {
      // We might be loading a member as an own if we're destructuring.
      // Don't adjust the RC, since we're only moving it.
    } else if (sourceRef->ownership == Ownership::MUTABLE_BORROW || sourceRef->ownership == Ownership::IMMUTABLE_BORROW) {
      // Do nothing, fast mode doesn't do stuff for borrow refs.
    } else if (sourceRef->ownership == Ownership::WEAK) {
      aliasWeakRef(from, functionState, builder, sourceRef, expr);
    } else if (sourceRef->ownership == Ownership::MUTABLE_SHARE || sourceRef->ownership == Ownership::IMMUTABLE_SHARE) {
      if (sourceRef->location == Location::INLINE) {
        // Do nothing, we can just let inline structs disappear
      } else {
        adjustStrongRc(from, globalState, functionState, &kindStructs, builder, expr, sourceRef, 1);
      }
    } else
      assert(false);
  } else {
    std::cerr << "Unimplemented type in acquireReference: "
              << typeid(*sourceRef->kind).name() << std::endl;
    assert(false);
  }
}

void Safe::dealias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  auto sourceRnd = sourceMT->kind;

  if (sourceMT->ownership == Ownership::MUTABLE_SHARE || sourceMT->ownership == Ownership::IMMUTABLE_SHARE) {
    assert(false);
  } else {
    if (sourceMT->ownership == Ownership::OWN) {
      // This can happen if we're sending an owning reference to the outside world, see DEPAR.
    } else if (sourceMT->ownership == Ownership::MUTABLE_BORROW || sourceMT->ownership == Ownership::IMMUTABLE_BORROW) {
      // Do nothing!
    } else if (sourceMT->ownership == Ownership::WEAK) {
      discardWeakRef(from, functionState, builder, sourceMT, sourceRef);
    } else assert(false);
  }
}

Ref Safe::weakAlias(FunctionState* functionState, LLVMBuilderRef builder, Reference* sourceRefMT, Reference* targetRefMT, Ref sourceRef) {
  assert(false);
//  return regularWeakAlias(globalState, functionState, &kindStructs, &wrcWeaks, builder, sourceRefMT, targetRefMT, sourceRef);
}

// Doesn't return a constraint ref, returns a raw ref to the wrapper struct.
WrapperPtrLE Safe::lockWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref weakRefLE,
    bool weakRefKnownLive) {
  assert(false);
//  switch (refM->ownership) {
//    case Ownership::OWN:
//    case Ownership::MUTABLE_SHARE:
//    case Ownership::IMMUTABLE_SHARE:
//    case Ownership::MUTABLE_BORROW:
//    case Ownership::IMMUTABLE_BORROW:
//      assert(false);
//      break;
//    case Ownership::WEAK: {
//      auto weakFatPtrLE =
//          kindStructs.makeWeakFatPtr(
//              refM,
//              checkValidReference(FL(), functionState, builder, false, refM, weakRefLE));
//      return kindStructs.makeWrapperPtr(
//          FL(), functionState, builder, refM,
//          wrcWeaks.lockWrciFatPtr(from, functionState, builder, refM, weakFatPtrLE));
//    }
//    default:
//      assert(false);
//      break;
//  }
}

Ref Safe::lockWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    bool thenResultIsNever,
    bool elseResultIsNever,
    Reference* resultOptTypeM,
    Reference* constraintRefM,
    Reference* sourceWeakRefMT,
    Ref sourceWeakRefLE,
    bool weakRefKnownLive,
    std::function<Ref(LLVMBuilderRef, Ref)> buildThen,
    std::function<Ref(LLVMBuilderRef)> buildElse) {

  assert(sourceWeakRefMT->ownership == Ownership::WEAK);
  auto isAliveLE =
      getIsAliveFromWeakRef(
          functionState, builder, sourceWeakRefMT, sourceWeakRefLE, weakRefKnownLive);
  auto resultOptTypeLE = globalState->getRegion(resultOptTypeM)->translateType(resultOptTypeM);
  return regularInnerLockWeak(
      globalState, functionState, builder, thenResultIsNever, elseResultIsNever, resultOptTypeM,
      constraintRefM, sourceWeakRefMT, sourceWeakRefLE, buildThen, buildElse,
      isAliveLE, resultOptTypeLE, &kindStructs, &fatWeaks);
}


Ref Safe::asSubtype(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* resultOptTypeM,
    Reference* sourceInterfaceRefMT,
    Ref sourceInterfaceRef,
    bool sourceRefKnownLive,
    Kind* targetKind,
    std::function<Ref(LLVMBuilderRef, Ref)> buildThen,
    std::function<Ref(LLVMBuilderRef)> buildElse) {

  return regularDowncast(
      globalState, functionState, builder, &kindStructs, resultOptTypeM,
      sourceInterfaceRefMT, sourceInterfaceRef, sourceRefKnownLive, targetKind, buildThen, buildElse);
}

LLVMTypeRef Safe::translateType(Reference* referenceM) {
  if (referenceM == regionRefMT) {
    // We just have a raw pointer to region structs
    return LLVMPointerType(kindStructs.getStructInnerStruct(regionKind), 0);
  }
  switch (referenceM->ownership) {
    case Ownership::MUTABLE_SHARE:
    case Ownership::IMMUTABLE_SHARE:
      assert(false);
    case Ownership::OWN:
    case Ownership::MUTABLE_BORROW:
    case Ownership::IMMUTABLE_BORROW:
      assert(referenceM->location != Location::INLINE);
      return translateReferenceSimple(globalState, &kindStructs, referenceM->kind);
    case Ownership::WEAK:
      assert(referenceM->location != Location::INLINE);
      return translateWeakReference(globalState, &kindStructs, referenceM->kind);
    default:
      assert(false);
  }
}

Ref Safe::upcastWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    WeakFatPtrLE sourceRefLE,
    StructKind* sourceStructKindM,
    Reference* sourceStructTypeM,
    InterfaceKind* targetInterfaceKindM,
    Reference* targetInterfaceTypeM) {
  assert(false);
//  auto resultWeakInterfaceFatPtr =
//      wrcWeaks.weakStructPtrToWrciWeakInterfacePtr(
//          globalState, functionState, builder, sourceRefLE, sourceStructKindM,
//          sourceStructTypeM, targetInterfaceKindM, targetInterfaceTypeM);
//  return wrap(this, targetInterfaceTypeM, resultWeakInterfaceFatPtr);
}

void Safe::declareStaticSizedArray(
    StaticSizedArrayDefinitionT* staticSizedArrayMT) {
  globalState->regionIdByKind.emplace(staticSizedArrayMT->kind, getRegionId());

  kindStructs.declareStaticSizedArray(staticSizedArrayMT->kind, Weakability::NON_WEAKABLE);
}

void Safe::declareRuntimeSizedArray(
    RuntimeSizedArrayDefinitionT* runtimeSizedArrayMT) {
  globalState->regionIdByKind.emplace(runtimeSizedArrayMT->kind, getRegionId());

  kindStructs.declareRuntimeSizedArray(runtimeSizedArrayMT->kind, Weakability::NON_WEAKABLE);
}

void Safe::defineRuntimeSizedArray(
    RuntimeSizedArrayDefinitionT* runtimeSizedArrayMT) {
  auto elementLT =
      globalState->getRegion(runtimeSizedArrayMT->elementType)
          ->translateType(runtimeSizedArrayMT->elementType);
  kindStructs.defineRuntimeSizedArray(runtimeSizedArrayMT, elementLT, true);
}

void Safe::defineStaticSizedArray(
    StaticSizedArrayDefinitionT* staticSizedArrayMT) {
  auto elementLT =
      globalState->getRegion(staticSizedArrayMT->elementType)
          ->translateType(staticSizedArrayMT->elementType);
  kindStructs.defineStaticSizedArray(staticSizedArrayMT, elementLT);
}

void Safe::declareStruct(
    StructDefinition* structM) {
  globalState->regionIdByKind.emplace(structM->kind, getRegionId());

  kindStructs.declareStruct(structM->kind, structM->weakability);
}

void Safe::defineStruct(
    StructDefinition* structM) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        globalState->getRegion(structM->members[i]->type)
            ->translateType(structM->members[i]->type));
  }
  kindStructs.defineStruct(structM->kind, innerStructMemberTypesL);
}

void Safe::declareEdge(
    Edge* edge) {
  kindStructs.declareEdge(edge);
}

void Safe::defineEdge(
    Edge* edge) {
  auto interfaceFunctionsLT = globalState->getInterfaceFunctionTypes(edge->interfaceName);
  auto edgeFunctionsL = globalState->getEdgeFunctions(edge);
  kindStructs.defineEdge(edge, interfaceFunctionsLT, edgeFunctionsL);
}

void Safe::declareInterface(
    InterfaceDefinition* interfaceM) {
  globalState->regionIdByKind.emplace(interfaceM->kind, getRegionId());

  kindStructs.declareInterface(interfaceM->kind, interfaceM->weakability);
}

void Safe::defineInterface(
    InterfaceDefinition* interfaceM) {
  auto interfaceMethodTypesL = globalState->getInterfaceFunctionTypes(interfaceM->kind);
  kindStructs.defineInterface(interfaceM, interfaceMethodTypesL);
}

void Safe::discardOwningRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    LiveRef sourceRef) {
  // Free it!
  deallocate(AFL("discardOwningRef"), functionState, builder, sourceMT, sourceRef);
}

void Safe::noteWeakableDestroyed(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    ControlBlockPtrLE controlBlockPtrLE) {
  assert(false);
//  // In fast mode, only shared things are strong RC'd
//  if (refM->ownership == Ownership::MUTABLE_SHARE || refM->ownership == Ownership::IMMUTABLE_SHARE) {
//    assert(false);
////    // Only shared stuff is RC'd in fast mode
////    auto rcIsZeroLE = strongRcIsZero(globalState, &kindStructs, builder, refM, controlBlockPtrLE);
////    buildAssertV(globalState, functionState, builder, rcIsZeroLE,
////        "Tried to free concrete that had nonzero RC!");
//  } else {
//    // It's a mutable, so mark WRCs dead
//
//    if (auto structKindM = dynamic_cast<StructKind *>(refM->kind)) {
//      auto structM = globalState->program->getStruct(structKindM);
//      if (structM->weakability == Weakability::WEAKABLE) {
//        wrcWeaks.innerNoteWeakableDestroyed(functionState, builder, refM, controlBlockPtrLE);
//      }
//    } else if (auto interfaceKindM = dynamic_cast<InterfaceKind *>(refM->kind)) {
//      auto interfaceM = globalState->program->getInterface(interfaceKindM);
//      if (interfaceM->weakability == Weakability::WEAKABLE) {
//        wrcWeaks.innerNoteWeakableDestroyed(functionState, builder, refM, controlBlockPtrLE);
//      }
//    } else {
//      // Do nothing, only structs and interfaces are weakable in assist mode.
//    }
//  }
}

void Safe::storeMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* structRefMT,
    LiveRef structRef,
    int memberIndex,
    const std::string& memberName,
    Reference* newMemberRefMT,
    Ref newMemberRef) {
  auto newMemberLE =
      globalState->getRegion(newMemberRefMT)->checkValidReference(
          FL(), functionState, builder, false, newMemberRefMT, newMemberRef);
  switch (structRefMT->ownership) {
    case Ownership::OWN:
    case Ownership::MUTABLE_SHARE:
    case Ownership::IMMUTABLE_SHARE:
    case Ownership::MUTABLE_BORROW:
    case Ownership::IMMUTABLE_BORROW: {
      storeMemberStrong(
          globalState, functionState, builder, &kindStructs, structRefMT, structRef,
          memberIndex, memberName, newMemberLE);
      break;
    }
    case Ownership::WEAK: {
      storeMemberWeak(
          globalState, functionState, builder, &kindStructs, structRefMT, structRef,
          memberIndex, memberName, newMemberLE);
      break;
    }
    default:
      assert(false);
  }
}

// Gets the itable PTR and the new value that we should put into the virtual param's slot
// (such as a void* or a weak void ref)
std::tuple<LLVMValueRef, LLVMValueRef> Safe::explodeInterfaceRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* virtualParamMT,
    Ref virtualArgRef) {
//  switch (virtualParamMT->ownership) {
//    case Ownership::OWN:
//    case Ownership::MUTABLE_BORROW:
//    case Ownership::IMMUTABLE_BORROW:
//    case Ownership::MUTABLE_SHARE:
//    case Ownership::IMMUTABLE_SHARE: {
//      return explodeStrongInterfaceRef(
//          globalState, functionState, builder, &kindStructs, virtualParamMT, virtualArgRef);
//    }
//    case Ownership::WEAK: {
//      return explodeWeakInterfaceRef(
//          globalState, functionState, builder, &kindStructs, &fatWeaks, &kindStructs,
//          virtualParamMT, virtualArgRef,
//          [this, functionState, builder, virtualParamMT](WeakFatPtrLE weakFatPtrLE) {
//            return wrcWeaks.weakInterfaceRefToWeakStructRef(
//                functionState, builder, virtualParamMT, weakFatPtrLE);
//          });
//    }
//    default:
//      assert(false);
//  }
}

Ref Safe::getRuntimeSizedArrayLength(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* rsaRefMT,
    LiveRef arrayRef) {
  return getRuntimeSizedArrayLengthStrong(globalState, functionState, builder, &kindStructs, rsaRefMT, arrayRef);
}

Ref Safe::getRuntimeSizedArrayCapacity(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* rsaRefMT,
    LiveRef arrayRef) {
  return getRuntimeSizedArrayCapacityStrong(globalState, functionState, builder, &kindStructs, rsaRefMT, arrayRef);
}

LLVMValueRef Safe::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    bool expectLive,
    Reference* refM,
    Ref ref) {
  Reference *actualRefM = nullptr;
  LLVMValueRef refLE = nullptr;
  std::tie(actualRefM, refLE) = megaGetRefInnardsForChecking(ref);
  assert(actualRefM == refM);
  assert(refLE != nullptr);
  assert(LLVMTypeOf(refLE) == translateType(refM));

  if (refM->ownership == Ownership::OWN) {
    regularCheckValidReference(checkerAFL, globalState, functionState, builder, &kindStructs, refM, refLE);
  } else if (refM->ownership == Ownership::MUTABLE_SHARE || refM->ownership == Ownership::IMMUTABLE_SHARE) {
    assert(false);
  } else {
    if (refM->ownership == Ownership::MUTABLE_BORROW || refM->ownership == Ownership::IMMUTABLE_BORROW) {
      regularCheckValidReference(checkerAFL, globalState, functionState, builder,
          &kindStructs, refM, refLE);
    } else if (refM->ownership == Ownership::WEAK) {
      assert(false);
//      wrcWeaks.buildCheckWeakRef(checkerAFL, functionState, builder, refM, ref);
    } else
      assert(false);
  }
  return refLE;
}

// TODO maybe combine with alias/acquireReference?
// After we load from a local, member, or element, we can feed the result through this
// function to turn it into a desired ownership.
// Example:
// - Can load from an owning ref member to get a constraint ref.
// - Can load from a constraint ref member to get a weak ref.
Ref Safe::upgradeLoadResultToRefWithTargetOwnership(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceType,
    Reference* targetType,
    LoadResult sourceLoadResult,
    bool resultKnownLive) {
  auto sourceRef = sourceLoadResult.extractForAliasingInternals();
  auto sourceOwnership = sourceType->ownership;
  auto sourceLocation = sourceType->location;
  auto targetOwnership = targetType->ownership;
  auto targetLocation = targetType->location;
//  assert(sourceLocation == targetLocation); // unimplemented

  if (sourceOwnership == Ownership::MUTABLE_SHARE || sourceOwnership == Ownership::IMMUTABLE_SHARE) {
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
    } else if (targetOwnership == Ownership::MUTABLE_BORROW || targetOwnership == Ownership::IMMUTABLE_BORROW) {
      auto resultRef = transmutePtr(globalState, functionState, builder, false, sourceType, targetType, sourceRef);
      checkValidReference(FL(), functionState, builder, false, targetType, resultRef);
      return resultRef;
    } else if (targetOwnership == Ownership::WEAK) {
      assert(false);
//      return wrcWeaks.assembleWeakRef(functionState, builder, sourceType, targetType, sourceRef);
    } else {
      assert(false);
    }
  } else if (sourceOwnership == Ownership::MUTABLE_BORROW || sourceOwnership == Ownership::IMMUTABLE_BORROW) {
    buildFlare(FL(), globalState, functionState, builder);

    if (targetOwnership == Ownership::OWN) {
      assert(false); // Cant load an owning reference from a constraint ref local.
    } else if (targetOwnership == Ownership::MUTABLE_BORROW || targetOwnership == Ownership::IMMUTABLE_BORROW) {
      return sourceRef;
    } else if (targetOwnership == Ownership::WEAK) {
      // Making a weak ref from a constraint ref local.
      assert(dynamic_cast<StructKind*>(sourceType->kind) || dynamic_cast<InterfaceKind*>(sourceType->kind));
      assert(false);
//      return wrcWeaks.assembleWeakRef(functionState, builder, sourceType, targetType, sourceRef);
    } else {
      assert(false);
    }
  } else if (sourceOwnership == Ownership::WEAK) {
    assert(targetOwnership == Ownership::WEAK);
    return sourceRef;
  } else {
    assert(false);
  }
  assert(false);
}

void Safe::aliasWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  assert(false);
//  return wrcWeaks.aliasWeakRef(from, functionState, builder, weakRefMT, weakRef);
}

void Safe::discardWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  assert(false);
//  return wrcWeaks.discardWeakRef(from, functionState, builder, weakRefMT, weakRef);
}

LLVMValueRef Safe::getCensusObjectId(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref ref) {
  auto controlBlockPtrLE =
      kindStructs.getControlBlockPtr(checkerAFL, functionState, builder, ref, refM);
  return kindStructs.getObjIdFromControlBlockPtr(builder, refM->kind, controlBlockPtrLE);
}

Ref Safe::getIsAliveFromWeakRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefM,
    Ref weakRef,
    bool knownLive) {
  assert(false);
//  return wrcWeaks.getIsAliveFromWeakRef(functionState, builder, weakRefM, weakRef);
}

// Returns object ID
void Safe::fillControlBlock(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Kind* kindM,
    ControlBlockPtrLE controlBlockPtrLE,
    const std::string& typeName) {

  LLVMValueRef newControlBlockLE = LLVMGetUndef(kindStructs.getControlBlock(kindM)->getStruct());

  newControlBlockLE =
      fillControlBlockCensusFields(
          from, globalState, functionState, &kindStructs, builder, kindM, newControlBlockLE, typeName);

  if (globalState->getKindWeakability(kindM) == Weakability::WEAKABLE) {
    assert(false);
//    newControlBlockLE = wrcWeaks.fillWeakableControlBlock(functionState, builder, &kindStructs, kindM,
//        newControlBlockLE);
  }

  LLVMBuildStore(
      builder,
      newControlBlockLE,
      controlBlockPtrLE.refLE);
}

LoadResult Safe::loadElementFromSSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* ssaRefMT,
    StaticSizedArrayT* ssaMT,
    LiveRef arrayRef,
    Ref indexRef) {
  auto ssaDef = globalState->program->getStaticSizedArray(ssaMT);
  return regularloadElementFromSSA(
      globalState, functionState, builder, ssaRefMT, ssaMT, ssaDef->elementType, ssaDef->size, ssaDef->mutability, arrayRef, indexRef, &kindStructs);
}

LoadResult Safe::loadElementFromRSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* rsaRefMT,
    RuntimeSizedArrayT* rsaMT,
    LiveRef arrayRef,
    Ref indexRef) {
  auto rsaDef = globalState->program->getRuntimeSizedArray(rsaMT);
  return regularLoadElementFromRSAWithoutUpgrade(
      globalState, functionState, builder, &kindStructs, true, rsaRefMT, rsaMT, rsaDef->mutability, rsaDef->elementType, arrayRef, indexRef);
}

Ref Safe::storeElementInRSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* rsaRefMT,
    RuntimeSizedArrayT* rsaMT,
    LiveRef arrayRef,
    Ref indexRef,
    Ref elementRef) {
  auto rsaDef = globalState->program->getRuntimeSizedArray(rsaMT);
  auto arrayWrapperPtrLE =
      kindStructs.makeWrapperPtr(
          FL(), functionState, builder, rsaRefMT,
          globalState->getRegion(rsaRefMT)
              ->checkValidReference(FL(), functionState, builder, true, rsaRefMT, arrayRef.inner));
  auto sizeRef = ::getRuntimeSizedArrayLength(globalState, functionState, builder, arrayWrapperPtrLE);
  auto arrayElementsPtrLE = getRuntimeSizedArrayContentsPtr(builder, true, arrayWrapperPtrLE);
  buildFlare(FL(), globalState, functionState, builder);
  return ::swapElement(
      globalState, functionState, builder, rsaRefMT->location, rsaDef->elementType, sizeRef, arrayElementsPtrLE, indexRef, elementRef);
}

Ref Safe::upcast(
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructMT,
    StructKind* sourceStructKindM,
    Ref sourceRefLE,

    Reference* targetInterfaceTypeM,
    InterfaceKind* targetInterfaceKindM) {

  switch (sourceStructMT->ownership) {
    case Ownership::MUTABLE_SHARE:
    case Ownership::IMMUTABLE_SHARE:
    case Ownership::OWN:
    case Ownership::MUTABLE_BORROW:
    case Ownership::IMMUTABLE_BORROW: {
      return upcastStrong(globalState, functionState, builder, &kindStructs, sourceStructMT, sourceStructKindM, sourceRefLE, targetInterfaceTypeM, targetInterfaceKindM);
    }
    case Ownership::WEAK: {
      return ::upcastWeak(globalState, functionState, builder, &kindStructs, sourceStructMT, sourceStructKindM, sourceRefLE, targetInterfaceTypeM, targetInterfaceKindM);
    }
    default:
      assert(false);
  }
}


void Safe::deallocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    LiveRef ref) {
  innerDeallocate(from, globalState, functionState, &kindStructs, builder, refMT, ref);
}

LiveRef Safe::constructRuntimeSizedArray(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* rsaMT,
    RuntimeSizedArrayT* runtimeSizedArrayT,
    Ref capacityRef,
    const std::string& typeName) {
  auto rsaWrapperPtrLT =
      kindStructs.getRuntimeSizedArrayWrapperStruct(runtimeSizedArrayT);
  auto rsaDef = globalState->program->getRuntimeSizedArray(runtimeSizedArrayT);
  auto elementType = globalState->program->getRuntimeSizedArray(runtimeSizedArrayT)->elementType;
  auto rsaElementLT = globalState->getRegion(elementType)->translateType(elementType);
  auto resultRef =
      ::constructRuntimeSizedArray(
          globalState, functionState, builder, &kindStructs, rsaMT, rsaDef->elementType, runtimeSizedArrayT,
          rsaWrapperPtrLT, rsaElementLT, globalState->constI32(0), capacityRef, true, typeName,
          [this, functionState, runtimeSizedArrayT, rsaMT, typeName](
              LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(),
                functionState,
                innerBuilder,
                runtimeSizedArrayT,
                controlBlockPtrLE,
                typeName);
          });
  return resultRef;
}

Ref Safe::loadMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* structRefMT,
    LiveRef structRef,
    int memberIndex,
    Reference* expectedMemberType,
    Reference* targetType,
    const std::string& memberName) {

  if (structRefMT->ownership == Ownership::MUTABLE_SHARE || structRefMT->ownership == Ownership::IMMUTABLE_SHARE) {
    assert(false);
  } else {
    auto unupgradedMemberLE =
        regularLoadMember(
            globalState, functionState, builder, &kindStructs, structRefMT, structRef,
            memberIndex, expectedMemberType, targetType, memberName);
    return upgradeLoadResultToRefWithTargetOwnership(
        functionState, builder, expectedMemberType, targetType, unupgradedMemberLE, false);
  }
}

void Safe::checkInlineStructType(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  auto argLE = checkValidReference(FL(), functionState, builder, false, refMT, ref);
  auto structKind = dynamic_cast<StructKind*>(refMT->kind);
  assert(structKind);
  assert(LLVMTypeOf(argLE) == kindStructs.getStructInnerStruct(structKind));
}


std::string Safe::generateRuntimeSizedArrayDefsC(
    Package* currentPackage,
    RuntimeSizedArrayDefinitionT* rsaDefM) {
  assert(rsaDefM->mutability == Mutability::MUTABLE);
  return generateUniversalRefStructDefC(currentPackage, currentPackage->getKindExportName(rsaDefM->kind, true));
}

std::string Safe::generateStaticSizedArrayDefsC(
    Package* currentPackage,
    StaticSizedArrayDefinitionT* ssaDefM) {
  assert(ssaDefM->mutability == Mutability::MUTABLE);
  return generateUniversalRefStructDefC(currentPackage, currentPackage->getKindExportName(ssaDefM->kind, true));
}

std::string Safe::generateStructDefsC(
    Package* currentPackage, StructDefinition* structDefM) {
  assert(structDefM->mutability == Mutability::MUTABLE);
  return generateUniversalRefStructDefC(currentPackage, currentPackage->getKindExportName(structDefM->kind, true));
}

std::string Safe::generateInterfaceDefsC(
    Package* currentPackage, InterfaceDefinition* interfaceDefM) {
  assert(interfaceDefM->mutability == Mutability::MUTABLE);
  return generateUniversalRefStructDefC(currentPackage, currentPackage->getKindExportName(interfaceDefM->kind, true));
}


LLVMTypeRef Safe::getExternalType(Reference* refMT) {
  if (dynamic_cast<StructKind*>(refMT->kind) ||
      dynamic_cast<StaticSizedArrayT*>(refMT->kind) ||
      dynamic_cast<RuntimeSizedArrayT*>(refMT->kind)) {
    return globalState->universalRefCompressedStructLT;
  } else if (dynamic_cast<InterfaceKind*>(refMT->kind)) {
    return globalState->universalRefCompressedStructLT;
  } else {
    assert(false);
  }
}

Ref Safe::receiveAndDecryptFamiliarReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    LLVMValueRef sourceRefLE) {
  assert(sourceRefMT->ownership != Ownership::IMMUTABLE_SHARE);
  return regularReceiveAndDecryptFamiliarReference(
      globalState, functionState, builder, &kindStructs, sourceRefMT, sourceRefLE);
}

LLVMTypeRef Safe::getInterfaceMethodVirtualParamAnyType(Reference* reference) {
  switch (reference->ownership) {
    case Ownership::MUTABLE_BORROW:
    case Ownership::IMMUTABLE_BORROW:
    case Ownership::OWN:
    case Ownership::IMMUTABLE_SHARE:
    case Ownership::MUTABLE_SHARE:
      return LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);
    case Ownership::WEAK:
      return kindStructs.getWeakVoidRefStruct(reference->kind);
    default:
      assert(false);
  }
}

std::pair<Ref, Ref> Safe::receiveUnencryptedAlienReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref sourceRegionInstanceRef,
    Ref targetRegionInstanceRef,
    Reference* sourceRefMT,
    Reference* targetRefMT,
    Ref sourceRef) {
  assert(false);
  exit(1);
}

LLVMValueRef Safe::encryptAndSendFamiliarReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(sourceRefMT->ownership != Ownership::MUTABLE_SHARE);
  assert(sourceRefMT->ownership != Ownership::IMMUTABLE_SHARE);
  return regularEncryptAndSendFamiliarReference(
      globalState, functionState, builder, &kindStructs, sourceRefMT, sourceRef);
}

void Safe::pushRuntimeSizedArrayNoBoundsCheck(
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference *rsaRefMT,
    RuntimeSizedArrayT *rsaMT,
    LiveRef rsaRef,
    Ref indexRef,
    Ref elementRef) {
  auto rsaDef = globalState->program->getRuntimeSizedArray(rsaMT);
  auto arrayWrapperPtrLE =
      kindStructs.makeWrapperPtr(
          FL(), functionState, builder, rsaRefMT,
          globalState->getRegion(rsaRefMT)
              ->checkValidReference(FL(), functionState, builder, true, rsaRefMT, rsaRef.inner));
  auto sizePtrLE = ::getRuntimeSizedArrayLengthPtr(globalState, builder, arrayWrapperPtrLE);
  auto arrayElementsPtrLE = getRuntimeSizedArrayContentsPtr(builder, true, arrayWrapperPtrLE);
  ::initializeElementAndIncrementSize(
      globalState, functionState, builder, rsaRefMT->location, rsaDef->elementType, sizePtrLE, arrayElementsPtrLE,
      indexRef, elementRef);
}

Ref Safe::popRuntimeSizedArrayNoBoundsCheck(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref arrayRegionInstanceRef,
    Reference* rsaRefMT,
    RuntimeSizedArrayT* rsaMT,
    LiveRef arrayRef,
    Ref indexRef) {
  auto rsaDef = globalState->program->getRuntimeSizedArray(rsaMT);
  auto elementLE =
      regularLoadElementFromRSAWithoutUpgrade(
          globalState, functionState, builder, &kindStructs, true, rsaRefMT, rsaMT, rsaDef->mutability, rsaDef->elementType, arrayRef, indexRef).move();
  auto rsaWrapperPtrLE =
      kindStructs.makeWrapperPtr(
          FL(), functionState, builder, rsaRefMT,
          globalState->getRegion(rsaRefMT)
              ->checkValidReference(FL(), functionState, builder, true, rsaRefMT, arrayRef.inner));
  decrementRSASize(globalState, functionState, &kindStructs, builder, rsaRefMT, rsaWrapperPtrLE);
  return elementLE;
}

void Safe::initializeElementInSSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* ssaRefMT,
    StaticSizedArrayT* ssaMT,
    LiveRef arrayRef,
    Ref indexRef,
    Ref elementRef) {
  auto ssaDef = globalState->program->getStaticSizedArray(ssaMT);
  auto arrayWrapperPtrLE =
      kindStructs.makeWrapperPtr(
          FL(), functionState, builder, ssaRefMT,
          globalState->getRegion(ssaRefMT)
              ->checkValidReference(FL(), functionState, builder, true, ssaRefMT, arrayRef.inner));
  auto sizeRef = globalState->constI32(ssaDef->size);
  auto arrayElementsPtrLE = getStaticSizedArrayContentsPtr(builder, arrayWrapperPtrLE);
  ::initializeElementWithoutIncrementSize(
      globalState, functionState, builder, ssaRefMT->location, ssaDef->elementType, sizeRef, arrayElementsPtrLE,
      indexRef, elementRef);
}

Ref Safe::deinitializeElementFromSSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* ssaRefMT,
    StaticSizedArrayT* ssaMT,
    LiveRef arrayRef,
    Ref indexRef) {
  assert(false);
  exit(1);
}

Weakability Safe::getKindWeakability(Kind* kind) {
  if (auto structKind = dynamic_cast<StructKind*>(kind)) {
    return globalState->lookupStruct(structKind)->weakability;
  } else if (auto interfaceKind = dynamic_cast<InterfaceKind*>(kind)) {
    return globalState->lookupInterface(interfaceKind)->weakability;
  } else {
    return Weakability::NON_WEAKABLE;
  }
}

LLVMValueRef Safe::getInterfaceMethodFunctionPtr(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* virtualParamMT,
    Ref virtualArgRef,
    int indexInEdge) {
  return getInterfaceMethodFunctionPtrFromItable(
      globalState, functionState, builder, virtualParamMT, virtualArgRef, indexInEdge);
}

LLVMValueRef Safe::stackify(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Local* local,
    Ref refToStore,
    bool knownLive) {
  auto toStoreLE = checkValidReference(FL(), functionState, builder, false, local->type, refToStore);
  auto typeLT = translateType(local->type);
  return makeBackendLocal(functionState, builder, typeLT, local->id->maybeName.c_str(), toStoreLE);
}

Ref Safe::unstackify(FunctionState* functionState, LLVMBuilderRef builder, Local* local, LLVMValueRef localAddr) {
  return loadLocal(functionState, builder, local, localAddr);
}

Ref Safe::loadLocal(FunctionState* functionState, LLVMBuilderRef builder, Local* local, LLVMValueRef localAddr) {
  return normalLocalLoad(globalState, functionState, builder, local, localAddr);
}

Ref Safe::localStore(FunctionState* functionState, LLVMBuilderRef builder, Local* local, LLVMValueRef localAddr, Ref refToStore, bool knownLive) {
  return normalLocalStore(globalState, functionState, builder, local, localAddr, refToStore);
}

std::string Safe::getExportName(
    Package* package,
    Reference* reference,
    bool includeProjectName) {
  return package->getKindExportName(reference->kind, includeProjectName) + (reference->location == Location::YONDER ? "Ref" : "");
}

Ref Safe::createRegionInstanceLocal(FunctionState* functionState, LLVMBuilderRef builder) {
  auto regionLT = kindStructs.getStructInnerStruct(regionKind);
  auto regionInstancePtrLE =
      makeBackendLocal(functionState, builder, regionLT, "region", LLVMGetUndef(regionLT));
  auto regionInstanceRef = wrap(this, regionRefMT, regionInstancePtrLE);
  return regionInstanceRef;
}

LiveRef Safe::checkRefLive(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* refMT,
    Ref ref,
    bool refKnownLive) {
  // The whole point of unsafe is to get around such notions of liveness, so just return a LiveRef.
  return LiveRef(ref);
}

LiveRef Safe::preCheckBorrow(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* refMT,
    Ref ref,
    bool refKnownLive) {
  // The whole point of unsafe is to get around such notions of liveness, so just return a LiveRef.
  return LiveRef(ref);
}