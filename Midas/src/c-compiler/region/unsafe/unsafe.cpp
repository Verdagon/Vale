#include <region/common/fatweaks/fatweaks.h>
#include <region/common/hgm/hgm.h>
#include <region/common/lgtweaks/lgtweaks.h>
#include <region/common/wrcweaks/wrcweaks.h>
#include <translatetype.h>
#include <region/common/common.h>
#include <utils/counters.h>
#include <region/common/controlblock.h>
#include <utils/branch.h>
#include <region/common/heap.h>
#include <function/expressions/shared/members.h>
#include <function/expressions/shared/elements.h>
#include <function/expressions/shared/string.h>
#include "unsafe.h"
#include <sstream>

Unsafe::Unsafe(GlobalState* globalState_) :
    globalState(globalState_),
    immStructs(globalState, makeImmControlBlock(globalState)),
    mutNonWeakableStructs(globalState, makeFastNonWeakableControlBlock(globalState)),
    mutWeakableStructs(
        globalState,
        makeFastWeakableControlBlock(globalState),
        WrcWeaks::makeWeakRefHeaderStruct(globalState)),
    defaultImmutables(globalState, &immStructs),
    referendStructs(
        globalState,
        [this](Referend* referend) -> IReferendStructsSource* {
          if (globalState->program->getReferendMutability(referend) == Mutability::IMMUTABLE) {
            return &immStructs;
          } else {
            if (globalState->program->getReferendWeakability(referend) == Weakability::NON_WEAKABLE) {
              return &mutNonWeakableStructs;
            } else {
              return &mutWeakableStructs;
            }
          }
        }),
    weakRefStructs(
        [this](Referend* referend) -> IWeakRefStructsSource* {
              if (globalState->program->getReferendMutability(referend) == Mutability::IMMUTABLE) {
                assert(false);
              } else {
                if (globalState->program->getReferendWeakability(referend) == Weakability::NON_WEAKABLE) {
                  assert(false);
                } else {
                  return &mutWeakableStructs;
                }
              }
        }),
    fatWeaks(globalState_, &weakRefStructs),
    wrcWeaks(globalState_, &referendStructs, &weakRefStructs) {
}

Ref Unsafe::constructKnownSizeArray(FunctionState *functionState, LLVMBuilderRef builder, Reference *referenceM, KnownSizeArrayT *referendM, const std::vector<Ref> &membersLE) {
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
  alias(FL(), functionState, builder, referenceM, resultRef);
  return resultRef;
}

WrapperPtrLE Unsafe::mallocStr(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  return defaultImmutables.mallocStr(functionState, builder, lengthLE);
}

Ref Unsafe::allocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* desiredReference,
    const std::vector<Ref>& membersLE) {
  auto structReferend = dynamic_cast<StructReferend*>(desiredReference->referend);
  auto structM = globalState->program->getStruct(structReferend->fullName);
  auto resultRef =
      innerAllocate(
          FL(), globalState, functionState, builder, desiredReference, &referendStructs, membersLE, Weakability::WEAKABLE,
          [this, functionState, desiredReference, structM](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
            fillControlBlock(
                FL(), functionState, innerBuilder, desiredReference->referend, structM->mutability,
                controlBlockPtrLE, structM->name->name);
          });
  alias(FL(), functionState, builder, desiredReference, resultRef);
  return resultRef;
}

void Unsafe::alias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    Ref expr) {
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
      // We might be loading a member as an own if we're destructuring.
      // Don't adjust the RC, since we're only moving it.
    } else if (sourceRef->ownership == Ownership::BORROW) {
      // Do nothing, fast mode doesn't do stuff for borrow refs.
    } else if (sourceRef->ownership == Ownership::WEAK) {
      aliasWeakRef(from, functionState, builder, sourceRef, expr);
    } else if (sourceRef->ownership == Ownership::SHARE) {
      if (sourceRef->location == Location::INLINE) {
        // Do nothing, we can just let inline structs disappear
      } else {
        adjustStrongRc(from, globalState, functionState, &referendStructs, builder, expr, sourceRef, 1);
      }
    } else
      assert(false);
  } else {
    std::cerr << "Unimplemented type in acquireReference: "
        << typeid(*sourceRef->referend).name() << std::endl;
    assert(false);
  }
}

void Unsafe::dealias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  auto sourceRnd = sourceMT->referend;

  if (sourceMT->ownership == Ownership::SHARE) {
    defaultImmutables.discard(
        from, globalState, functionState, builder, sourceMT, sourceRef);
  } else {
    if (sourceMT->ownership == Ownership::OWN) {
      // This can happen if we're sending an owning reference to the outside world, see DEPAR.
    } else if (sourceMT->ownership == Ownership::BORROW) {
      // Do nothing!
    } else if (sourceMT->ownership == Ownership::WEAK) {
      discardWeakRef(from, functionState, builder, sourceMT, sourceRef);
    } else assert(false);
  }
}

Ref Unsafe::weakAlias(FunctionState* functionState, LLVMBuilderRef builder, Reference* sourceRefMT, Reference* targetRefMT, Ref sourceRef) {
  return regularWeakAlias(globalState, functionState, &referendStructs, &wrcWeaks, builder, sourceRefMT, targetRefMT, sourceRef);
}

// Doesn't return a constraint ref, returns a raw ref to the wrapper struct.
WrapperPtrLE Unsafe::lockWeakRef(
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

Ref Unsafe::lockWeak(
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
  auto resultOptTypeLE = translateType(resultOptTypeM);
  return regularInnerLockWeak(
      globalState, functionState, builder, thenResultIsNever, elseResultIsNever, resultOptTypeM,
      constraintRefM, sourceWeakRefMT, sourceWeakRefLE, buildThen, buildElse,
      isAliveLE, resultOptTypeLE, &weakRefStructs, &fatWeaks);
}

LLVMTypeRef Unsafe::translateType(Reference* referenceM) {
  switch (referenceM->ownership) {
    case Ownership::SHARE:
      return defaultImmutables.translateType(globalState, referenceM);
    case Ownership::OWN:
    case Ownership::BORROW:
      assert(referenceM->location != Location::INLINE);
      return translateReferenceSimple(globalState, &referendStructs, referenceM->referend);
    case Ownership::WEAK:
      assert(referenceM->location != Location::INLINE);
      return translateWeakReference(globalState, &weakRefStructs, referenceM->referend);
    default:
      assert(false);
  }
}

Ref Unsafe::upcastWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    WeakFatPtrLE sourceRefLE,
    StructReferend* sourceStructReferendM,
    Reference* sourceStructTypeM,
    InterfaceReferend* targetInterfaceReferendM,
    Reference* targetInterfaceTypeM) {
  auto resultWeakInterfaceFatPtr =
      wrcWeaks.weakStructPtrToWrciWeakInterfacePtr(
          globalState, functionState, builder, sourceRefLE, sourceStructReferendM,
          sourceStructTypeM, targetInterfaceReferendM, targetInterfaceTypeM);
  return wrap(this, targetInterfaceTypeM, resultWeakInterfaceFatPtr);
}

void Unsafe::declareKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  referendStructs.declareKnownSizeArray(knownSizeArrayMT);
}

void Unsafe::declareUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  referendStructs.declareUnknownSizeArray(unknownSizeArrayMT);
}

void Unsafe::translateUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto elementLT =
      translateType(
          unknownSizeArrayMT->rawArray->elementType);
  referendStructs.translateUnknownSizeArray(unknownSizeArrayMT, elementLT);
}

void Unsafe::translateKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto elementLT =
      translateType(
          knownSizeArrayMT->rawArray->elementType);
  referendStructs.translateKnownSizeArray(knownSizeArrayMT, elementLT);
}

void Unsafe::declareStruct(
    StructDefinition* structM) {
  referendStructs.declareStruct(structM);
}

void Unsafe::translateStruct(
    StructDefinition* structM) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        translateType(
            structM->members[i]->type));
  }
  referendStructs.translateStruct(
      structM,
      innerStructMemberTypesL);
}

void Unsafe::declareEdge(
    Edge* edge) {
  referendStructs.declareEdge(edge);
}

void Unsafe::translateEdge(
    Edge* edge) {
  auto interfaceM = globalState->program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    auto interfaceFunctionLT =
        translateInterfaceMethodToFunctionType(this, interfaceM->methods[i]);
    interfaceFunctionsLT.push_back(interfaceFunctionLT);

    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    auto edgeFunctionL = globalState->getFunction(funcName);
    edgeFunctionsL.push_back(edgeFunctionL);
  }
  referendStructs.translateEdge(edge, interfaceFunctionsLT, edgeFunctionsL);
}

void Unsafe::declareInterface(
    InterfaceDefinition* interfaceM) {
  referendStructs.declareInterface(interfaceM);
}

void Unsafe::translateInterface(
    InterfaceDefinition* interfaceM) {
  std::vector<LLVMTypeRef> interfaceMethodTypesL;
  for (int i = 0; i < interfaceM->methods.size(); i++) {
    interfaceMethodTypesL.push_back(
        LLVMPointerType(
            translateInterfaceMethodToFunctionType(this, interfaceM->methods[i]),
            0));
  }
  referendStructs.translateInterface(
      interfaceM,
      interfaceMethodTypesL);
}
//
//LLVMTypeRef Unsafe::translateInterfaceMethodToFunctionType(
//    InterfaceReferend* referend,
//    InterfaceMethod* method) {
//  auto returnMT = method->prototype->returnType;
//  auto paramsMT = method->prototype->params;
//  auto returnLT = translateType(returnMT);
//  auto paramsLT = translateTypes(globalState, this, paramsMT);
//
//  switch (paramsMT[method->virtualParamIndex]->ownership) {
//    case Ownership::BORROW:
//    case Ownership::OWN:
//    case Ownership::SHARE:
//      paramsLT[method->virtualParamIndex] = LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);
//      break;
//    case Ownership::WEAK:
//      paramsLT[method->virtualParamIndex] = weakRefStructs.getWeakVoidRefStruct(referend);
//      break;
//  }
//
//  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
//}
//

void Unsafe::discardOwningRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  // Free it!
  deallocate(AFL("discardOwningRef"), functionState, builder, sourceMT, sourceRef);
}

void Unsafe::noteWeakableDestroyed(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    ControlBlockPtrLE controlBlockPtrLE) {
  // In fast mode, only shared things are strong RC'd
  if (refM->ownership == Ownership::SHARE) {
    // Only shared stuff is RC'd in fast mode
    auto rcIsZeroLE = strongRcIsZero(globalState, &referendStructs, builder, refM, controlBlockPtrLE);
    buildAssert(globalState, functionState, builder, rcIsZeroLE,
        "Tried to free concrete that had nonzero RC!");
  } else {
    // It's a mutable, so mark WRCs dead

    if (auto structReferendM = dynamic_cast<StructReferend *>(refM->referend)) {
      auto structM = globalState->program->getStruct(structReferendM->fullName);
      if (structM->weakability == Weakability::WEAKABLE) {
        wrcWeaks.innerNoteWeakableDestroyed(functionState, builder, refM, controlBlockPtrLE);
      }
    } else if (auto interfaceReferendM = dynamic_cast<InterfaceReferend *>(refM->referend)) {
      auto interfaceM = globalState->program->getStruct(interfaceReferendM->fullName);
      if (interfaceM->weakability == Weakability::WEAKABLE) {
        wrcWeaks.innerNoteWeakableDestroyed(functionState, builder, refM, controlBlockPtrLE);
      }
    } else {
      // Do nothing, only structs and interfaces are weakable in assist mode.
    }
  }
}

void Unsafe::storeMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    bool structKnownLive,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef newValueLE) {
  switch (structRefMT->ownership) {
    case Ownership::OWN:
    case Ownership::SHARE:
    case Ownership::BORROW: {
      storeMemberStrong(
          globalState, functionState, builder, &referendStructs, structRefMT, structRef,
          structKnownLive, memberIndex, memberName, newValueLE);
      break;
    }
    case Ownership::WEAK: {
      storeMemberWeak(
          globalState, functionState, builder, &referendStructs, structRefMT, structRef,
          structKnownLive, memberIndex, memberName, newValueLE);
      break;
    }
    default:
      assert(false);
  }
}

// Gets the itable PTR and the new value that we should put into the virtual param's slot
// (such as a void* or a weak void ref)
std::tuple<LLVMValueRef, LLVMValueRef> Unsafe::explodeInterfaceRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* virtualParamMT,
    Ref virtualArgRef) {
  switch (virtualParamMT->ownership) {
    case Ownership::OWN:
    case Ownership::BORROW:
    case Ownership::SHARE: {
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

Ref Unsafe::getUnknownSizeArrayLength(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    Ref arrayRef,
    bool arrayKnownLive) {
  return getUnknownSizeArrayLengthStrong(globalState, functionState, builder, &referendStructs, usaRefMT, arrayRef);
}

LLVMValueRef Unsafe::checkValidReference(
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
  assert(LLVMTypeOf(refLE) == functionState->defaultRegion->translateType(refM));

  if (refM->ownership == Ownership::OWN) {
    regularCheckValidReference(checkerAFL, globalState, functionState, builder, &referendStructs, refM, refLE);
  } else if (refM->ownership == Ownership::SHARE) {
    defaultImmutables.checkValidReference(checkerAFL, functionState, builder, &referendStructs, refM, refLE);
  } else {
    if (refM->ownership == Ownership::BORROW) {
      regularCheckValidReference(checkerAFL, globalState, functionState, builder,
          &referendStructs, refM, refLE);
    } else if (refM->ownership == Ownership::WEAK) {
      wrcWeaks.buildCheckWeakRef(checkerAFL, functionState, builder, refM, ref);
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
Ref Unsafe::upgradeLoadResultToRefWithTargetOwnership(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceType,
    Reference* targetType,
    LoadResult sourceLoadResult) {
  auto sourceRef = sourceLoadResult.extractForAliasingInternals();
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
      auto resultRef = transmutePtr(functionState, builder, sourceType, targetType, sourceRef);
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
      assert(dynamic_cast<StructReferend*>(sourceType->referend) || dynamic_cast<InterfaceReferend*>(sourceType->referend));
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
  assert(false);
}

void Unsafe::aliasWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  return wrcWeaks.aliasWeakRef(from, functionState, builder, weakRefMT, weakRef);
}

void Unsafe::discardWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  return wrcWeaks.discardWeakRef(from, functionState, builder, weakRefMT, weakRef);
}

LLVMValueRef Unsafe::getCensusObjectId(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref ref) {
  auto controlBlockPtrLE =
      referendStructs.getControlBlockPtr(checkerAFL, functionState, builder, ref, refM);
  return referendStructs.getObjIdFromControlBlockPtr(builder, refM->referend, controlBlockPtrLE);
}

Ref Unsafe::getIsAliveFromWeakRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefM,
    Ref weakRef,
    bool knownLive) {
  return wrcWeaks.getIsAliveFromWeakRef(functionState, builder, weakRefM, weakRef);
}

// Returns object ID
void Unsafe::fillControlBlock(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Referend* referendM,
    Mutability mutability,
    ControlBlockPtrLE controlBlockPtrLE,
    const std::string& typeName) {
  assert(mutability != Mutability::IMMUTABLE); // imms should be handled by DefaultImmutables

  LLVMValueRef newControlBlockLE = LLVMGetUndef(referendStructs.getControlBlock(referendM)->getStruct());

  newControlBlockLE =
      fillControlBlockCensusFields(
          from, globalState, functionState, &referendStructs, builder, referendM, newControlBlockLE, typeName);
  if (globalState->program->getReferendWeakability(referendM) == Weakability::WEAKABLE) {
    newControlBlockLE = wrcWeaks.fillWeakableControlBlock(functionState, builder, &referendStructs, referendM,
        newControlBlockLE);
  }

  LLVMBuildStore(
      builder,
      newControlBlockLE,
      controlBlockPtrLE.refLE);
}

LoadResult Unsafe::loadElementFromKSA(
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

LoadResult Unsafe::loadElementFromUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef) {
  return regularLoadElementFromUSAWithoutUpgrade(globalState, functionState, builder, &referendStructs, usaRefMT, usaMT, arrayRef, arrayKnownLive, indexRef);
}

Ref Unsafe::storeElementInUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef,
    Ref elementRef) {
  return regularStoreElementInUSA(
      globalState, functionState, builder, &referendStructs, usaRefMT, usaMT, arrayRef, indexRef, elementRef);
}

Ref Unsafe::upcast(
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


void Unsafe::deallocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref refLE) {
  innerDeallocate(from, globalState, functionState, &referendStructs, builder, refMT, refLE);
}

Ref Unsafe::constructUnknownSizeArrayCountedStruct(
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
  alias(FL(), functionState, builder, usaMT, resultRef);
  return resultRef;
}

Ref Unsafe::loadMember(
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
    auto memberLE =
        defaultImmutables.loadMember(
            functionState, builder, structRefMT, structRef, memberIndex, expectedMemberType,
            targetType, memberName);
    auto resultRef =
        upgradeLoadResultToRefWithTargetOwnership(
            functionState, builder, expectedMemberType, targetType, memberLE);
    return resultRef;
  } else {
    auto unupgradedMemberLE =
        regularLoadMember(
            globalState, functionState, builder, &referendStructs, structRefMT, structRef,
            memberIndex, expectedMemberType, targetType, memberName);
    return upgradeLoadResultToRefWithTargetOwnership(
        functionState, builder, expectedMemberType, targetType, unupgradedMemberLE);
  }
}

void Unsafe::checkInlineStructType(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref refLE) {
  auto argLE = checkValidReference(FL(), functionState, builder, refMT, refLE);
  auto structReferend = dynamic_cast<StructReferend*>(refMT->referend);
  assert(structReferend);
  assert(LLVMTypeOf(argLE) == referendStructs.getInnerStruct(structReferend));
}


std::string Unsafe::getRefNameC(Reference* refMT) {
  if (refMT->ownership == Ownership::SHARE) {
    return defaultImmutables.getRefNameC(refMT);
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

void Unsafe::generateStructDefsC(
    std::unordered_map<std::string, std::string>* cByExportedName, StructDefinition* structDefM) {
  if (structDefM->mutability == Mutability::IMMUTABLE) {
    return defaultImmutables.generateStructDefsC(cByExportedName, structDefM);
  } else {
    auto baseName = globalState->program->getExportedName(structDefM->referend->fullName);
    auto refTypeName = baseName + "Ref";
    std::stringstream s;
    s << "typedef struct " << refTypeName << " { void* unused; } " << refTypeName << ";" << std::endl;
    cByExportedName->insert(std::make_pair(baseName, s.str()));
  }
}

void Unsafe::generateInterfaceDefsC(
    std::unordered_map<std::string, std::string>* cByExportedName, InterfaceDefinition* interfaceDefM) {
//      return "void* unused; void* unused;";
  assert(false); // impl
}

LLVMTypeRef Unsafe::getExternalType(Reference* refMT) {
  if (refMT->ownership == Ownership::SHARE) {
    return defaultImmutables.getExternalType(refMT);
  } else {
    if (auto structReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
      return LLVMPointerType(referendStructs.getWrapperStruct(structReferend), 0);
    } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(refMT->referend)) {
      assert(false); // impl
    } else {
      std::cerr << "Invalid type for extern!" << std::endl;
      assert(false);
    }
  }
  assert(false);
}

Ref Unsafe::copyAlien(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    IRegion* sourceRegion,
    Reference* sourceRefMT,
    Ref sourceReff) {
  assert(sourceRefMT->ownership == Ownership::SHARE);
  // Someday, when coord contains the region, this line will change.
  auto targetRefMT = sourceRefMT;

  if (targetRefMT->ownership == Ownership::SHARE) {
    auto resultRef =
        defaultImmutables.copyAlien(
            functionState, builder, sourceRegion, this, sourceRefMT, sourceReff);
    checkValidReference(FL(), functionState, builder, sourceRefMT, resultRef);
    return resultRef;
  } else {
    auto sourceLE = sourceRegion->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceReff);
    if (auto structReferend = dynamic_cast<StructReferend *>(sourceRefMT->referend)) {
      assert(false); // impl
      return makeEmptyTupleRef(globalState, functionState, builder);

//    if (sourceRefMT->location == Location::INLINE) {
//      functionState->defaultRegion->checkInlineStructType(functionState, builder, sourceRefMT, ref);
//    } else {
////            std::cerr << "Can only pass inline imm structs between C and Vale currently." << std::endl;
//      assert(false); // impl
//      return nullptr;
//    }
    } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend *>(sourceRefMT->referend)) {

      assert(false); // impl
      return makeEmptyTupleRef(globalState, functionState, builder);
    } else {
      std::cerr << "Invalid type for extern!" << std::endl;
      assert(false);
      return makeEmptyTupleRef(globalState, functionState, builder);
    }
  }

  assert(false);
  return makeEmptyTupleRef(globalState, functionState, builder);
}

Ref Unsafe::welcomeAlienRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    IRegion* sourceRegion,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(sourceRefMT->ownership != Ownership::SHARE);

  // Here, we'll eventually have the source region encrypt its references
  // since we're unsafe and we're welcoming it into our structs and stack
  // and so on. For now, we do nothing.

  return sourceRef;
}

LLVMTypeRef Unsafe::getInterfaceMethodVirtualParamAnyType(Reference* reference) {
  return mutWeakableStructs.getWeakVoidRefStruct(reference->referend);
}

LLVMValueRef Unsafe::getStringBytesPtr(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  auto strWrapperPtrLE =
      referendStructs.makeWrapperPtr(
          FL(), functionState, builder,
          globalState->metalCache.strRef,
          globalState->region->checkValidReference(
              FL(), functionState, builder,
              globalState->metalCache.strRef, ref));
  return referendStructs.getStringBytesPtr(functionState, builder, strWrapperPtrLE);
}
LLVMValueRef Unsafe::getStringLen(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  auto strWrapperPtrLE =
      referendStructs.makeWrapperPtr(
          FL(), functionState, builder,
          globalState->metalCache.strRef,
          globalState->region->checkValidReference(
              FL(), functionState, builder,
              globalState->metalCache.strRef, ref));
  return referendStructs.getStringLen(functionState, builder, strWrapperPtrLE);
}