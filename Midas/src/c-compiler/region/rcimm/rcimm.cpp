#include <function/expressions/shared/shared.h>
#include <utils/counters.h>
#include <utils/branch.h>
#include <region/common/controlblock.h>
#include <region/common/heap.h>
#include <function/expressions/shared/string.h>
#include <region/common/common.h>
#include <sstream>
#include "rcimm.h"
#include "translatetype.h"

void fillControlBlock(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    IReferendStructsSource* structs,
    LLVMBuilderRef builder,
    Referend* referendM,
    ControlBlockPtrLE controlBlockPtrLE,
    const std::string& typeName) {
  LLVMValueRef newControlBlockLE =
      LLVMGetUndef(structs->getControlBlock(referendM)->getStruct());
  newControlBlockLE =
      fillControlBlockCensusFields(
          from, globalState, functionState, structs, builder, referendM, newControlBlockLE, typeName);
  newControlBlockLE =
      insertStrongRc(globalState, builder, structs, referendM, newControlBlockLE);
  LLVMBuildStore(
      builder,
      newControlBlockLE,
      controlBlockPtrLE.refLE);
}

ControlBlock makeImmControlBlock(GlobalState* globalState) {
  ControlBlock controlBlock(globalState, LLVMStructCreateNamed(globalState->context, "immControlBlock"));
  controlBlock.addMember(ControlBlockMember::STRONG_RC);
  // This is where we put the size in the current generational heap, we can use it for something
  // else until we get rid of that.
  controlBlock.addMember(ControlBlockMember::UNUSED_32B);
  if (globalState->opt->census) {
    controlBlock.addMember(ControlBlockMember::CENSUS_TYPE_STR);
    controlBlock.addMember(ControlBlockMember::CENSUS_OBJ_ID);
  }
  controlBlock.build();
  return controlBlock;
}

RCImm::RCImm(GlobalState* globalState_)
  : globalState(globalState_),
    referendStructs(globalState, makeImmControlBlock(globalState)) {
}




void RCImm::alias(
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
    if (sourceRef->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      adjustStrongRc(from, globalState, functionState, &referendStructs, builder, ref, sourceRef, 1);
    }
  } else {
    std::cerr << "Unimplemented type in acquireReference: "
              << typeid(*sourceRef->referend).name() << std::endl;
    assert(false);
  }
}

void RCImm::dealias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  discard(from, globalState, functionState, builder, sourceMT, sourceRef);
}

Ref RCImm::lockWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    bool thenResultIsNever,
    bool elseResultIsNever,
    Reference* resultOptTypeM,
//      LLVMTypeRef resultOptTypeL,
    Reference* constraintRefM,
    Reference* sourceWeakRefMT,
    Ref sourceWeakRefLE,
    bool weakRefKnownLive,
    std::function<Ref(LLVMBuilderRef, Ref)> buildThen,
    std::function<Ref(LLVMBuilderRef)> buildElse) {
  assert(false);
}

LLVMTypeRef RCImm::translateType(Reference* referenceM) {
  return translateType(globalState, referenceM);
}

LLVMValueRef RCImm::getCensusObjectId(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref refLE) {
  if (refM == globalState->metalCache.intRef) {
    return constI64LE(globalState, -2);
  } else if (refM == globalState->metalCache.boolRef) {
    return constI64LE(globalState, -3);
  } else if (refM == globalState->metalCache.neverRef) {
    return constI64LE(globalState, -4);
  } else if (refM == globalState->metalCache.floatRef) {
    return constI64LE(globalState, -5);
  } else if (refM->location == Location::INLINE) {
    return constI64LE(globalState, -1);
  } else {
    auto controlBlockPtrLE =
        referendStructs.getControlBlockPtr(checkerAFL, functionState, builder, refLE, refM);
    auto exprLE =
        referendStructs.getObjIdFromControlBlockPtr(builder, refM->referend, controlBlockPtrLE);
    return exprLE;
  }
}

Ref RCImm::upcastWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    WeakFatPtrLE sourceRefLE,
    StructReferend* sourceStructReferendM,
    Reference* sourceStructTypeM,
    InterfaceReferend* targetInterfaceReferendM,
    Reference* targetInterfaceTypeM) {
  assert(false);
}

void RCImm::declareKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  referendStructs.declareKnownSizeArray(knownSizeArrayMT);
}

void RCImm::declareUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  referendStructs.declareUnknownSizeArray(unknownSizeArrayMT);
}

void RCImm::translateUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto elementLT =
      translateType(
          unknownSizeArrayMT->rawArray->elementType);
  referendStructs.translateUnknownSizeArray(unknownSizeArrayMT, elementLT);
}

void RCImm::translateKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto elementLT =
      translateType(
          knownSizeArrayMT->rawArray->elementType);
  referendStructs.translateKnownSizeArray(knownSizeArrayMT, elementLT);
}

void RCImm::declareStruct(
    StructDefinition* structM) {
  referendStructs.declareStruct(structM);
}

void RCImm::translateStruct(
    StructDefinition* structM) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        globalState->getRegion(structM->members[i]->type)
            ->translateType(structM->members[i]->type));
  }
  referendStructs.translateStruct(structM, innerStructMemberTypesL);
}

void RCImm::declareEdge(
    Edge* edge) {
  referendStructs.declareEdge(edge);
}

void RCImm::translateEdge(
    Edge* edge) {
  auto interfaceM = globalState->program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    auto interfaceFunctionLT =
        translateInterfaceMethodToFunctionType(
            edge->interfaceName, interfaceM->methods[i]);
    interfaceFunctionsLT.push_back(interfaceFunctionLT);

    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    auto edgeFunctionL = globalState->getFunction(funcName);
    edgeFunctionsL.push_back(edgeFunctionL);
  }
  referendStructs.translateEdge(edge, interfaceFunctionsLT, edgeFunctionsL);
}

void RCImm::declareInterface(
    InterfaceDefinition* interfaceM) {
  referendStructs.declareInterface(interfaceM);
}

void RCImm::translateInterface(
    InterfaceDefinition* interfaceM) {
  assert((uint64_t)interfaceM->referend > 0x10000);
  std::vector<LLVMTypeRef> interfaceMethodTypesL;
  for (int i = 0; i < interfaceM->methods.size(); i++) {
    interfaceMethodTypesL.push_back(
        LLVMPointerType(
            translateInterfaceMethodToFunctionType(interfaceM->referend, interfaceM->methods[i]),
            0));
  }
  referendStructs.translateInterface(
      interfaceM,
      interfaceMethodTypesL);
}

Ref RCImm::weakAlias(
    FunctionState* functionState, LLVMBuilderRef builder, Reference* sourceRefMT, Reference* targetRefMT, Ref sourceRef) {
  assert(false);
}

void RCImm::discardOwningRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  assert(false);
}


void RCImm::noteWeakableDestroyed(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    ControlBlockPtrLE controlBlockPtrLE) {
  // Do nothing
}

Ref RCImm::loadMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    bool structKnownLive,
    int memberIndex,
    Reference* expectedMemberType,
    Reference* targetMemberType,
    const std::string& memberName) {
  auto memberLE =
      loadMember(
          functionState, builder, structRefMT, structRef, memberIndex, expectedMemberType,
          targetMemberType, memberName);
  auto resultRef =
      upgradeLoadResultToRefWithTargetOwnership(
          functionState, builder, expectedMemberType, targetMemberType, memberLE);
  return resultRef;
}

void RCImm::storeMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    bool structKnownLive,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef newValueLE) {
  assert(false);
}

std::tuple<LLVMValueRef, LLVMValueRef> RCImm::explodeInterfaceRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* virtualParamMT,
    Ref virtualArgRef) {
  return explodeStrongInterfaceRef(
      globalState, functionState, builder, &referendStructs, virtualParamMT, virtualArgRef);
}


void RCImm::aliasWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  assert(false);
}

void RCImm::discardWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  assert(false);
}

Ref RCImm::getIsAliveFromWeakRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefM,
    Ref weakRef,
    bool knownLive) {
  assert(false);
}

LLVMValueRef RCImm::getStringBytesPtr(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  return referendStructs.getStringBytesPtr(functionState, builder, ref);
}

Ref RCImm::allocate(
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
                FL(), globalState, functionState, &referendStructs, innerBuilder, desiredReference->referend,
                controlBlockPtrLE, structM->name->name);
          });
  alias(FL(), functionState, builder, desiredReference, resultRef);
  return resultRef;
}

Ref RCImm::upcast(
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructMT,
    StructReferend* sourceStructReferendM,
    Ref sourceRefLE,

    Reference* targetInterfaceTypeM,
    InterfaceReferend* targetInterfaceReferendM) {
  return upcastStrong(globalState, functionState, builder, &referendStructs, sourceStructMT, sourceStructReferendM, sourceRefLE, targetInterfaceTypeM, targetInterfaceReferendM);
}

WrapperPtrLE RCImm::lockWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref weakRefLE,
    bool weakRefKnownLive) {
  assert(false);
}

Ref RCImm::constructKnownSizeArray(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* referenceM,
    KnownSizeArrayT* referendM,
    const std::vector<Ref>& membersLE) {
  auto resultRef =
      ::constructKnownSizeArray(
          globalState, functionState, builder, referenceM, referendM, membersLE, &referendStructs,
          [this, functionState, referenceM, referendM](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
//            fillControlBlock(
//                FL(),
//                functionState,
//                innerBuilder,
//                referenceM->referend,
//                referendM->rawArray->mutability,
//                controlBlockPtrLE,
//                referendM->name->name);
            fillControlBlock(
                FL(), globalState, functionState, &referendStructs, innerBuilder, referendM, controlBlockPtrLE,
                referendM->name->name);
          });
  adjustStrongRc(FL(), globalState, functionState, &referendStructs, builder, resultRef, referenceM, 1);
  return resultRef;
}

Ref RCImm::getUnknownSizeArrayLength(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    Ref arrayRef,
    bool arrayKnownLive) {
  return getUnknownSizeArrayLengthStrong(globalState, functionState, builder, &referendStructs, usaRefMT, arrayRef);
}

LLVMValueRef RCImm::checkValidReference(
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
    checkValidReference(checkerAFL, functionState, builder, &referendStructs, refM, refLE);
  }
  return refLE;
}

Ref RCImm::upgradeLoadResultToRefWithTargetOwnership(
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

  if (sourceLocation == Location::INLINE) {
    return sourceRef;
  } else {
    return sourceRef;
  }
}

void RCImm::checkInlineStructType(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref refLE) {
  auto argLE = checkValidReference(FL(), functionState, builder, refMT, refLE);
  auto structReferend = dynamic_cast<StructReferend*>(refMT->referend);
  assert(structReferend);
  assert(LLVMTypeOf(argLE) == referendStructs.getInnerStruct(structReferend));
}

LoadResult RCImm::loadElementFromKSA(
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

LoadResult RCImm::loadElementFromUSA(
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


Ref RCImm::storeElementInUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef,
    Ref elementRef) {
  assert(false);
}


void RCImm::deallocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref refLE) {
  innerDeallocate(from, globalState, functionState, &referendStructs, builder, refMT, refLE);
}


Ref RCImm::constructUnknownSizeArrayCountedStruct(
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
//            fillControlBlock(
//                FL(),
//                functionState,
//                innerBuilder,
//                unknownSizeArrayT,
//                unknownSizeArrayT->rawArray->mutability,
//                controlBlockPtrLE,
//                typeName);

            fillControlBlock(
                FL(), globalState, functionState, &referendStructs, innerBuilder, unknownSizeArrayT, controlBlockPtrLE,
                typeName);
          });
  adjustStrongRc(FL(), globalState, functionState, &referendStructs, builder, resultRef, usaMT, 1);
  return resultRef;
}


WrapperPtrLE RCImm::mallocStr(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  auto resultRef =
      ::mallocStr(
          globalState, functionState, builder, lengthLE, &referendStructs,
          [this, functionState](LLVMBuilderRef innerBuilder, ControlBlockPtrLE controlBlockPtrLE) {
//            fillControlBlock(
//                FL(), functionState, innerBuilder, globalState->metalCache.str,
//                Mutability::IMMUTABLE, controlBlockPtrLE, "Str");
            fillControlBlock(
                FL(), globalState, functionState, &referendStructs, innerBuilder, globalState->metalCache.str, controlBlockPtrLE,
                "str");
          });
  return resultRef;
}

LLVMValueRef RCImm::getStringLen(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  return referendStructs.getStringLen(functionState, builder, ref);
}

void RCImm::discard(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  auto sourceRnd = sourceMT->referend;

  if (dynamic_cast<Int *>(sourceRnd) ||
      dynamic_cast<Bool *>(sourceRnd) ||
      dynamic_cast<Float *>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend *>(sourceRnd)) {
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      assert(false); // impl
    } else {
      auto rcLE =
          adjustStrongRc(
              from, globalState, functionState, &referendStructs, builder, sourceRef, sourceMT, -1);
      buildIf(
          globalState, functionState,
          builder,
          isZeroLE(builder, rcLE),
          [globalState, functionState, sourceRef, interfaceRnd, sourceMT](
              LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceMT->referend);

            auto interfaceM = globalState->program->getInterface(interfaceRnd->fullName);
            int indexInEdge = -1;
            for (int i = 0; i < interfaceM->methods.size(); i++) {
              if (interfaceM->methods[i]->prototype == immDestructor) {
                indexInEdge = i;
              }
            }
            assert(indexInEdge >= 0);

            std::vector<Ref> argExprsL = {sourceRef};
            buildInterfaceCall(
                globalState, functionState, thenBuilder, immDestructor, argExprsL, 0, indexInEdge);
          });
    }
  } else if (dynamic_cast<StructReferend *>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT *>(sourceRnd)) {
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      auto rcLE =
          adjustStrongRc(
              from, globalState, functionState, &referendStructs, builder, sourceRef, sourceMT, -1);
      buildIf(
          globalState, functionState,
          builder,
          isZeroLE(builder, rcLE),
          [from, globalState, functionState, sourceRef, sourceMT](LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceMT->referend);
            auto funcL = globalState->getFunction(immDestructor->name);

            auto sourceLE =
                globalState->getRegion(sourceMT)->checkValidReference(FL(),
                    functionState, thenBuilder, sourceMT, sourceRef);
            std::vector<LLVMValueRef> argExprsL = {sourceLE};
            return LLVMBuildCall(thenBuilder, funcL, argExprsL.data(), argExprsL.size(), "");
          });
    }
  } else if (dynamic_cast<Str *>(sourceRnd)) {
    assert(sourceMT->ownership == Ownership::SHARE);
    auto rcLE =
        adjustStrongRc(
            from, globalState, functionState, &referendStructs, builder, sourceRef, sourceMT, -1);
    buildIf(
        globalState, functionState,
        builder,
        isZeroLE(builder, rcLE),
        [this, from, globalState, functionState, sourceRef, sourceMT](
            LLVMBuilderRef thenBuilder) {
          buildFlare(from, globalState, functionState, thenBuilder, "Freeing shared str!");
          innerDeallocate(from, globalState, functionState, &referendStructs, thenBuilder, sourceMT, sourceRef);
        });
  } else {
    std::cerr << "Unimplemented type in discard: "
        << typeid(*sourceMT->referend).name() << std::endl;
    assert(false);
  }
}


LLVMTypeRef RCImm::translateType(GlobalState* globalState, Reference* referenceM) {
  if (primitives.isPrimitive(referenceM)) {
    return primitives.translatePrimitive(globalState, referenceM);
  } else {
    if (dynamic_cast<Str *>(referenceM->referend) != nullptr) {
      assert(referenceM->location != Location::INLINE);
      assert(referenceM->ownership == Ownership::SHARE);
      return LLVMPointerType(referendStructs.stringWrapperStructL, 0);
    } else if (auto knownSizeArrayMT = dynamic_cast<KnownSizeArrayT *>(referenceM->referend)) {
      assert(referenceM->location != Location::INLINE);
      auto knownSizeArrayCountedStructLT = referendStructs.getKnownSizeArrayWrapperStruct(knownSizeArrayMT);
      return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
    } else if (auto unknownSizeArrayMT =
        dynamic_cast<UnknownSizeArrayT *>(referenceM->referend)) {
      assert(referenceM->location != Location::INLINE);
      auto unknownSizeArrayCountedStructLT =
          referendStructs.getUnknownSizeArrayWrapperStruct(unknownSizeArrayMT);
      return LLVMPointerType(unknownSizeArrayCountedStructLT, 0);
    } else if (auto structReferend =
        dynamic_cast<StructReferend *>(referenceM->referend)) {
      if (referenceM->location == Location::INLINE) {
        auto innerStructL = referendStructs.getInnerStruct(structReferend);
        return innerStructL;
      } else {
        auto countedStructL = referendStructs.getWrapperStruct(structReferend);
        return LLVMPointerType(countedStructL, 0);
      }
    } else if (auto interfaceReferend =
        dynamic_cast<InterfaceReferend *>(referenceM->referend)) {
      assert(referenceM->location != Location::INLINE);
      auto interfaceRefStructL =
          referendStructs.getInterfaceRefStruct(interfaceReferend);
      return interfaceRefStructL;
    } else if (dynamic_cast<Never*>(referenceM->referend)) {
      auto result = LLVMPointerType(makeNeverType(globalState), 0);
      assert(LLVMTypeOf(globalState->neverPtr) == result);
      return result;
    } else {
      std::cerr << "Unimplemented type: " << typeid(*referenceM->referend).name() << std::endl;
      assert(false);
      return nullptr;
    }
  }
}


LLVMTypeRef RCImm::getControlBlockStruct(Referend* referend) {
  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    auto structM = globalState->program->getStruct(structReferend->fullName);
    assert(structM->mutability == Mutability::IMMUTABLE);
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(referend)) {
    auto interfaceM = globalState->program->getInterface(interfaceReferend->fullName);
    assert(interfaceM->mutability == Mutability::IMMUTABLE);
  } else if (auto ksaMT = dynamic_cast<KnownSizeArrayT*>(referend)) {
    assert(ksaMT->rawArray->mutability == Mutability::IMMUTABLE);
  } else if (auto usaMT = dynamic_cast<UnknownSizeArrayT*>(referend)) {
    assert(usaMT->rawArray->mutability == Mutability::IMMUTABLE);
  } else if (auto strMT = dynamic_cast<Str*>(referend)) {
  } else {
    assert(false);
  }
  return referendStructs.controlBlock.getStruct();
}

ControlBlock* RCImm::getControlBlock(Referend* referend) {
  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    auto structM = globalState->program->getStruct(structReferend->fullName);
    assert(structM->mutability == Mutability::IMMUTABLE);
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(referend)) {
    auto interfaceM = globalState->program->getInterface(interfaceReferend->fullName);
    assert(interfaceM->mutability == Mutability::IMMUTABLE);
  } else if (auto ksaMT = dynamic_cast<KnownSizeArrayT*>(referend)) {
    assert(ksaMT->rawArray->mutability == Mutability::IMMUTABLE);
  } else if (auto usaMT = dynamic_cast<UnknownSizeArrayT*>(referend)) {
    assert(usaMT->rawArray->mutability == Mutability::IMMUTABLE);
  } else if (auto strMT = dynamic_cast<Str*>(referend)) {
  } else {
    assert(false);
  }
  return &referendStructs.controlBlock;
}


LoadResult RCImm::loadMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    int memberIndex,
    Reference* expectedMemberType,
    Reference* targetType,
    const std::string& memberName) {
  if (structRefMT->location == Location::INLINE) {
    auto innerStructLE =
        globalState->getRegion(structRefMT)->checkValidReference(
            FL(), functionState, builder, structRefMT, structRef);
    auto memberLE =
        LLVMBuildExtractValue(builder, innerStructLE, memberIndex, memberName.c_str());
    return LoadResult{wrap(globalState->getRegion(expectedMemberType), expectedMemberType, memberLE)};
  } else {
    return regularLoadStrongMember(globalState, functionState, builder, &referendStructs, structRefMT, structRef, memberIndex, expectedMemberType, targetType, memberName);
  }
}

void RCImm::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    IReferendStructsSource* referendStructs,
    Reference* refM,
    LLVMValueRef refLE) {
  regularCheckValidReference(checkerAFL, globalState, functionState, builder, referendStructs, refM, refLE);
}

std::string RCImm::getRefNameC(Reference* sourceMT) {
  auto sourceRnd = sourceMT->referend;
  if (dynamic_cast<Int *>(sourceRnd)) {
    return "int64_t";
  } else if (dynamic_cast<Bool *>(sourceRnd)) {
    return "int8_t";
  } else if (dynamic_cast<Float *>(sourceRnd)) {
    return "double";
  } else if (dynamic_cast<Str *>(sourceRnd)) {
    return "ValeStr*";
  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend *>(sourceRnd)) {
    auto baseName = globalState->program->getExportedName(interfaceRnd->fullName);
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      return baseName + "Inl";
    } else {
      return baseName + "Ref";
    }
  } else if (sourceRnd == globalState->metalCache.emptyTupleStruct) {
    return "void";
  } else if (auto structRnd = dynamic_cast<StructReferend *>(sourceRnd)) {
    auto baseName = globalState->program->getExportedName(structRnd->fullName);
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      return baseName + "Inl";
    } else {
      return baseName + "Ref";
    }
  } else if (dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
             dynamic_cast<UnknownSizeArrayT *>(sourceRnd)) {
    assert(false); // impl
  } else {
    std::cerr << "Unimplemented type in immutables' getRefNameC: "
              << typeid(*sourceMT->referend).name() << std::endl;
    assert(false);
  }
}

void RCImm::generateStructDefsC(std::unordered_map<std::string, std::string>* cByExportedName, StructDefinition* structDefM) {
  auto name = globalState->program->getExportedName(structDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused; } " << name << ";" << std::endl;

  // For inlines
  s << "typedef struct " << name << "Inl {";
  for (int i = 0; i < structDefM->members.size(); i++) {
    auto member = structDefM->members[i];
    s << getRefNameC(member->type) << " unused" << i << ";";
  }
  s << " } " + name + ";" << std::endl;

  cByExportedName->insert(std::make_pair(name, s.str()));
}

void RCImm::generateInterfaceDefsC(std::unordered_map<std::string, std::string>* cByExportedName, InterfaceDefinition* interfaceDefM) {
  auto name = globalState->program->getExportedName(interfaceDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused1; void* unused2; } " << name << ";";
  cByExportedName->insert(std::make_pair(name, s.str()));
}

LLVMTypeRef RCImm::getExternalType(
    Reference* refMT) {
  // these arent exposed to the outside world, we instead copy to the Linear region
  assert(false);
  return nullptr;
}


Ref RCImm::receiveFrom(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(sourceRefMT->ownership == Ownership::SHARE);

  auto sourceRegion = globalState->getExternRegion(sourceRefMT);
  // Someday when we include the region in the coord, we wont have to assume its linear.
  // When that happens, change the LLVMBuildTrunc below too.
  assert(sourceRegion == globalState->linearRegion);
  auto sourceRefLE = sourceRegion->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceRef);
  // Someday when we include the region in the coord, this line will change.
  auto targetRefMT = sourceRefMT;

  if (sourceRefMT == globalState->metalCache.intRef) {
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, sourceRefLE);
  } else if (sourceRefMT == globalState->metalCache.boolRef) {
    auto asI1LE =
        LLVMBuildTrunc(
            builder, sourceRefLE, LLVMInt1TypeInContext(globalState->context), "boolAsI1");
    return wrap(this, targetRefMT, asI1LE);
  } else if (sourceRefMT == globalState->metalCache.floatRef) {
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, sourceRefLE);
  } else if (sourceRefMT == globalState->metalCache.strRef) {
//    auto structL = referendStructs.getStringWrapperStruct();

//    assert(LLVMTypeOf(sourceRefLE) == LLVMPointerType(structL, 0));
//    auto extStrPtrLE = sourceRefLE;

    auto strLenLE = sourceRegion->getStringLen(functionState, builder, sourceRef);
    auto strLenBytesPtrLE = sourceRegion->getStringBytesPtr(functionState, builder, sourceRef);

    auto vstrPtrLE = LLVMBuildCall(builder, globalState->newVStr, &strLenLE, 1, "vstrPtr");
    auto vstrCharsPtrLE = LLVMBuildCall(builder, globalState->getStrCharsFunc, &vstrPtrLE, 1, "vstrCharsPtr");

    std::vector<LLVMValueRef> strncpyArgs = { vstrCharsPtrLE, strLenBytesPtrLE, strLenLE };
    LLVMBuildCall(builder, globalState->strncpy, strncpyArgs.data(), strncpyArgs.size(), "");

    sourceRegion->dealias(FL(), functionState, builder, sourceRefMT, sourceRef);

//    // Free the thing C gave us.
//    auto extStrI8PtrLE =
//        LLVMBuildPointerCast(
//            builder,
//            extStrPtrLE,
//            LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
//            "extStrPtrLE");
//    LLVMBuildCall(builder, globalState->free, &extStrI8PtrLE, 1, "");

    return wrap(globalState->getRegion(globalState->metalCache.strRef), globalState->metalCache.strRef, vstrPtrLE);
  } else if (auto usa = dynamic_cast<UnknownSizeArrayT*>(sourceRefMT->referend)) {
    assert(false);
//    start here, perhaps make an external struct for all USAs.
//        itll be like the string one except with the number of elements rather than the total
//        number of bytes. or maybe it can have both?
//    auto externalStructLIter = externalStructLByReferend.find(globalState->metalCache.str);
//    assert(externalStructLIter != externalStructLByReferend.end());
//    auto externalStructL = externalStructLIter->second;
//
//    assert(LLVMTypeOf(refLE) == LLVMPointerType(externalStructL, 0));
//    auto extStrPtrLE = refLE;
//
//    auto extStrLenPtrLE = LLVMBuildStructGEP(builder, extStrPtrLE, 0, "extStrLenPtr");
//    auto extStrLenLE = LLVMBuildLoad(builder, extStrLenPtrLE, "extStrLen");
//
//    auto extStrCharsPtrPtrLE = LLVMBuildStructGEP(builder, extStrPtrLE, 1, "extStrCharsPtr");
//    auto extStrCharsPtrLE = LLVMBuildLoad(builder, extStrCharsPtrPtrLE, "extStrChars");
//
//    auto vstrPtrLE = LLVMBuildCall(builder, globalState->newVStr, &extStrLenLE, 1, "vstrPtr");
//    auto vstrCharsPtrLE = LLVMBuildCall(builder, globalState->getStrCharsFunc, &vstrPtrLE, 1, "vstrCharsPtr");
//
//    std::vector<LLVMValueRef> strncpyArgs = { vstrCharsPtrLE, extStrCharsPtrLE, extStrLenLE };
//    LLVMBuildCall(builder, globalState->strncpy, strncpyArgs.data(), strncpyArgs.size(), "");
//
//    // Free the thing C gave us.
//    auto extStrI8PtrLE =
//        LLVMBuildPointerCast(
//            builder,
//            extStrPtrLE,
//            LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
//            "extStrPtrLE");
//    LLVMBuildCall(builder, globalState->free, &extStrI8PtrLE, 1, "");
//
//    return wrap(globalState->getRegion(globalState->metalCache.strRef), globalState->metalCache.strRef, vstrPtrLE);
  } else if (sourceRefMT == globalState->metalCache.neverRef) {
    assert(false); // How can we hand a never into something?
  } else if (sourceRefMT == globalState->metalCache.emptyTupleStructRef) {
    return wrap(globalState->getRegion(sourceRefMT), sourceRefMT, makeEmptyTuple(globalState, functionState, builder));
  } else if (auto structReferend = dynamic_cast<StructReferend*>(sourceRefMT->referend)) {
    assert(false); // impl

//    if (sourceRefMT->location == Location::INLINE) {
//      globalState->getRegion(refHere)->checkInlineStructType(functionState, builder, sourceRefMT, ref);
//    } else {
////            std::cerr << "Can only pass inline imm structs between C and Vale currently." << std::endl;
//      assert(false); // impl
//      return nullptr;
//    }
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(sourceRefMT->referend)) {

    assert(false); // impl
  } else {
    std::cerr << "Invalid type for extern!" << std::endl;
    assert(false);
  }

  assert(false);
}


LLVMTypeRef RCImm::translateInterfaceMethodToFunctionType(
    InterfaceReferend* referend,
    InterfaceMethod* method) {
  auto returnMT = method->prototype->returnType;
  auto paramsMT = method->prototype->params;
  auto returnLT = translateType(returnMT);
  auto paramsLT = translateTypes(globalState, paramsMT);

  paramsLT[method->virtualParamIndex] = LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);

  return LLVMFunctionType(returnLT, paramsLT.data(), paramsLT.size(), false);
}
