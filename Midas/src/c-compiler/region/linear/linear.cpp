#include <function/expressions/shared/shared.h>
#include <utils/counters.h>
#include <utils/branch.h>
#include <region/common/controlblock.h>
#include <region/common/heap.h>
#include <function/expressions/shared/string.h>
#include <region/common/common.h>
#include <sstream>
#include <function/expressions/shared/elements.h>
#include <midasfunctions.h>
#include "linear.h"
#include "translatetype.h"



Ref unsafeCast(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Reference* desiredRefMT,
    Ref sourceRef) {
  auto sourcePtrLE = globalState->getRegion(sourceRefMT)->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceRef);
  auto desiredPtrLT = globalState->getRegion(sourceRefMT)->translateType(desiredRefMT);
  auto desiredPtrLE = LLVMBuildPointerCast(builder, sourcePtrLE, desiredPtrLT, "destStructPtr");
  auto desiredRef = wrap(globalState->getRegion(desiredRefMT), desiredRefMT, desiredPtrLE);
  return desiredRef;
}

LLVMValueRef hexRoundDown(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef n) {
  // Mask off the last four bits, to round downward to the next multiple of 16.
  auto mask = LLVMConstInt(LLVMInt64TypeInContext(globalState->context), ~0xFUL, false);
  return LLVMBuildAnd(builder, n, mask, "rounded");
}

LLVMValueRef lowerAndHexRoundDownPointer(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef rawPtrLE,
    LLVMValueRef subtractIntLE) {
  auto rawPtrIntLE =
      LLVMBuildPointerCast(
          builder, rawPtrLE, LLVMInt64TypeInContext(globalState->context), "rawPtrInt");
  auto loweredRawPointerIntLE = LLVMBuildSub(builder, rawPtrIntLE, subtractIntLE, "loweredRawPtrInt");
  auto roundedLoweredRawPointerIntLE = hexRoundDown(globalState, builder, loweredRawPointerIntLE);
  return LLVMBuildPointerCast(builder, roundedLoweredRawPointerIntLE, LLVMTypeOf(rawPtrLE), "loweredRoundedRawPtr");
}

Linear::Linear(GlobalState* globalState_)
  : globalState(globalState_),
    structs(globalState_) {

  regionReferend =
      globalState->metalCache->getStructReferend(
          globalState->metalCache->getName("__Linear_Region"));
  regionRefMT =
      globalState->metalCache->getReference(
          Ownership::BORROW, Location::YONDER, regionReferend);

  linearStr = globalState->metalCache->getStr(globalState->metalCache->linearRegionId);
  linearStrRefMT =
      globalState->metalCache->getReference(
          Ownership::SHARE, Location::YONDER, linearStr);

  regionLT = LLVMStructCreateNamed(globalState->context, "__Linear_Region");
  std::vector<LLVMTypeRef> membersLT = {
      LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
      LLVMInt64TypeInContext(globalState->context),
  };
  LLVMStructSetBody(regionLT, membersLT.data(), membersLT.size(), false);
}

RegionId* Linear::getRegionId() {
  return globalState->metalCache->linearRegionId;
}

void Linear::alias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    Ref ref) {
  assert(false); // impl
}

void Linear::dealias(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  assert(false); // impl
}

Ref Linear::lockWeak(
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

LLVMTypeRef Linear::translateType(Reference* referenceM) {
  if (dynamic_cast<Int*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMInt64TypeInContext(globalState->context);
  } else if (dynamic_cast<Bool*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMInt1TypeInContext(globalState->context);
  } else if (dynamic_cast<Float*>(referenceM->referend) != nullptr) {
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMDoubleTypeInContext(globalState->context);
  } else if (dynamic_cast<Never*>(referenceM->referend) != nullptr) {
    return LLVMArrayType(LLVMIntTypeInContext(globalState->context, NEVER_INT_BITS), 0);
  } else if (dynamic_cast<Str *>(referenceM->referend) != nullptr) {
    assert(referenceM->location != Location::INLINE);
    assert(referenceM->ownership == Ownership::SHARE);
    return LLVMPointerType(structs.getStringStruct(), 0);
  } else if (auto knownSizeArrayMT = dynamic_cast<KnownSizeArrayT *>(referenceM->referend)) {
    assert(referenceM->location != Location::INLINE);
    auto knownSizeArrayCountedStructLT = structs.getKnownSizeArrayStruct(knownSizeArrayMT);
    return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
  } else if (auto unknownSizeArrayMT =
      dynamic_cast<UnknownSizeArrayT *>(referenceM->referend)) {
    assert(referenceM->location != Location::INLINE);
    auto unknownSizeArrayCountedStructLT =
        structs.getUnknownSizeArrayStruct(unknownSizeArrayMT);
    return LLVMPointerType(unknownSizeArrayCountedStructLT, 0);
  } else if (auto structReferend =
      dynamic_cast<StructReferend *>(referenceM->referend)) {
    if (structReferend == regionReferend) {
      return LLVMPointerType(regionLT, 0);
    } else {
      if (referenceM->location == Location::INLINE) {
        auto innerStructL = structs.getStructStruct(structReferend);
        return innerStructL;
      } else {
        auto countedStructL = structs.getStructStruct(structReferend);
        return LLVMPointerType(countedStructL, 0);
      }
    }
  } else if (auto interfaceReferend =
      dynamic_cast<InterfaceReferend *>(referenceM->referend)) {
    assert(referenceM->location != Location::INLINE);
    auto interfaceRefStructL =
        structs.getInterfaceRefStruct(interfaceReferend);
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

LLVMValueRef Linear::getCensusObjectId(
    AreaAndFileAndLine checkerAFL,
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Reference *refM,
    Ref ref) {
  return constI64LE(globalState, 0);
}

Ref Linear::upcastWeak(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    WeakFatPtrLE sourceRefLE,
    StructReferend* sourceStructReferendM,
    Reference* sourceStructTypeM,
    InterfaceReferend* targetInterfaceReferendM,
    Reference* targetInterfaceTypeM) {
  assert(false);
}

void Linear::declareKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto hostName = globalState->metalCache->getName(namePrefix + "_" + knownSizeArrayMT->name->name);
  auto hostReferend = globalState->metalCache->getStructReferend(hostName);
  hostReferendByValeReferend.emplace(knownSizeArrayMT, hostReferend);

  structs.declareKnownSizeArray(knownSizeArrayMT);
}

void Linear::declareUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto hostName = globalState->metalCache->getName(namePrefix + "_" + unknownSizeArrayMT->name->name);
  auto hostReferend = globalState->metalCache->getStructReferend(hostName);
  hostReferendByValeReferend.emplace(unknownSizeArrayMT, hostReferend);

  structs.declareUnknownSizeArray(unknownSizeArrayMT);
}

void Linear::translateUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto elementLT =
      translateType(
          linearizeReference(
              unknownSizeArrayMT->rawArray->elementType));
  structs.translateUnknownSizeArray(unknownSizeArrayMT, elementLT);
}

void Linear::translateKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto elementLT =
      translateType(
          linearizeReference(
              knownSizeArrayMT->rawArray->elementType));

  structs.translateKnownSizeArray(knownSizeArrayMT, elementLT);
}

void Linear::declareStruct(
    StructDefinition* structM) {
  auto hostName = globalState->metalCache->getName(namePrefix + "_" + structM->name->name);
  auto hostReferend = globalState->metalCache->getStructReferend(hostName);
  hostReferendByValeReferend.emplace(structM->referend, hostReferend);

  structs.declareStruct(structM);
}

void Linear::translateStruct(
    StructDefinition* structM) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        translateType(linearizeReference(structM->members[i]->type)));
  }
  structs.translateStruct(structM, innerStructMemberTypesL);
}

void Linear::declareEdge(
    Edge* edge) {
  assert(false);
  structs.declareEdge(edge);
}

void Linear::translateEdge(
    Edge* edge) {
  assert(false);
  auto interfaceM = globalState->program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  std::tie(interfaceFunctionsLT, edgeFunctionsL) =
      globalState->getEdgeFunctionTypesAndFunctions(edge);

  structs.translateEdge(edge, interfaceFunctionsLT, edgeFunctionsL);
}

void Linear::declareInterface(
    InterfaceDefinition* interfaceM) {
  assert(false);
  structs.declareInterface(interfaceM);
}

void Linear::translateInterface(
    InterfaceDefinition* interfaceM) {
  assert((uint64_t)interfaceM->referend > 0x10000);
  std::vector<LLVMTypeRef> interfaceMethodTypesL;
  for (int i = 0; i < interfaceM->methods.size(); i++) {
    interfaceMethodTypesL.push_back(
        LLVMPointerType(
            translateInterfaceMethodToFunctionType(globalState, interfaceM->methods[i]),
            0));
  }
  structs.translateInterface(interfaceM);
}

Ref Linear::weakAlias(
    FunctionState* functionState, LLVMBuilderRef builder, Reference* sourceRefMT, Reference* targetRefMT, Ref sourceRef) {
  assert(false);
}

void Linear::discardOwningRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  assert(false);
}


void Linear::noteWeakableDestroyed(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    ControlBlockPtrLE controlBlockPtrLE) {
  // Do nothing
}

Ref Linear::loadMember(
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

void Linear::storeMember(
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
  assert(false);
}

std::tuple<LLVMValueRef, LLVMValueRef> Linear::explodeInterfaceRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* virtualParamMT,
    Ref virtualArgRef) {
//  return explodeStrongInterfaceRef(
//      globalState, functionState, builder, &referendStructs, virtualParamMT, virtualArgRef);
  assert(false); // do we need this method in linear?
}


void Linear::aliasWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  assert(false);
}

void Linear::discardWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefMT,
    Ref weakRef) {
  assert(false);
}

Ref Linear::getIsAliveFromWeakRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* weakRefM,
    Ref weakRef,
    bool knownLive) {
  assert(false);
}

LLVMValueRef Linear::getStringBytesPtr(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  auto strWrapperPtrLE =
      checkValidReference(FL(), functionState, builder, linearStrRefMT, ref);
  return structs.getStringBytesPtr(functionState, builder, strWrapperPtrLE);
}

Ref Linear::allocate(
    Ref regionInstanceRef,
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    const std::vector<Ref>& memberRefs) {
  return innerAllocate(regionInstanceRef, from, functionState, builder, structRefMT, memberRefs, false);
}

Ref Linear::innerAllocate(
    Ref regionInstanceRef,
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    const std::vector<Ref>& memberHostRefs,
    bool dryRun) {

  auto intMT = globalState->metalCache->intRef;

  auto desiredStructMT = dynamic_cast<StructReferend*>(structRefMT->referend);
  assert(desiredStructMT);

  auto structDefM = globalState->program->getStruct(desiredStructMT->fullName);

  auto objectRef = getDestinationRef(functionState, builder, regionInstanceRef, structRefMT);
  auto objectPtrLE = checkValidReference(FL(), functionState, builder, structRefMT, objectRef);

  if (!dryRun) {
    fillInnerStruct(globalState, functionState, builder, structDefM, memberHostRefs, objectPtrLE);
  }

  LLVMValueRef substructSizeIntLE =
      predictShallowSize(builder, desiredStructMT, constI64LE(globalState, 0));
  bumpDestinationOffset(functionState, builder, regionInstanceRef, substructSizeIntLE);

  return objectRef;
}

Ref Linear::upcast(
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructMT,
    StructReferend* sourceStructReferendM,
    Ref sourceRefLE,

    Reference* targetInterfaceTypeM,
    InterfaceReferend* targetInterfaceReferendM) {
  assert(false);
//  return upcastStrong(globalState, functionState, builder, &referendStructs, sourceStructMT, sourceStructReferendM, sourceRefLE, targetInterfaceTypeM, targetInterfaceReferendM);
}

WrapperPtrLE Linear::lockWeakRef(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    Ref weakRefLE,
    bool weakRefKnownLive) {
  assert(false);
}

Ref Linear::constructKnownSizeArray(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* referenceM,
    KnownSizeArrayT* referendM,
    const std::vector<Ref>& memberRefs) {
  // The serialize functions for each region are what populates the linear region.
  assert(false);
}

Ref Linear::getUnknownSizeArrayLength(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    Ref arrayRef,
    bool arrayKnownLive) {
  assert(false);
//  return getUnknownSizeArrayLengthStrong(globalState, functionState, builder, &referendStructs, usaRefMT, arrayRef);
}

LLVMValueRef Linear::checkValidReference(
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
  assert(LLVMTypeOf(refLE) == this->translateType(refM));
  return refLE;
}

Ref Linear::upgradeLoadResultToRefWithTargetOwnership(
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

void Linear::checkInlineStructType(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  auto argLE = checkValidReference(FL(), functionState, builder, refMT, ref);
  auto structReferend = dynamic_cast<StructReferend*>(refMT->referend);
  assert(structReferend);
  assert(LLVMTypeOf(argLE) == structs.getStructStruct(structReferend));
}

LoadResult Linear::loadElementFromKSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* ksaRefMT,
    KnownSizeArrayT* ksaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef) {
  auto arrayRefLE = checkValidReference(FL(), functionState, builder, ksaRefMT, arrayRef);
  // Array is the only member in the KSA struct.
  auto elementsPtrLE = LLVMBuildStructGEP(builder, arrayRefLE, 0, "ksaElemsPtr");
  return loadElementFromKSAInner(globalState, functionState, builder, ksaRefMT, ksaMT, indexRef, elementsPtrLE);
}

LoadResult Linear::loadElementFromUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayKnownLive,
    Ref indexRef) {
  auto arrayRefLE = checkValidReference(FL(), functionState, builder, usaRefMT, arrayRef);
  // Size is the first member in the USA struct.
  auto sizeLE = LLVMBuildLoad(builder, LLVMBuildStructGEP(builder, arrayRefLE, 0, "usaSizePtr"), "usaSize");
  auto sizeRef = wrap(this, globalState->metalCache->intRef, sizeLE);
  // Elements is the 1th member in the USA struct, after size.
  auto elementsPtrLE = LLVMBuildStructGEP(builder, arrayRefLE, 1, "usaElemsPtr");

  return loadElementWithoutUpgrade(
      globalState, functionState, builder, usaRefMT,
      usaMT->rawArray->elementType,
      sizeRef, elementsPtrLE, usaMT->rawArray->mutability, indexRef);
}


Ref Linear::storeElementInUSA(
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

void Linear::deallocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  auto refLE = checkValidReference(FL(), functionState, builder, refMT, ref);
  auto concreteAsCharPtrLE =
      LLVMBuildBitCast(
          builder,
          refLE,
          LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
          "concreteCharPtrForFree");
  LLVMBuildCall(builder, globalState->free, &concreteAsCharPtrLE, 1, "");
}

Ref Linear::constructUnknownSizeArrayCountedStruct(
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
  assert(false);
}

Ref Linear::mallocStr(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  return innerMallocStr(regionInstanceRef, functionState, builder, lengthLE, false);
}

Ref Linear::innerMallocStr(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE,
    bool dryRun) {
  auto lenI64LE = LLVMBuildZExt(builder, lengthLE, LLVMInt64TypeInContext(globalState->context), "");

  auto strRef = getDestinationRef(functionState, builder, regionInstanceRef, linearStrRefMT);
  auto strPtrLE = checkValidReference(FL(), functionState, builder, linearStrRefMT, strRef);

  if (!dryRun) {
    auto lenPtrLE = LLVMBuildStructGEP(builder, strPtrLE, 0, "lenPtr");
    LLVMBuildStore(builder, lenI64LE, lenPtrLE);

    // Set the null terminating character to the 0th spot and the end spot, just to guard against bugs
    auto charsBeginPtr = getStringBytesPtr(functionState, builder, strRef);
    LLVMBuildStore(builder, constI8LE(globalState, 0), charsBeginPtr);
    auto charsEndPtr = LLVMBuildGEP(builder, charsBeginPtr, &lengthLE, 1, "charsEndPtr");
    LLVMBuildStore(builder, constI8LE(globalState, 0), charsEndPtr);

    // The caller still needs to initialize the actual chars inside!
  }

  auto sizeLE = predictShallowSize(builder, linearStr, lengthLE);
  bumpDestinationOffset(functionState, builder, regionInstanceRef, sizeLE);

  return strRef;
}

LLVMValueRef Linear::getStringLen(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  auto refPtrLE = checkValidReference(FL(), functionState, builder, linearStrRefMT, ref);
  return LLVMBuildLoad(builder, LLVMBuildStructGEP(builder, refPtrLE, 0, "lenPtr"), "len");
}

LoadResult Linear::loadMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    int memberIndex,
    Reference* expectedMemberType,
    Reference* targetType,
    const std::string& memberName) {
  auto structRefLE = checkValidReference(FL(), functionState, builder, structRefMT, structRef);
  if (structRefMT->location == Location::INLINE) {
    auto memberLE = LLVMBuildExtractValue(builder, structRefLE, memberIndex, memberName.c_str());
    return LoadResult{wrap(globalState->getRegion(expectedMemberType), expectedMemberType, memberLE)};
  } else {
    auto structPtrLE = structRefLE;
    return loadInnerInnerStructMember(globalState, builder, structPtrLE, memberIndex, expectedMemberType, memberName);
  }
}

void Linear::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    IReferendStructsSource* referendStructs,
    Reference* refM,
    LLVMValueRef refLE) {
  regularCheckValidReference(checkerAFL, globalState, functionState, builder, referendStructs, refM, refLE);
}

std::string Linear::getRefNameC(Reference* sourceMT) {
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
  } else if (sourceRnd == globalState->metalCache->emptyTupleStruct) {
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

void Linear::generateStructDefsC(std::unordered_map<std::string, std::string>* cByExportedName, StructDefinition* structDefM) {
  auto name = globalState->program->getExportedName(structDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused; } " << name << "Ref;" << std::endl;

  // For inlines
  s << "typedef struct " << name << "Inl {";
  for (int i = 0; i < structDefM->members.size(); i++) {
    auto member = structDefM->members[i];
    s << getRefNameC(member->type) << " unused" << i << ";";
  }
  s << " } " + name + "Inl;" << std::endl;

  cByExportedName->insert(std::make_pair(name, s.str()));
}

void Linear::generateInterfaceDefsC(std::unordered_map<std::string, std::string>* cByExportedName, InterfaceDefinition* interfaceDefM) {
  auto name = globalState->program->getExportedName(interfaceDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused1; void* unused2; } " << name << "Ref;";
  cByExportedName->insert(std::make_pair(name, s.str()));
}

LLVMTypeRef Linear::getExternalType(
    Reference* refMT) {
  assert(false);
//  assert(refMT->ownership == Ownership::SHARE);
//
//  if (refMT == globalState->metalCache->intRef) {
//    return LLVMInt64TypeInContext(globalState->context);
//  } else if (refMT == globalState->metalCache->boolRef) {
//    return LLVMInt8TypeInContext(globalState->context);
//  } else if (refMT == globalState->metalCache->floatRef) {
//    return LLVMDoubleTypeInContext(globalState->context);
//  } else if (refMT == globalState->metalCache->strRef) {
//    auto structLIter = externalStructLByReferend.find(globalState->metalCache->str);
//    assert(structLIter != externalStructLByReferend.end());
//    auto structL = structLIter->second;
//    return LLVMPointerType(structL, 0);
//  } else if (refMT == globalState->metalCache->neverRef) {
//    assert(false); // How can we hand a never into something?
//    return nullptr;
//  } else if (auto usa = dynamic_cast<UnknownSizeArrayT*>(refMT->referend)) {
//    auto structLIter = externalStructLByReferend.find(globalState->metalCache->str);
//    assert(structLIter != externalStructLByReferend.end());
//    auto structL = structLIter->second;
//    return LLVMPointerType(structL, 0);
//  } else if (auto structReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
//    assert(false); // impl
//    return nullptr;
//  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(refMT->referend)) {
//    assert(false); // impl
//    return nullptr;
//  } else {
//    std::cerr << "Invalid type for extern!" << std::endl;
//    assert(false);
//    return nullptr;
//  }

  assert(false);
  return nullptr;
}

Ref Linear::topLevelSerialize(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* valeRefMT,
    Reference* hostRefMT,
    Ref ref) {
  buildFlare(FL(), globalState, functionState, builder, "topLevelSerialize");

  auto nullLT = LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0));
  auto dryRunCounterBeginLE = constI64LE(globalState, 0x4000000000000000UL);

  auto dryRunInitialRegionStructLE = LLVMGetUndef(regionLT);
  dryRunInitialRegionStructLE = LLVMBuildInsertValue(builder, dryRunInitialRegionStructLE, nullLT, 0, "regionStruct");
  dryRunInitialRegionStructLE = LLVMBuildInsertValue(builder, dryRunInitialRegionStructLE, dryRunCounterBeginLE, 1, "regionStruct");
  auto dryRunRegionInstancePtrLE = makeMidasLocal(functionState, builder, regionLT, "region", dryRunInitialRegionStructLE);
  auto dryRunRegionInstanceRef = wrap(this, regionRefMT, dryRunRegionInstancePtrLE);

  callSerialize(functionState, builder, valeRefMT, hostRefMT, dryRunRegionInstanceRef, ref, globalState->constI1(true));

  auto dryRunFinalOffsetLE = getDestinationOffset(builder, dryRunRegionInstancePtrLE);
  auto sizeIntLE = LLVMBuildSub(builder, dryRunCounterBeginLE, dryRunFinalOffsetLE, "size");

  LLVMValueRef bufferBeginPtrLE = callMalloc(globalState, builder, sizeIntLE);

  auto initialRegionStructLE = LLVMGetUndef(regionLT);
  initialRegionStructLE = LLVMBuildInsertValue(builder, initialRegionStructLE, bufferBeginPtrLE, 0, "regionStruct");
  initialRegionStructLE = LLVMBuildInsertValue(builder, initialRegionStructLE, sizeIntLE, 1, "regionStruct");
  auto regionInstancePtrLE = makeMidasLocal(functionState, builder, regionLT, "region", initialRegionStructLE);
  auto regionInstanceRef = wrap(this, regionRefMT, regionInstancePtrLE);

  auto resultRef = callSerialize(functionState, builder, valeRefMT, hostRefMT, regionInstanceRef, ref, globalState->constI1(false));

  auto destinationIntLE = getDestinationOffset(builder, regionInstancePtrLE);
  auto condLE = LLVMBuildICmp(builder, LLVMIntEQ, destinationIntLE, constI64LE(globalState, 0), "cond");
  buildAssert(globalState, functionState, builder, condLE, "Serialization start mismatch!");

  return resultRef;
}

Ref Linear::receiveUnencryptedAlienReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Reference* targetRefMT,
    Ref sourceRef) {
  assert(sourceRefMT->ownership == Ownership::SHARE);

  auto sourceRegion = globalState->getRegion(sourceRefMT);

  auto sourceRefLE =
      globalState->getRegion(sourceRefMT)
          ->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceRef);

  if (dynamic_cast<Int*>(sourceRefMT->referend)) {
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, sourceRefLE);
  } else if (dynamic_cast<Bool*>(sourceRefMT->referend)) {
    auto resultLE = LLVMBuildZExt(builder, sourceRefLE, LLVMInt8TypeInContext(globalState->context), "boolAsI8");
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, resultLE);
  } else if (dynamic_cast<Float*>(sourceRefMT->referend)) {
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, sourceRefLE);
  } else if (dynamic_cast<Str*>(sourceRefMT->referend) ||
      dynamic_cast<StructReferend*>(sourceRefMT->referend) ||
      dynamic_cast<InterfaceReferend*>(sourceRefMT->referend) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRefMT->referend) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRefMT->referend)) {
    if (sourceRefMT->location == Location::INLINE) {
      assert(false); // impl
      if (sourceRefMT == globalState->metalCache->emptyTupleStructRef) {
        return makeEmptyTupleRef(globalState, this, builder);
      } else {
        assert(false);
      }
    } else {
      return topLevelSerialize(functionState, builder, sourceRefMT, targetRefMT, sourceRef);
    }
  } else assert(false);

  assert(false);
}

LLVMTypeRef Linear::getInterfaceMethodVirtualParamAnyType(Reference* reference) {
  return LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);
}

LLVMValueRef Linear::predictShallowSize(LLVMBuilderRef builder, Referend* referend, LLVMValueRef lenIntLE) {
  if (referend == linearStr) {
    auto headerBytesLE =
        constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, structs.getStringStruct()));
    return LLVMBuildAdd(builder, headerBytesLE, lenIntLE, "sum");
  } else if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    return constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, structs.getStructStruct(structReferend)));
  } else assert(false);
}

Ref Linear::receiveAndDecryptFamiliarReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(false);
}

Ref Linear::encryptAndSendFamiliarReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(false);
}



//// This should NOT be called on anything inline, because it adds padding at the end.
//Ref Linear::serializeInto(
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* refMT,
//    Ref sourceRef,
//    Ref destinationRawPtrRef) {
//  assert(refMT->location != Location::INLINE); // impl
//
//  auto strMT = globalState->metalCache->strRef;
//  auto intMT = globalState->metalCache->intRef;
//  if (refMT == globalState->metalCache->intRef) {
//    assert(false);
//  } else if (refMT == globalState->metalCache->boolRef) {
//    assert(false);
//  } else if (refMT == globalState->metalCache->floatRef) {
//    assert(false);
//  } else if (refMT == globalState->metalCache->strRef) {
//    auto prototype =
//        globalState->metalCache->getPrototype(
//            globalState->serializeName, rawPointerMT, { globalState->metalCache->regionRef, refMT });
//    return buildCall(
//        globalState, functionState, builder, prototype,
//        { sourceRef, destinationRawPtrRef });
//  } else if (auto structReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
//    if (refMT->location == Location::INLINE) {
//      if (refMT == globalState->metalCache->emptyTupleStructRef) {
//        // Return immediately, dont add padding.
//        return globalState->constI64(0);
//      } else {
//        assert(false); // impl
//      }
//    } else {
//      return buildCall(
//          globalState, functionState, builder, getSerializeProtoype(refMT),
//          { sourceRef, destinationRawPtrRef });
//    }
//  } else if (auto interfaceReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
//    auto prototype =
//        globalState->metalCache->getPrototype(
//            globalState->serializeName, rawPointerMT, { refMT, rawPointerMT });
//    destinationRawPtrRef =
//        buildInterfaceCall(
//            globalState, functionState, builder, prototype,
//            { sourceRef, destinationRawPtrRef },
//            0);
//    destinationRawPtrRef = addTrailingPadding(globalState, functionState, builder, destinationRawPtrRef);
//    return destinationRawPtrRef;
//  } else assert(false);
//}


Prototype* Linear::getSerializeProtoype(
    Reference* valeRefMT,
    Reference* hostRefMT) {
  return globalState->metalCache->getPrototype(
      globalState->serializeName, hostRefMT,
      {regionRefMT, valeRefMT, globalState->metalCache->boolRef});
}

Ref Linear::callSerialize(
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Reference* valeRefMT,
    Reference* hostRefMT,
    Ref regionInstanceRef,
    Ref objectRef,
    Ref dryRunBoolRef) {
  auto prototype = getSerializeProtoype(valeRefMT, hostRefMT);
  if (dynamic_cast<InterfaceReferend*>(valeRefMT->referend)) {
    return buildInterfaceCall(globalState, functionState, builder, prototype, {regionInstanceRef, objectRef, dryRunBoolRef}, 0);
  } else {
    return buildCall(globalState, functionState, builder, prototype, {regionInstanceRef, objectRef, dryRunBoolRef});
  }
}

void Linear::defineSerializeFunc(
    Prototype* prototype) {
  auto intMT = globalState->metalCache->intRef;
  auto boolMT = globalState->metalCache->boolRef;
  auto valeStrMT = globalState->metalCache->strRef;
  auto nameM = globalState->measureName;

  defineFunctionBody(
      globalState, prototype,
      [&](FunctionState* functionState, LLVMBuilderRef builder) -> void {
        buildFlare(FL(), globalState, functionState, builder, "In serialize function!");

        auto valeObjectRefMT = prototype->params[1];
        auto hostObjectRefMT = prototype->returnType;

        auto regionInstanceRef = wrap(globalState->getRegion(regionRefMT), regionRefMT, LLVMGetParam(functionState->containingFuncL, 0));
        auto valeObjectRef = wrap(globalState->getRegion(valeObjectRefMT), valeObjectRefMT, LLVMGetParam(functionState->containingFuncL, 1));
        auto dryRunBoolRef = wrap(globalState->getRegion(boolMT), boolMT, LLVMGetParam(functionState->containingFuncL, 2));

        if (auto valeStructReferend = dynamic_cast<StructReferend*>(valeObjectRefMT->referend)) {
          auto hostReferend = hostReferendByValeReferend.find(valeStructReferend)->second;
          auto hostStructReferend = dynamic_cast<StructReferend*>(hostReferend);
          assert(hostStructReferend);
          auto valeStructDefM = globalState->program->getStruct(valeStructReferend->fullName);
          auto hostStructDefM = globalState->program->getStruct(hostStructReferend->fullName);

          buildFlare(FL(), globalState, functionState, builder);

          std::vector<Ref> memberRefs;

          for (int i = 0; i < valeStructDefM->members.size(); i++) {
            auto valeMemberM = valeStructDefM->members[i];
            auto hostMemberM = hostStructDefM->members[i];
            auto sourceMemberRefMT = valeMemberM->type;
            auto targetMemberRefMT =
                globalState->metalCache->getReference(
                    sourceMemberRefMT->ownership, sourceMemberRefMT->location, hostStructReferend);
            auto sourceMemberRef =
                globalState->getRegion(valeObjectRefMT)->loadMember(
                    functionState, builder, valeObjectRefMT, valeObjectRef, true,
                    i, valeMemberM->type, valeMemberM->type, valeMemberM->name);
            auto sourceMemberLE =
                globalState->getRegion(sourceMemberRefMT)->checkValidReference(
                    FL(), functionState, builder, sourceMemberRefMT, sourceMemberRef);
            if (sourceMemberRefMT == globalState->metalCache->intRef) {
              memberRefs.push_back(wrap(globalState->getRegion(targetMemberRefMT), targetMemberRefMT, sourceMemberLE));
            } else if (sourceMemberRefMT == globalState->metalCache->boolRef) {
              auto resultLE = LLVMBuildZExt(builder, sourceMemberLE, LLVMInt8TypeInContext(globalState->context), "boolAsI8");
              memberRefs.push_back(wrap(globalState->getRegion(targetMemberRefMT), targetMemberRefMT, resultLE));
            } else if (sourceMemberRefMT == globalState->metalCache->floatRef) {
              memberRefs.push_back(wrap(globalState->getRegion(targetMemberRefMT), targetMemberRefMT, sourceMemberLE));
            } else if (
                dynamic_cast<Str*>(sourceMemberRefMT->referend) ||
                dynamic_cast<StructReferend*>(sourceMemberRefMT->referend) ||
                dynamic_cast<InterfaceReferend*>(sourceMemberRefMT->referend) ||
                dynamic_cast<KnownSizeArrayT*>(sourceMemberRefMT->referend) ||
                dynamic_cast<UnknownSizeArrayT*>(sourceMemberRefMT->referend)) {
              auto destinationMemberRef =
                  callSerialize(
                      functionState, builder, sourceMemberRefMT, targetMemberRefMT, regionInstanceRef, sourceMemberRef, dryRunBoolRef);
              memberRefs.push_back(destinationMemberRef);
            } else assert(false);
          }

          auto resultRef =
              buildIfElse(
                  globalState,
                  functionState,
                  builder,
                  dryRunBoolRef,
                  translateType(hostObjectRefMT),
                  hostObjectRefMT,
                  hostObjectRefMT,
                  [this, regionInstanceRef, functionState, hostObjectRefMT, memberRefs](LLVMBuilderRef thenBuilder) {
                    return innerAllocate(regionInstanceRef, FL(), functionState, thenBuilder, hostObjectRefMT, memberRefs, true);
                  },
                  [this, regionInstanceRef, functionState, hostObjectRefMT, memberRefs](LLVMBuilderRef elseBuilder) {
                    return allocate(regionInstanceRef, FL(), functionState, elseBuilder, hostObjectRefMT, memberRefs);
                  });
//
//          // Remember, we're subtracting each size from a very large number, so its easier to round down
//          // to the next multiple of 16.
//          totalSizeIntLE = hexRoundDown(globalState, builder, totalSizeIntLE);

          auto resultRefLE = checkValidReference(FL(), functionState, builder, hostObjectRefMT, resultRef);

          buildFlare(FL(), globalState, functionState, builder, "Returning from serialize function!");

          LLVMBuildRet(builder, resultRefLE);
        } else if (dynamic_cast<Str*>(valeObjectRefMT->referend)) {
          auto lengthLE = globalState->getRegion(valeObjectRefMT)->getStringLen(functionState, builder, valeObjectRef);

          buildFlare(FL(), globalState, functionState, builder);

          auto strRef =
              buildIfElse(
                  globalState,
                  functionState,
                  builder,
                  dryRunBoolRef,
                  translateType(hostObjectRefMT),
                  hostObjectRefMT,
                  hostObjectRefMT,
                  [this, regionInstanceRef, functionState, lengthLE](LLVMBuilderRef thenBuilder) {
                    buildFlare(FL(), globalState, functionState, thenBuilder);

                    return innerMallocStr(regionInstanceRef, functionState, thenBuilder, lengthLE, true);
                  },
                  [this, regionInstanceRef, functionState, lengthLE, valeObjectRefMT, valeObjectRef](LLVMBuilderRef elseBuilder) {
                    buildFlare(FL(), globalState, functionState, elseBuilder);

                    auto strRef = mallocStr(regionInstanceRef, functionState, elseBuilder, lengthLE);

                    buildFlare(FL(), globalState, functionState, elseBuilder);

                    auto destCharsPtrLE = getStringBytesPtr(functionState, elseBuilder, strRef);

                    buildFlare(FL(), globalState, functionState, elseBuilder);

                    auto sourceCharsPtrLE = globalState->getRegion(valeObjectRefMT)->getStringBytesPtr(functionState, elseBuilder, valeObjectRef);

                    buildFlare(FL(), globalState, functionState, elseBuilder);

                    std::vector<LLVMValueRef> argsLE = { destCharsPtrLE, sourceCharsPtrLE, lengthLE };
                    LLVMBuildCall(elseBuilder, globalState->strncpy, argsLE.data(), argsLE.size(), "");

                    return strRef;
                  });

          buildFlare(FL(), globalState, functionState, builder, "Returning from serialize function!");

          LLVMBuildRet(builder, checkValidReference(FL(), functionState, builder, linearStrRefMT, strRef));
        } else assert(false);
      });
}

void Linear::bumpDestinationOffset(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    LLVMValueRef sizeIntLE) {
  auto regionInstancePtrLE =
      checkValidReference(FL(), functionState, builder, regionRefMT, regionInstanceRef);
  auto destinationOffsetPtrLE =
      LLVMBuildStructGEP(builder, regionInstancePtrLE, 1, "destinationOffsetPtr");
  auto destinationOffsetLE = LLVMBuildLoad(builder, destinationOffsetPtrLE, "destinationOffset");
  destinationOffsetLE = LLVMBuildSub(builder, destinationOffsetLE, sizeIntLE, "bumpedDestinationOffset");
  destinationOffsetLE = hexRoundDown(globalState, builder, destinationOffsetLE);
  LLVMBuildStore(builder, destinationOffsetLE, destinationOffsetPtrLE);
}

LLVMValueRef Linear::getDestinationOffset(
    LLVMBuilderRef builder,
    LLVMValueRef regionInstancePtrLE) {
  auto destinationOffsetPtrLE =
      LLVMBuildStructGEP(builder, regionInstancePtrLE, 1, "destinationOffsetPtr");
  return LLVMBuildLoad(builder, destinationOffsetPtrLE, "destinationOffset");
}

Ref Linear::getDestinationRef(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref regionInstanceRef,
    Reference* desiredRefMT) {
  auto regionInstancePtrLE =
      checkValidReference(FL(), functionState, builder, regionRefMT, regionInstanceRef);
  auto bufferBeginPtrPtrLE = LLVMBuildStructGEP(builder, regionInstancePtrLE, 0, "bufferBeginPtrPtr");
  auto bufferBeginPtrLE = LLVMBuildLoad(builder, bufferBeginPtrPtrLE, "bufferBeginPtr");

  auto destinationOffsetPtrLE =
      LLVMBuildStructGEP(builder, regionInstancePtrLE, 1, "destinationOffsetPtr");
  auto destinationOffsetLE = LLVMBuildLoad(builder, destinationOffsetPtrLE, "destinationOffset");

  auto destinationI8PtrLE = LLVMBuildGEP(builder, bufferBeginPtrLE, &destinationOffsetLE, 1, "destinationI8Ptr");

  auto desiredRefLT = translateType(desiredRefMT);
  auto destinationPtr = LLVMBuildBitCast(builder, destinationI8PtrLE, desiredRefLT, "destinationPtr");

  return wrap(this, desiredRefMT, destinationPtr);
}

void Linear::addSerializeFunctions() {
  auto program = globalState->program;

  auto boolMT = globalState->metalCache->boolRef;
  auto intMT = globalState->metalCache->intRef;
  auto intLT = globalState->getRegion(intMT)->translateType(intMT);

  // The actual LLVM name will be different, disambiguated.
  auto nameM = globalState->serializeName;

  std::vector<Prototype*> serializePrototypes;

  {
    auto valeStrMT = globalState->metalCache->strRef;
    auto prototype = globalState->metalCache->getPrototype(nameM, linearStrRefMT, {regionRefMT, valeStrMT, boolMT});
    auto nameL = globalState->serializeName->name + "_str";
    declareExtraFunction(globalState, prototype, nameL);
    serializePrototypes.push_back(prototype);
  }

  for (auto valeReferendAndHostReferend : hostReferendByValeReferend) {
    auto valeReferend = valeReferendAndHostReferend.first;
    if (dynamic_cast<InterfaceReferend*>(valeReferend) == nullptr) {
      auto sourceStructRefMT =
          globalState->metalCache->getReference(
              Ownership::SHARE, Location::YONDER, valeReferend);
      auto destStructRefMT = linearizeReference(sourceStructRefMT);
      auto prototype =
          globalState->metalCache->getPrototype(
              nameM, destStructRefMT, {regionRefMT, sourceStructRefMT, boolMT});
      auto nameL = globalState->serializeName->name + "_" + globalState->getReferendName(valeReferend)->name;
      declareExtraFunction(globalState, prototype, nameL);
      serializePrototypes.push_back(prototype);
    }
  }

  std::vector<Prototype*> thunkPrototypes;

  for (auto valeReferendAndHostReferend : hostReferendByValeReferend) {
    auto valeReferend = valeReferendAndHostReferend.first;
    if (auto valeInterface = dynamic_cast<InterfaceReferend*>(valeReferend)) {
      auto sourceInterfaceRefMT =
          globalState->metalCache->getReference(
              Ownership::SHARE, Location::YONDER, valeInterface);
      auto destInterfaceRefMT = linearizeReference(sourceInterfaceRefMT);
      auto interfacePrototype =
          globalState->metalCache->getPrototype(
              nameM, destInterfaceRefMT, {regionRefMT, sourceInterfaceRefMT, boolMT});
      declareExtraInterfaceMethod(
          globalState,
          valeInterface,
          new InterfaceMethod(interfacePrototype, 0),
          [this](Prototype *substructPrototype) {
            return globalState->lookupFunction(substructPrototype);
          },
          [](StructReferend *substruct, Prototype *substructPrototype) {});
    }
  }

  for (auto prototype : serializePrototypes) {
    defineSerializeFunc(prototype);
  }
}

Reference* Linear::linearizeReference(Reference* immRcRefMT) {
  assert(globalState->getRegion(immRcRefMT) == globalState->rcImm);

  auto hostReferendI = hostReferendByValeReferend.find(immRcRefMT->referend);
  assert(hostReferendI != hostReferendByValeReferend.end());
  auto hostReferend = hostReferendI->second;

  return globalState->metalCache->getReference(
      immRcRefMT->ownership, immRcRefMT->location, hostReferend);
}