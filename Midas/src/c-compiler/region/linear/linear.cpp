#include <function/expressions/shared/shared.h>
#include <utils/counters.h>
#include <utils/branch.h>
#include <region/common/controlblock.h>
#include <region/common/heap.h>
#include <function/expressions/shared/string.h>
#include <region/common/common.h>
#include <sstream>
#include <serialize.h>
#include <function/expressions/shared/elements.h>
#include "linear.h"
#include "translatetype.h"

Linear::Linear(GlobalState* globalState_)
  : globalState(globalState_),
    structs(globalState_) {
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
    if (referenceM->location == Location::INLINE) {
      auto innerStructL = structs.getStructStruct(structReferend);
      return innerStructL;
    } else {
      auto countedStructL = structs.getStructStruct(structReferend);
      return LLVMPointerType(countedStructL, 0);
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
    Ref refLE) {
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
  structs.declareKnownSizeArray(knownSizeArrayMT);
}

void Linear::declareUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  structs.declareUnknownSizeArray(unknownSizeArrayMT);
}

void Linear::translateUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto elementLT =
      translateType(
          unknownSizeArrayMT->rawArray->elementType);
  structs.translateUnknownSizeArray(unknownSizeArrayMT, elementLT);
}

void Linear::translateKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto elementLT =
      translateType(
          knownSizeArrayMT->rawArray->elementType);
  structs.translateKnownSizeArray(knownSizeArrayMT, elementLT);
}

void Linear::declareStruct(
    StructDefinition* structM) {
  structs.declareStruct(structM);
}

void Linear::translateStruct(
    StructDefinition* structM) {
  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        globalState->getRegion(structM->members[i]->type)
            ->translateType(structM->members[i]->type));
  }
  structs.translateStruct(structM, innerStructMemberTypesL);
}

void Linear::declareEdge(
    Edge* edge) {
  structs.declareEdge(edge);
}

void Linear::translateEdge(
    Edge* edge) {
  auto interfaceM = globalState->program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  std::tie(interfaceFunctionsLT, edgeFunctionsL) =
      globalState->getEdgeFunctionTypesAndFunctions(edge);

  structs.translateEdge(edge, interfaceFunctionsLT, edgeFunctionsL);
}

void Linear::declareInterface(
    InterfaceDefinition* interfaceM) {
  structs.declareInterface(interfaceM);
}

void Linear::translateInterface(
    InterfaceDefinition* interfaceM) {
  assert((uint64_t)interfaceM->referend > 0x10000);
  std::vector<LLVMTypeRef> interfaceMethodTypesL;
  for (int i = 0; i < interfaceM->methods.size(); i++) {
    interfaceMethodTypesL.push_back(
        LLVMPointerType(
            translateInterfaceMethodToFunctionType(
                globalState, this, interfaceM->methods[i]),
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
    LLVMValueRef newValueLE) {
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
      checkValidReference(FL(), functionState, builder, globalState->metalCache.strRef, ref);
  return structs.getStringBytesPtr(functionState, builder, strWrapperPtrLE);
}

Ref Linear::allocate(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* desiredReference,
    const std::vector<Ref>& membersLE) {
  // The serialize functions for each region are what populates the linear region.
  assert(false);
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
    const std::vector<Ref>& membersLE) {
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
    Ref refLE) {
  auto argLE = checkValidReference(FL(), functionState, builder, refMT, refLE);
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
  auto sizeRef = wrap(this, globalState->metalCache.intRef, sizeLE);
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
  // The serialize functions for each region are what populates the linear region.
  assert(false);
}


WrapperPtrLE Linear::mallocStr(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  // The serialize functions for each region are what populates the linear region.
  assert(false);
}

LLVMValueRef Linear::getStringLen(FunctionState* functionState, LLVMBuilderRef builder, Ref ref) {
  auto refPtrLE = checkValidReference(FL(), functionState, builder, globalState->metalCache.strRef, ref);
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

void Linear::generateStructDefsC(std::unordered_map<std::string, std::string>* cByExportedName, StructDefinition* structDefM) {
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

void Linear::generateInterfaceDefsC(std::unordered_map<std::string, std::string>* cByExportedName, InterfaceDefinition* interfaceDefM) {
  auto name = globalState->program->getExportedName(interfaceDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused1; void* unused2; } " << name << ";";
  cByExportedName->insert(std::make_pair(name, s.str()));
}

LLVMTypeRef Linear::getExternalType(
    Reference* refMT) {
  assert(false);
//  assert(refMT->ownership == Ownership::SHARE);
//
//  if (refMT == globalState->metalCache.intRef) {
//    return LLVMInt64TypeInContext(globalState->context);
//  } else if (refMT == globalState->metalCache.boolRef) {
//    return LLVMInt8TypeInContext(globalState->context);
//  } else if (refMT == globalState->metalCache.floatRef) {
//    return LLVMDoubleTypeInContext(globalState->context);
//  } else if (refMT == globalState->metalCache.strRef) {
//    auto structLIter = externalStructLByReferend.find(globalState->metalCache.str);
//    assert(structLIter != externalStructLByReferend.end());
//    auto structL = structLIter->second;
//    return LLVMPointerType(structL, 0);
//  } else if (refMT == globalState->metalCache.neverRef) {
//    assert(false); // How can we hand a never into something?
//    return nullptr;
//  } else if (auto usa = dynamic_cast<UnknownSizeArrayT*>(refMT->referend)) {
//    auto structLIter = externalStructLByReferend.find(globalState->metalCache.str);
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


Ref Linear::receiveUnencryptedAlienReference(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceRefMT,
    Ref sourceRef) {
  assert(sourceRefMT->ownership == Ownership::SHARE);

  auto sourceRegion = globalState->getRegion(sourceRefMT);
  assert(sourceRegion == globalState->rcImm);
  // Someday when we include the region in the coord, this line will change.
  auto targetRefMT = sourceRefMT;

  auto sourceRefLE =
      globalState->getRegion(sourceRefMT)
          ->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceRef);

  if (sourceRefMT == globalState->metalCache.intRef) {
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, sourceRefLE);
  } else if (sourceRefMT == globalState->metalCache.boolRef) {
    auto resultLE =
        LLVMBuildZExt(
            builder, sourceRefLE, LLVMInt8TypeInContext(globalState->context), "boolAsI8");
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, resultLE);
  } else if (sourceRefMT == globalState->metalCache.floatRef) {
    return wrap(globalState->getRegion(sourceRefMT), targetRefMT, sourceRefLE);
  } else if (dynamic_cast<Str*>(sourceRefMT->referend) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRefMT->referend) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRefMT->referend)) {
    auto sizeIntRef =
        getCalculatedSerializedSize(
            globalState, functionState, builder, sourceRefMT, {sourceRef});
    auto sizeIntLE =
        globalState->getRegion(globalState->metalCache.intRef)
            ->checkValidReference(FL(), functionState, builder, globalState->metalCache.intRef, sizeIntRef);
    auto bufferPtrLE = callMalloc(globalState, builder, sizeIntLE);
    auto serializePrototype =
        globalState->metalCache.getPrototype(
            globalState->serializeName, globalState->metalCache.intRef, { sourceRefMT });
    buildCall(globalState, functionState, builder, serializePrototype, {sourceRef});
    return wrap(this, targetRefMT, bufferPtrLE);
  } else if (dynamic_cast<StructReferend*>(sourceRefMT->referend)) {
    if (sourceRefMT->location == Location::INLINE) {
      if (sourceRefMT == globalState->metalCache.emptyTupleStructRef) {
        return makeEmptyTupleRef(globalState, this, builder);
      } else {
        assert(false);
      }
    } else {
      assert(false);
    }
  } else if (dynamic_cast<InterfaceReferend*>(sourceRefMT->referend)) {
    if (sourceRefMT->location == Location::INLINE && dynamic_cast<StructReferend*>(sourceRefMT->referend)) {
      assert(false);
    } else {
      assert(false);
    }
  } else assert(false);

  assert(false);
}

LLVMTypeRef Linear::getInterfaceMethodVirtualParamAnyType(Reference* reference) {
  return LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);
}

Ref Linear::predictShallowSize(FunctionState* functionState, LLVMBuilderRef builder, Referend* referend, Ref lenIntRef) {
  if (referend == globalState->metalCache.str) {
    return globalState->buildAdd(functionState, builder,
        globalState->constI64(LLVMABISizeOfType(globalState->dataLayout, structs.getStringStruct())),
        lenIntRef);
  } else if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    return globalState->constI64(LLVMABISizeOfType(globalState->dataLayout, structs.getStructStruct(structReferend)));
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
