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
    structs(globalState_),
    hostReferendByValeReferend(0, globalState->addressNumberer->makeHasher<Referend*>()),
    valeReferendByHostReferend(0, globalState->addressNumberer->makeHasher<Referend*>()) {
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

  addMappedReferend(globalState->metalCache->innt, globalState->metalCache->getInt(getRegionId()));
  addMappedReferend(globalState->metalCache->boool, globalState->metalCache->getBool(getRegionId()));
  addMappedReferend(globalState->metalCache->flooat, globalState->metalCache->getFloat(getRegionId()));
  addMappedReferend(globalState->metalCache->str, linearStr);
  addMappedReferend(globalState->metalCache->never, globalState->metalCache->getNever(getRegionId()));

  globalState->regionIdByReferend.emplace(regionReferend, globalState->metalCache->linearRegionId);
  structs.declareStruct(regionReferend);
  std::vector<LLVMTypeRef> membersLT = {
      LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
      LLVMInt64TypeInContext(globalState->context),
  };
  structs.translateStruct(regionReferend, membersLT);
}

void Linear::declareExtraFunctions() {
  auto boolMT = globalState->metalCache->boolRef;
  auto valeStrMT = globalState->metalCache->strRef;
  auto prototype =
      globalState->metalCache->getPrototype(
          globalState->serializeName, linearStrRefMT, {regionRefMT, valeStrMT, boolMT});
  auto nameL = globalState->serializeName->name + "__str";
  declareExtraFunction(globalState, prototype, nameL);
}

void Linear::defineExtraFunctions() {
  defineConcreteSerializeFunction(globalState->metalCache->str);
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
  auto sourceRefLE = checkValidReference(FL(), functionState, builder, sourceMT, sourceRef);

  auto sourceI8PtrLE =
      LLVMBuildPointerCast(
          builder,
          sourceRefLE,
          LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
          "extStrPtrLE");

  LLVMBuildCall(builder, globalState->free, &sourceI8PtrLE, 1, "");
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
    KnownSizeArrayDefinitionT* ksaDefM) {

  auto hostName = globalState->metalCache->getName(namePrefix + "_" + ksaDefM->name->name);
  auto hostReferend = globalState->metalCache->getKnownSizeArray(hostName);
  addMappedReferend(ksaDefM->referend, hostReferend);
  globalState->regionIdByReferend.emplace(hostReferend, getRegionId());

  structs.declareKnownSizeArray(hostReferend);

  declareConcreteSerializeFunction(ksaDefM->referend);
}

void Linear::declareUnknownSizeArray(
    UnknownSizeArrayDefinitionT* usaDefM) {
  auto hostName = globalState->metalCache->getName(namePrefix + "_" + usaDefM->name->name);
  auto hostReferend = globalState->metalCache->getUnknownSizeArray(hostName);
  addMappedReferend(usaDefM->referend, hostReferend);
  globalState->regionIdByReferend.emplace(hostReferend, getRegionId());

  structs.declareUnknownSizeArray(hostReferend);

  declareConcreteSerializeFunction(usaDefM->referend);
}

void Linear::translateUnknownSizeArray(
    UnknownSizeArrayDefinitionT* unknownSizeArrayMT) {
  auto elementLT =
      translateType(
          linearizeReference(
              unknownSizeArrayMT->rawArray->elementType));
  auto hostReferend = hostReferendByValeReferend.find(unknownSizeArrayMT->referend)->second;
  auto hostUsaMT = dynamic_cast<UnknownSizeArrayT*>(hostReferend);
  assert(hostUsaMT);
  structs.translateUnknownSizeArray(hostUsaMT, elementLT);
}
void Linear::addUnknownSizeArrayExtraFunctions(UnknownSizeArrayDefinitionT* usaDefM) {
  defineConcreteSerializeFunction(usaDefM->referend);
}

void Linear::translateKnownSizeArray(
    KnownSizeArrayDefinitionT* knownSizeArrayMT) {
  auto ksaDef = globalState->program->getKnownSizeArray(knownSizeArrayMT->name);
  auto elementLT =
      translateType(
          linearizeReference(
              knownSizeArrayMT->rawArray->elementType));
  auto hostReferend = hostReferendByValeReferend.find(knownSizeArrayMT->referend)->second;
  auto hostKsaMT = dynamic_cast<KnownSizeArrayT*>(hostReferend);
  assert(hostKsaMT);

  structs.translateKnownSizeArray(hostKsaMT, ksaDef->size, elementLT);
}
void Linear::addKnownSizeArrayExtraFunctions(KnownSizeArrayDefinitionT* ksaDef) {
  defineConcreteSerializeFunction(ksaDef->referend);
}

void Linear::declareStruct(
    StructDefinition* structM) {

  auto hostName = globalState->metalCache->getName(namePrefix + "_" + structM->name->name);
  auto hostReferend = globalState->metalCache->getStructReferend(hostName);
  addMappedReferend(structM->referend, hostReferend);
  globalState->regionIdByReferend.emplace(hostReferend, getRegionId());

  structs.declareStruct(hostReferend);

  declareConcreteSerializeFunction(structM->referend);
}

void Linear::translateStruct(
    StructDefinition* structM) {
  auto hostReferend = hostReferendByValeReferend.find(structM->referend)->second;
  auto hostStructMT = dynamic_cast<StructReferend*>(hostReferend);
  assert(hostStructMT);

  std::vector<LLVMTypeRef> innerStructMemberTypesL;
  for (int i = 0; i < structM->members.size(); i++) {
    innerStructMemberTypesL.push_back(
        translateType(linearizeReference(structM->members[i]->type)));
  }
  structs.translateStruct(hostStructMT, innerStructMemberTypesL);
}

void Linear::addStructExtraFunctions(StructDefinition* structDefM) {
  defineConcreteSerializeFunction(structDefM->referend);
}

void Linear::declareEdge(Edge* edge) {
  auto hostStructReferend = dynamic_cast<StructReferend*>(hostReferendByValeReferend.find(edge->structName)->second);
  assert(hostStructReferend);
  auto hostInterfaceReferend = dynamic_cast<InterfaceReferend*>(hostReferendByValeReferend.find(edge->interfaceName)->second);
  assert(hostInterfaceReferend);

  structs.declareEdge(hostStructReferend, hostInterfaceReferend);

  auto interfaceMethod = getSerializeInterfaceMethod(edge->interfaceName);
  auto thunkPrototype = getSerializeThunkPrototype(edge->structName, edge->interfaceName);
  globalState->addEdgeExtraMethod(edge, interfaceMethod, thunkPrototype);
  auto nameL = globalState->serializeName->name + "__" + edge->interfaceName->fullName->name + "__" + edge->structName->fullName->name;
  declareExtraFunction(globalState, thunkPrototype, nameL);
}

void Linear::translateEdge(Edge* edge) {
  auto boolMT = globalState->metalCache->boolRef;
//  auto interfaceM = globalState->program->getInterface(edge->interfaceName->fullName);

  auto interfaceFunctionsLT = globalState->getInterfaceFunctionTypes(edge->interfaceName);
  auto edgeFunctionsL = globalState->getEdgeFunctions(edge);
  structs.translateEdge(edge, interfaceFunctionsLT, edgeFunctionsL);

  auto thunkPrototype = getSerializeThunkPrototype(edge->structName, edge->interfaceName);
  defineFunctionBody(
      globalState, thunkPrototype,
      [&](FunctionState* functionState, LLVMBuilderRef builder) {
        auto structPrototype = getSerializePrototype(edge->structName);

        auto valeObjectRefMT = structPrototype->params[1];

        auto regionInstanceRef = wrap(globalState->getRegion(regionRefMT), regionRefMT, LLVMGetParam(functionState->containingFuncL, 0));
        auto valeObjectRef = wrap(globalState->getRegion(valeObjectRefMT), valeObjectRefMT, LLVMGetParam(functionState->containingFuncL, 1));
        auto dryRunBoolRef = wrap(globalState->getRegion(boolMT), boolMT, LLVMGetParam(functionState->containingFuncL, 2));

        auto structRef = buildCall(globalState, functionState, builder, structPrototype, {regionInstanceRef, valeObjectRef, dryRunBoolRef});

        auto hostInterfaceReferend = dynamic_cast<InterfaceReferend*>(thunkPrototype->returnType->referend);
        assert(hostInterfaceReferend);
        auto hostStructReferend = dynamic_cast<StructReferend*>(structPrototype->returnType->referend);
        assert(hostStructReferend);

        auto interfaceRef =
            upcast(
                functionState, builder, structPrototype->returnType, hostStructReferend,
                structRef, thunkPrototype->returnType, hostInterfaceReferend);
        auto interfaceRefLE = checkValidReference(FL(), functionState, builder, thunkPrototype->returnType, interfaceRef);
        LLVMBuildRet(builder, interfaceRefLE);
      });
}

void Linear::declareInterface(
    InterfaceDefinition* interfaceM) {
  auto hostName = globalState->metalCache->getName(namePrefix + "_" + interfaceM->name->name);
  auto hostReferend = globalState->metalCache->getInterfaceReferend(hostName);
  addMappedReferend(interfaceM->referend, hostReferend);
  globalState->regionIdByReferend.emplace(hostReferend, getRegionId());

  structs.declareInterface(hostReferend);

  declareInterfaceSerializeFunction(interfaceM->referend);
}

void Linear::declareInterfaceSerializeFunction(InterfaceReferend* valeInterface) {
  auto interfaceMethod = getSerializeInterfaceMethod(valeInterface);
  globalState->addInterfaceExtraMethod(valeInterface, interfaceMethod);
}

void Linear::translateInterface(InterfaceDefinition* interfaceM) {
  auto hostReferend = hostReferendByValeReferend.find(interfaceM->referend)->second;
  auto hostInterfaceMT = dynamic_cast<InterfaceReferend*>(hostReferend);
  assert(hostInterfaceMT);

  auto interfaceMethodTypesL = globalState->getInterfaceFunctionTypes(interfaceM->referend);
  structs.translateInterface(hostInterfaceMT);
}
void Linear::addInterfaceExtraFunctions(InterfaceDefinition* interfaceDefM) {
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
  return innerAllocate(regionInstanceRef, from, functionState, builder, structRefMT, memberRefs, globalState->constI1(false));
}

Ref Linear::innerAllocate(
    Ref regionInstanceRef,
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* hostStructRefMT,
    const std::vector<Ref>& memberHostRefs,
    Ref dryRunBoolRef) {

  auto intMT = globalState->metalCache->intRef;
  auto boolMT = globalState->metalCache->boolRef;

  auto valeStructRefMT = unlinearizeReference(hostStructRefMT);
  auto desiredValeStructMT = dynamic_cast<StructReferend*>(valeStructRefMT->referend);
  assert(desiredValeStructMT);
  auto valeStructDefM = globalState->program->getStruct(desiredValeStructMT->fullName);

  LLVMValueRef substructSizeIntLE =
      predictShallowSize(builder, hostStructRefMT->referend, constI64LE(globalState, 0));
  bumpDestinationOffset(functionState, builder, regionInstanceRef, substructSizeIntLE);

  auto objectRef = getDestinationRef(functionState, builder, regionInstanceRef, hostStructRefMT);
  auto objectPtrLE = checkValidReference(FL(), functionState, builder, hostStructRefMT, objectRef);

  auto dryRunBoolLE = globalState->getRegion(boolMT)->checkValidReference(FL(), functionState, builder, boolMT, dryRunBoolRef);

  buildIf(
      globalState, functionState, builder, LLVMBuildNot(builder, dryRunBoolLE, "notDryRun"),
      [this, functionState, valeStructDefM, memberHostRefs, objectPtrLE](LLVMBuilderRef elseBuilder) {
        fillLinearInnerStruct(functionState, elseBuilder, valeStructDefM, memberHostRefs, objectPtrLE);
      });

  return objectRef;
}

Ref Linear::upcast(
    FunctionState* functionState,
    LLVMBuilderRef builder,

    Reference* sourceStructMT,
    StructReferend* sourceStructReferendM,
    Ref sourceRef,

    Reference* targetInterfaceTypeM,
    InterfaceReferend* targetInterfaceReferendM) {
  assert(valeReferendByHostReferend.find(sourceStructMT->referend) != valeReferendByHostReferend.end());
  assert(valeReferendByHostReferend.find(sourceStructReferendM) != valeReferendByHostReferend.end());
  assert(valeReferendByHostReferend.find(targetInterfaceTypeM->referend) != valeReferendByHostReferend.end());
  assert(valeReferendByHostReferend.find(targetInterfaceReferendM) != valeReferendByHostReferend.end());

  auto i8PtrLT = LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);

  auto structRefLE = checkValidReference(FL(), functionState, builder, sourceStructMT, sourceRef);
  auto structI8PtrLE = LLVMBuildPointerCast(builder, structRefLE, i8PtrLT, "objAsVoidPtr");

  auto interfaceRefLT = structs.getInterfaceRefStruct(targetInterfaceReferendM);

  auto interfaceRefLE = LLVMGetUndef(interfaceRefLT);
  interfaceRefLE = LLVMBuildInsertValue(builder, interfaceRefLE, structI8PtrLE, 0, "interfaceRefWithOnlyObj");
  auto edgeNumber = structs.getEdgeNumber(targetInterfaceReferendM, sourceStructReferendM);
  LLVMValueRef edgeNumberLE = constI64LE(globalState, edgeNumber);
  interfaceRefLE = LLVMBuildInsertValue(builder, interfaceRefLE, edgeNumberLE, 1, "interfaceRef");

  return wrap(globalState->getRegion(targetInterfaceTypeM), targetInterfaceTypeM, interfaceRefLE);
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
  auto ksaDef = globalState->program->getKnownSizeArray(ksaMT->name);
  return loadElementFromKSAInner(
      globalState, functionState, builder, ksaRefMT, ksaMT, ksaDef->size, ksaDef->rawArray->mutability, ksaDef->rawArray->elementType, indexRef, elementsPtrLE);
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

  auto usaDef = globalState->program->getKnownSizeArray(usaMT->name);
  return loadElementWithoutUpgrade(
      globalState, functionState, builder, usaRefMT,
      usaDef->rawArray->elementType,
      sizeRef, elementsPtrLE, usaDef->rawArray->mutability, indexRef);
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

Ref Linear::constructUnknownSizeArray(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaMT,
    UnknownSizeArrayT* unknownSizeArrayT,
    Ref sizeRef,
    const std::string& typeName) {
  return innerConstructUnknownSizeArray(
      regionInstanceRef, functionState, builder, usaMT, unknownSizeArrayT, sizeRef, typeName, globalState->constI1(false));
}

Ref Linear::constructKnownSizeArray(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* ksaRefMT,
    KnownSizeArrayT* ksaMT) {
  return innerConstructKnownSizeArray(
      regionInstanceRef, functionState, builder, ksaRefMT, ksaMT, globalState->constI1(false));
}

Ref Linear::innerConstructKnownSizeArray(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* ksaRefMT,
    KnownSizeArrayT* hostKsaMT,
    Ref dryRunBoolRef) {
  buildFlare(FL(), globalState, functionState, builder);

  auto boolMT = globalState->metalCache->boolRef;
  auto intRefMT = globalState->metalCache->intRef;

  assert(ksaRefMT->referend == hostKsaMT);
  assert(globalState->getRegion(hostKsaMT) == this);

  auto valeReferendMT = valeReferendByHostReferend.find(hostKsaMT)->second;
  auto valeKsaMT = dynamic_cast<KnownSizeArrayT*>(valeReferendMT);
  assert(valeKsaMT);

  auto valeKsaDefM = globalState->program->getKnownSizeArray(valeKsaMT->name);
  auto sizeLE = predictShallowSize(builder, hostKsaMT, constI64LE(globalState, valeKsaDefM->size));
  bumpDestinationOffset(functionState, builder, regionInstanceRef, sizeLE);

  auto ksaRef = getDestinationRef(functionState, builder, regionInstanceRef, ksaRefMT);
  auto ksaPtrLE = checkValidReference(FL(), functionState, builder, ksaRefMT, ksaRef);

  auto dryRunBoolLE = globalState->getRegion(boolMT)->checkValidReference(FL(), functionState, builder, boolMT, dryRunBoolRef);
  buildIf(
      globalState, functionState, builder, LLVMBuildNot(builder, dryRunBoolLE, "notDryRun"),
      [this, functionState, ksaPtrLE, hostKsaMT](LLVMBuilderRef thenBuilder) mutable {
        buildFlare(FL(), globalState, functionState, thenBuilder);

        auto ksaLT = structs.getKnownSizeArrayStruct(hostKsaMT);
        auto ksaValLE = LLVMGetUndef(ksaLT); // There are no fields
        LLVMBuildStore(thenBuilder, ksaValLE, ksaPtrLE);

        buildFlare(FL(), globalState, functionState, thenBuilder);

        // Caller still needs to initialize the elements!
      });

  buildFlare(FL(), globalState, functionState, builder);

  return ksaRef;
}

Ref Linear::innerConstructUnknownSizeArray(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref sizeRef,
    const std::string& typeName,
    Ref dryRunBoolRef) {
  buildFlare(FL(), globalState, functionState, builder);

  auto boolMT = globalState->metalCache->boolRef;
  auto intRefMT = globalState->metalCache->intRef;

  assert(usaRefMT->referend == usaMT);
  assert(globalState->getRegion(usaMT) == this);

  auto lenLE = globalState->getRegion(intRefMT)->checkValidReference(FL(), functionState, builder, intRefMT, sizeRef);
  auto lenI64LE = LLVMBuildZExt(builder, lenLE, LLVMInt64TypeInContext(globalState->context), "");

  auto sizeLE = predictShallowSize(builder, usaMT, lenI64LE);
  bumpDestinationOffset(functionState, builder, regionInstanceRef, sizeLE);

  auto usaRef = getDestinationRef(functionState, builder, regionInstanceRef, usaRefMT);
  auto usaPtrLE = checkValidReference(FL(), functionState, builder, usaRefMT, usaRef);

  auto dryRunBoolLE = globalState->getRegion(boolMT)->checkValidReference(FL(), functionState, builder, boolMT, dryRunBoolRef);
  buildIf(
      globalState, functionState, builder, LLVMBuildNot(builder, dryRunBoolLE, "notDryRun"),
      [this, functionState, usaPtrLE, lenI64LE, usaMT](LLVMBuilderRef thenBuilder) mutable {
        buildFlare(FL(), globalState, functionState, thenBuilder);

        auto usaLT = structs.getUnknownSizeArrayStruct(usaMT);
        auto usaWithLenVal = LLVMBuildInsertValue(thenBuilder, LLVMGetUndef(usaLT), lenI64LE, 0, "usaWithLen");
        LLVMBuildStore(thenBuilder, usaWithLenVal, usaPtrLE);

        buildFlare(FL(), globalState, functionState, thenBuilder);

        // Caller still needs to initialize the elements!
      });

  buildFlare(FL(), globalState, functionState, builder);

  return usaRef;
}

Ref Linear::mallocStr(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE,
    LLVMValueRef sourceCharsPtrLE) {
  return innerMallocStr(regionInstanceRef, functionState, builder, lengthLE, sourceCharsPtrLE, globalState->constI1(false));
}

Ref Linear::innerMallocStr(
    Ref regionInstanceRef,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE,
    LLVMValueRef sourceCharsPtrLE,
    Ref dryRunBoolRef) {
  auto boolMT = globalState->metalCache->boolRef;

  auto lenI64LE = LLVMBuildZExt(builder, lengthLE, LLVMInt64TypeInContext(globalState->context), "");

  auto sizeLE = predictShallowSize(builder, linearStr, lengthLE);
  bumpDestinationOffset(functionState, builder, regionInstanceRef, sizeLE);

  auto strRef = getDestinationRef(functionState, builder, regionInstanceRef, linearStrRefMT);
  auto strPtrLE = checkValidReference(FL(), functionState, builder, linearStrRefMT, strRef);

  auto dryRunBoolLE = globalState->getRegion(boolMT)->checkValidReference(FL(), functionState, builder, boolMT, dryRunBoolRef);

  buildIf(
      globalState, functionState, builder, LLVMBuildNot(builder, dryRunBoolLE, "notDryRun"),
      [this, functionState, strPtrLE, lenI64LE, strRef, lengthLE, sourceCharsPtrLE](LLVMBuilderRef thenBuilder) mutable {
        auto strWithLenValLE = LLVMBuildInsertValue(thenBuilder, LLVMGetUndef(structs.getStringStruct()), lenI64LE, 0, "strWithLen");
        LLVMBuildStore(thenBuilder, strWithLenValLE, strPtrLE);

        buildFlare(FL(), globalState, functionState, thenBuilder, "length for str: ", lenI64LE);

        auto charsBeginPtr = getStringBytesPtr(functionState, thenBuilder, strRef);

        std::vector<LLVMValueRef> argsLE = { charsBeginPtr, sourceCharsPtrLE, lengthLE };
        LLVMBuildCall(thenBuilder, globalState->strncpy, argsLE.data(), argsLE.size(), "");

        auto charsEndPtr = LLVMBuildGEP(thenBuilder, charsBeginPtr, &lengthLE, 1, "charsEndPtr");
        LLVMBuildStore(thenBuilder, constI8LE(globalState, 0), charsEndPtr);

        return strRef;
      });

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

std::string Linear::getRefNameC(Reference* hostRefMT) {
  assert(valeReferendByHostReferend.find(hostRefMT->referend) != valeReferendByHostReferend.end());

  auto hostMT = hostRefMT->referend;
  if (dynamic_cast<Int *>(hostMT)) {
    return "int64_t";
  } else if (dynamic_cast<Bool *>(hostMT)) {
    return "int8_t";
  } else if (dynamic_cast<Float *>(hostMT)) {
    return "double";
  } else if (dynamic_cast<Str *>(hostMT)) {
    return "ValeStr*";
  } else if (auto hostInterfaceMT = dynamic_cast<InterfaceReferend *>(hostMT)) {
    auto valeMT = valeReferendByHostReferend.find(hostMT)->second;
    auto valeInterfaceMT = dynamic_cast<InterfaceReferend*>(valeMT);
    assert(valeInterfaceMT);
    auto baseName = globalState->program->getExportedName(valeInterfaceMT->fullName);
    assert(hostRefMT->ownership == Ownership::SHARE);
    if (hostRefMT->location == Location::INLINE) {
      return baseName;
    } else {
      return baseName + "*";
    };
  } else if (auto hostStructMT = dynamic_cast<StructReferend *>(hostMT)) {
    auto valeMT = valeReferendByHostReferend.find(hostMT)->second;
    auto valeStructMT = dynamic_cast<StructReferend*>(valeMT);
    assert(valeStructMT);
    if (valeStructMT == globalState->metalCache->emptyTupleStruct) {
      return "void";
    }
    auto baseName = globalState->program->getExportedName(valeStructMT->fullName);
    assert(hostRefMT->ownership == Ownership::SHARE);
    if (hostRefMT->location == Location::INLINE) {
      return baseName;
    } else {
      return baseName + "*";
    }
  } else if (dynamic_cast<KnownSizeArrayT *>(hostMT) ||
             dynamic_cast<UnknownSizeArrayT *>(hostMT)) {
    assert(false); // impl
  } else {
    std::cerr << "Unimplemented type in immutables' getRefNameC: "
              << typeid(*hostRefMT->referend).name() << std::endl;
    assert(false);
  }
}

void Linear::generateStructDefsC(
    std::unordered_map<std::string, std::string>* cByExportedName,
    StructDefinition* structDefM) {
  auto name = globalState->program->getExportedName(structDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << " {" << std::endl;
  for (int i = 0; i < structDefM->members.size(); i++) {
    auto member = structDefM->members[i];
    auto hostMT = hostReferendByValeReferend.find(member->type->referend)->second;
    auto hostRefMT = globalState->metalCache->getReference(member->type->ownership, member->type->location, hostMT);
    s << "  " << getRefNameC(hostRefMT) << " " << member->name << ";" << std::endl;
  }
  s << "} " << name << ";" << std::endl;

  cByExportedName->insert(std::make_pair(name, s.str()));
}

void Linear::generateInterfaceDefsC(
    std::unordered_map<std::string, std::string>* cByExportedName,
    InterfaceDefinition* interfaceDefM) {
  auto name = globalState->program->getExportedName(interfaceDefM->referend->fullName);
  std::stringstream s;

  auto hostReferend = hostReferendByValeReferend.find(interfaceDefM->referend)->second;
  auto hostInterfaceReferend = dynamic_cast<InterfaceReferend*>(hostReferend);
  assert(hostInterfaceReferend);

  s << "typedef enum " << name << "_Type {" << std::endl;
  for (auto hostStructReferend : structs.getOrderedEdges(hostInterfaceReferend)) {
    auto valeReferend = valeReferendByHostReferend.find(hostStructReferend)->second;
    auto valeStructReferend = dynamic_cast<StructReferend*>(valeReferend);
    assert(valeStructReferend);
    s << "  " << name << "_" << globalState->program->getExportedName(valeStructReferend->fullName) << "," << std::endl;
  }
  s << "} " << name << "_Type;" << std::endl;

  s << "typedef struct " << name << " { void* obj; " << name << "_Type type; } " << name << ";" << std::endl;

  cByExportedName->insert(std::make_pair(name, s.str()));
}

void Linear::generateUnknownSizeArrayDefsC(
    std::unordered_map<std::string, std::string>* cByExportedName,
    UnknownSizeArrayDefinitionT* usaDefM) {
  auto name = globalState->program->getExportedName(usaDefM->name);

  auto valeMemberRefMT = usaDefM->rawArray->elementType;
  auto hostMemberRefMT = linearizeReference(valeMemberRefMT);
  auto hostMemberRefName = getRefNameC(hostMemberRefMT);

  std::stringstream s;
  s << "typedef struct " << name << " {" << std::endl;
  s << "  uint64_t length;" << std::endl;
  s << "  " << hostMemberRefName << " elements[0];" << std::endl;
  s << "} " << name << ";" << std::endl;

  cByExportedName->insert(std::make_pair(name, s.str()));
}

void Linear::generateKnownSizeArrayDefsC(
    std::unordered_map<std::string, std::string>* cByExportedName,
    KnownSizeArrayDefinitionT* usaDefM) {
}

Reference* Linear::getExternalType(Reference* refMT) {
  return refMT;
}

Ref Linear::topLevelSerialize(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Referend* valeReferend,
    Ref ref) {
  auto nullLT = LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0));
  auto dryRunCounterBeginLE = constI64LE(globalState, 0x4000000000000000UL);

  auto regionLT = structs.getStructStruct(regionReferend);

  auto dryRunInitialRegionStructLE = LLVMGetUndef(regionLT);
  dryRunInitialRegionStructLE = LLVMBuildInsertValue(builder, dryRunInitialRegionStructLE, nullLT, 0, "regionStruct");
  dryRunInitialRegionStructLE = LLVMBuildInsertValue(builder, dryRunInitialRegionStructLE, dryRunCounterBeginLE, 1, "regionStruct");
  auto dryRunRegionInstancePtrLE = makeMidasLocal(functionState, builder, regionLT, "region", dryRunInitialRegionStructLE);
  auto dryRunRegionInstanceRef = wrap(this, regionRefMT, dryRunRegionInstancePtrLE);

  callSerialize(functionState, builder, valeReferend, dryRunRegionInstanceRef, ref, globalState->constI1(true));

  auto dryRunFinalOffsetLE = getDestinationOffset(builder, dryRunRegionInstancePtrLE);
  auto sizeIntLE = LLVMBuildSub(builder, dryRunCounterBeginLE, dryRunFinalOffsetLE, "size");

  LLVMValueRef bufferBeginPtrLE = callMalloc(globalState, builder, sizeIntLE);

  auto initialRegionStructLE = LLVMGetUndef(regionLT);
  initialRegionStructLE = LLVMBuildInsertValue(builder, initialRegionStructLE, bufferBeginPtrLE, 0, "regionStruct");
  initialRegionStructLE = LLVMBuildInsertValue(builder, initialRegionStructLE, sizeIntLE, 1, "regionStruct");
  auto regionInstancePtrLE = makeMidasLocal(functionState, builder, regionLT, "region", initialRegionStructLE);
  auto regionInstanceRef = wrap(this, regionRefMT, regionInstancePtrLE);

  auto resultRef = callSerialize(functionState, builder, valeReferend, regionInstanceRef, ref, globalState->constI1(false));

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
      if (sourceRefMT == globalState->metalCache->emptyTupleStructRef) {
        auto emptyTupleRefMT = linearizeReference(globalState->metalCache->emptyTupleStructRef);
        return wrap(this, emptyTupleRefMT, LLVMGetUndef(translateType(emptyTupleRefMT)));
      } else {
        assert(false);
      }
    } else {
      return topLevelSerialize(functionState, builder, sourceRefMT->referend, sourceRef);
    }
  } else assert(false);

  assert(false);
}

LLVMTypeRef Linear::getInterfaceMethodVirtualParamAnyType(Reference* reference) {
  return LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0);
}

LLVMValueRef Linear::predictShallowSize(LLVMBuilderRef builder, Referend* referend, LLVMValueRef lenIntLE) {
  assert(globalState->getRegion(referend) == this);
  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    return constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, structs.getStructStruct(structReferend)));
  } else if (referend == linearStr) {
    auto headerBytesLE =
        constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, structs.getStringStruct()));
    return LLVMBuildAdd(builder, headerBytesLE, lenIntLE, "sum");
  } else if (auto hostUsaMT = dynamic_cast<UnknownSizeArrayT*>(referend)) {
    auto headerBytesLE =
        constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, structs.getUnknownSizeArrayStruct(hostUsaMT)));

    auto valeReferendMT = valeReferendByHostReferend.find(hostUsaMT)->second;
    auto valeUsaMT = dynamic_cast<UnknownSizeArrayT*>(valeReferendMT);
    assert(valeUsaMT);
    auto valeElementRefMT = globalState->program->getUnknownSizeArray(valeUsaMT->name)->rawArray->elementType;
    auto hostElementRefMT = linearizeReference(valeElementRefMT);
    auto hostElementRefLT = translateType(hostElementRefMT);

    auto sizePerElement = LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(hostElementRefLT, 1));
    // The above line tries to include padding... if the below fails, we know there are some serious shenanigans
    // going on in LLVM.
    assert(sizePerElement * 2 == LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(hostElementRefLT, 2)));
    auto elementsSizeLE = LLVMBuildMul(builder, constI64LE(globalState, sizePerElement), lenIntLE, "elementsSize");

    return LLVMBuildAdd(builder, headerBytesLE, elementsSizeLE, "sum");
  } else if (auto hostKsaMT = dynamic_cast<KnownSizeArrayT*>(referend)) {
    auto headerBytesLE =
        constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, structs.getKnownSizeArrayStruct(hostKsaMT)));

    auto valeReferendMT = valeReferendByHostReferend.find(hostKsaMT)->second;
    auto valeKsaMT = dynamic_cast<KnownSizeArrayT*>(valeReferendMT);
    assert(valeKsaMT);
    auto valeElementRefMT = globalState->program->getKnownSizeArray(valeKsaMT->name)->rawArray->elementType;
    auto hostElementRefMT = linearizeReference(valeElementRefMT);
    auto hostElementRefLT = translateType(hostElementRefMT);

    auto sizePerElement = LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(hostElementRefLT, 1));
    // The above line tries to include padding... if the below fails, we know there are some serious shenanigans
    // going on in LLVM.
    assert(sizePerElement * 2 == LLVMABISizeOfType(globalState->dataLayout, LLVMArrayType(hostElementRefLT, 2)));
    auto elementsSizeLE = LLVMBuildMul(builder, constI64LE(globalState, sizePerElement), lenIntLE, "elementsSize");

    return LLVMBuildAdd(builder, headerBytesLE, elementsSizeLE, "sum");
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


InterfaceMethod* Linear::getSerializeInterfaceMethod(Referend* valeReferend) {
  return globalState->metalCache->getInterfaceMethod(
      getSerializePrototype(valeReferend), 1);
}

Ref Linear::callSerialize(
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Referend* valeReferend,
    Ref regionInstanceRef,
    Ref objectRef,
    Ref dryRunBoolRef) {
  auto prototype = getSerializePrototype(valeReferend);
  if (dynamic_cast<InterfaceReferend*>(valeReferend)) {
    return buildInterfaceCall(globalState, functionState, builder, prototype, {regionInstanceRef, objectRef, dryRunBoolRef}, 1);
  } else {
    return buildCall(globalState, functionState, builder, prototype, {regionInstanceRef, objectRef, dryRunBoolRef});
  }
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
  buildFlare(FL(), globalState, functionState, builder, "subtracted: ", destinationOffsetLE);
  destinationOffsetLE = hexRoundDown(globalState, builder, destinationOffsetLE);
  buildFlare(FL(), globalState, functionState, builder, "rounded: ", destinationOffsetLE);
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

Prototype* Linear::getSerializePrototype(Referend* valeReferend) {
  auto boolMT = globalState->metalCache->boolRef;
  auto sourceStructRefMT =
      globalState->metalCache->getReference(
          Ownership::SHARE, Location::YONDER, valeReferend);
  auto hostRefMT = linearizeReference(sourceStructRefMT);
  return globalState->metalCache->getPrototype(
      globalState->serializeName, hostRefMT,
      {regionRefMT, sourceStructRefMT, boolMT});
}

Prototype* Linear::getSerializeThunkPrototype(StructReferend* structReferend, InterfaceReferend* interfaceReferend) {
  auto boolMT = globalState->metalCache->boolRef;
  auto valeStructRefMT =
      globalState->metalCache->getReference(
          Ownership::SHARE, Location::YONDER, structReferend);
  auto valeInterfaceRefMT =
      globalState->metalCache->getReference(
          Ownership::SHARE, Location::YONDER, interfaceReferend);
  auto hostRefMT = linearizeReference(valeInterfaceRefMT);
  return globalState->metalCache->getPrototype(
      globalState->serializeThunkName, hostRefMT,
      {regionRefMT, valeStructRefMT, boolMT});
}

void Linear::declareConcreteSerializeFunction(Referend* valeReferend) {
  auto prototype = getSerializePrototype(valeReferend);
  auto nameL = globalState->serializeName->name + "__" + globalState->getReferendName(valeReferend)->name;
  declareExtraFunction(globalState, prototype, nameL);
}

void Linear::defineConcreteSerializeFunction(Referend* valeReferend) {
  auto intMT = globalState->metalCache->intRef;
  auto boolMT = globalState->metalCache->boolRef;

  auto prototype = getSerializePrototype(valeReferend);

  auto serializeMemberOrElement =
      [this](
          FunctionState* functionState,
          LLVMBuilderRef builder,
          Reference* sourceMemberRefMT,
          Ref regionInstanceRef,
          Ref sourceMemberRef,
          Ref dryRunBoolRef) {
        auto targetMemberRefMT = linearizeReference(sourceMemberRefMT);
        auto sourceMemberLE =
            globalState->getRegion(sourceMemberRefMT)->checkValidReference(
                FL(), functionState, builder, sourceMemberRefMT, sourceMemberRef);
        if (sourceMemberRefMT == globalState->metalCache->intRef) {
          buildFlare(FL(), globalState, functionState, builder, "serializing int: ", sourceMemberLE);
          return wrap(globalState->getRegion(targetMemberRefMT), targetMemberRefMT, sourceMemberLE);
        } else if (sourceMemberRefMT == globalState->metalCache->boolRef) {
          auto resultLE = LLVMBuildZExt(builder, sourceMemberLE, LLVMInt8TypeInContext(globalState->context), "boolAsI8");
          return wrap(globalState->getRegion(targetMemberRefMT), targetMemberRefMT, resultLE);
        } else if (sourceMemberRefMT == globalState->metalCache->floatRef) {
          return wrap(globalState->getRegion(targetMemberRefMT), targetMemberRefMT, sourceMemberLE);
        } else if (
            dynamic_cast<Str*>(sourceMemberRefMT->referend) ||
            dynamic_cast<StructReferend*>(sourceMemberRefMT->referend) ||
            dynamic_cast<InterfaceReferend*>(sourceMemberRefMT->referend) ||
            dynamic_cast<KnownSizeArrayT*>(sourceMemberRefMT->referend) ||
            dynamic_cast<UnknownSizeArrayT*>(sourceMemberRefMT->referend)) {
          auto destinationMemberRef =
              callSerialize(
                  functionState, builder, sourceMemberRefMT->referend, regionInstanceRef, sourceMemberRef, dryRunBoolRef);
          return destinationMemberRef;
        } else assert(false);
      };

  defineFunctionBody(
      globalState, prototype,
      [&](FunctionState* functionState, LLVMBuilderRef builder) -> void {
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

          std::vector<Ref> memberRefs;

          for (int i = 0; i < valeStructDefM->members.size(); i++) {
            auto valeMemberM = valeStructDefM->members[i];
            auto sourceMemberRefMT = valeMemberM->type;
            auto sourceMemberRef =
                globalState->getRegion(valeObjectRefMT)->loadMember(
                    functionState, builder, valeObjectRefMT, valeObjectRef, true,
                    i, valeMemberM->type, valeMemberM->type, valeMemberM->name);
            memberRefs.push_back(
                serializeMemberOrElement(
                    functionState, builder, sourceMemberRefMT, regionInstanceRef, sourceMemberRef, dryRunBoolRef));
          }

          auto resultRef = innerAllocate(regionInstanceRef, FL(), functionState, builder, hostObjectRefMT, memberRefs, dryRunBoolRef);

//          // Remember, we're subtracting each size from a very large number, so its easier to round down
//          // to the next multiple of 16.
//          totalSizeIntLE = hexRoundDown(globalState, builder, totalSizeIntLE);

          auto resultRefLE = checkValidReference(FL(), functionState, builder, hostObjectRefMT, resultRef);

          LLVMBuildRet(builder, resultRefLE);
        } else if (dynamic_cast<Str*>(valeObjectRefMT->referend)) {
          auto lengthLE = globalState->getRegion(valeObjectRefMT)->getStringLen(functionState, builder, valeObjectRef);
          auto sourceBytesPtrLE = globalState->getRegion(valeObjectRefMT)->getStringBytesPtr(functionState, builder, valeObjectRef);

          auto strRef = innerMallocStr(regionInstanceRef, functionState, builder, lengthLE, sourceBytesPtrLE, dryRunBoolRef);

          buildFlare(FL(), globalState, functionState, builder, "Returning from serialize function!");

          LLVMBuildRet(builder, checkValidReference(FL(), functionState, builder, linearStrRefMT, strRef));
        } else if (auto valeUsaMT = dynamic_cast<UnknownSizeArrayT*>(valeObjectRefMT->referend)) {

          buildFlare(FL(), globalState, functionState, builder, "In USA serialize!");

          auto hostReferendMT = hostReferendByValeReferend.find(valeUsaMT)->second;
          auto hostUsaMT = dynamic_cast<UnknownSizeArrayT*>(hostReferendMT);
          assert(hostUsaMT);
          auto hostUsaRefMT = globalState->metalCache->getReference(Ownership::SHARE, Location::YONDER, hostReferendMT);

          auto lengthRef = globalState->getRegion(valeObjectRefMT)->getUnknownSizeArrayLength(functionState, builder, valeObjectRefMT, valeObjectRef, true);

          buildFlare(FL(), globalState, functionState, builder);

          auto hostUsaRef =
              innerConstructUnknownSizeArray(
                  regionInstanceRef,
                  functionState, builder, hostUsaRefMT, hostUsaMT, lengthRef, "serializedusa", dryRunBoolRef);
          auto valeMemberRefMT = globalState->program->getUnknownSizeArray(valeUsaMT->name)->rawArray->elementType;

          buildFlare(FL(), globalState, functionState, builder);

          auto dryRunBoolLE = globalState->getRegion(boolMT)->checkValidReference(FL(), functionState, builder, boolMT, dryRunBoolRef);
          buildIf(
              globalState, functionState, builder, LLVMBuildNot(builder, dryRunBoolLE, "notDryRun"),
              [this, functionState, valeMemberRefMT, regionInstanceRef, hostObjectRefMT, hostUsaRef, lengthRef, dryRunBoolRef, valeObjectRefMT, hostUsaMT, valeUsaMT, valeObjectRef, serializeMemberOrElement](
                  LLVMBuilderRef thenBuilder) mutable {
                intRangeLoopReverse(
                    globalState, functionState, thenBuilder, lengthRef,
                    [this, functionState, hostObjectRefMT, hostUsaRef, valeObjectRefMT, hostUsaMT, valeUsaMT, valeObjectRef, valeMemberRefMT, regionInstanceRef, serializeMemberOrElement, dryRunBoolRef](
                        Ref indexRef, LLVMBuilderRef bodyBuilder){
                      buildFlare(FL(), globalState, functionState, bodyBuilder, "In serialize iteration!");

                      auto sourceMemberRef =
                          globalState->getRegion(valeObjectRefMT)
                              ->loadElementFromUSA(functionState, bodyBuilder, valeObjectRefMT, valeUsaMT, valeObjectRef, true, indexRef)
                          .move();
                      buildFlare(FL(), globalState, functionState, bodyBuilder);
                      auto hostElementRef =
                          serializeMemberOrElement(
                              functionState, bodyBuilder, valeMemberRefMT, regionInstanceRef, sourceMemberRef, dryRunBoolRef);
                      buildFlare(FL(), globalState, functionState, bodyBuilder);
                      initializeElementInUSA(
                          functionState, bodyBuilder, hostObjectRefMT, hostUsaMT, hostUsaRef, true, indexRef, hostElementRef);
                      buildFlare(FL(), globalState, functionState, bodyBuilder, "done iter");
                    });
              });

          buildFlare(FL(), globalState, functionState, builder, "Returning from serialize function!");

          LLVMBuildRet(builder, checkValidReference(FL(), functionState, builder, hostUsaRefMT, hostUsaRef));
        } else if (auto valeKsaMT = dynamic_cast<KnownSizeArrayT*>(valeObjectRefMT->referend)) {
          auto hostReferendMT = hostReferendByValeReferend.find(valeKsaMT)->second;
          auto hostKsaMT = dynamic_cast<KnownSizeArrayT*>(hostReferendMT);
          assert(hostKsaMT);
          auto hostKsaRefMT = globalState->metalCache->getReference(Ownership::SHARE, Location::YONDER, hostReferendMT);

          auto hostKsaRef =
              innerConstructKnownSizeArray(
                  regionInstanceRef,
                  functionState, builder, hostKsaRefMT, hostKsaMT, dryRunBoolRef);
          auto valeKsaDefM = globalState->program->getKnownSizeArray(valeKsaMT->name);
          int length = valeKsaDefM->size;
          auto valeMemberRefMT = valeKsaDefM->rawArray->elementType;

          auto dryRunBoolLE = globalState->getRegion(boolMT)->checkValidReference(FL(), functionState, builder, boolMT, dryRunBoolRef);
          buildIf(
              globalState, functionState, builder, LLVMBuildNot(builder, dryRunBoolLE, "notDryRun"),
              [this, functionState, valeMemberRefMT, length, regionInstanceRef, hostObjectRefMT, hostKsaRef, dryRunBoolRef, valeObjectRefMT, hostKsaMT, valeKsaMT, valeObjectRef, serializeMemberOrElement](
                  LLVMBuilderRef thenBuilder) mutable {
                intRangeLoopReverse(
                    globalState, functionState, thenBuilder, globalState->constI64(length),
                    [this, functionState, hostObjectRefMT, hostKsaRef, valeObjectRefMT, hostKsaMT, valeKsaMT, valeObjectRef, valeMemberRefMT, regionInstanceRef, serializeMemberOrElement, dryRunBoolRef](
                        Ref indexRef, LLVMBuilderRef bodyBuilder){

                      auto sourceMemberRef =
                          globalState->getRegion(valeObjectRefMT)
                              ->loadElementFromKSA(functionState, bodyBuilder, valeObjectRefMT, valeKsaMT, valeObjectRef, true, indexRef)
                              .move();
                      auto hostElementRef =
                          serializeMemberOrElement(
                              functionState, bodyBuilder, valeMemberRefMT, regionInstanceRef, sourceMemberRef, dryRunBoolRef);
                      initializeElementInKSA(
                          functionState, bodyBuilder, hostObjectRefMT, hostKsaMT, hostKsaRef, true, indexRef, hostElementRef);
                    });
              });


          LLVMBuildRet(builder, checkValidReference(FL(), functionState, builder, hostKsaRefMT, hostKsaRef));
        } else assert(false);
      });
}

Reference* Linear::linearizeReference(Reference* immRcRefMT) {
  assert(globalState->getRegion(immRcRefMT) == globalState->rcImm);
  auto hostReferend = hostReferendByValeReferend.find(immRcRefMT->referend)->second;
  return globalState->metalCache->getReference(
      immRcRefMT->ownership, immRcRefMT->location, hostReferend);
}

Reference* Linear::unlinearizeReference(Reference* hostRefMT) {
  assert(globalState->getRegion(hostRefMT) == globalState->linearRegion);
  auto valeReferend = valeReferendByHostReferend.find(hostRefMT->referend)->second;
  return globalState->metalCache->getReference(
      hostRefMT->ownership, hostRefMT->location, valeReferend);
}

void Linear::fillLinearInnerStruct(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    StructDefinition* structM,
    std::vector<Ref> membersLE,
    LLVMValueRef innerStructPtrLE) {
  for (int i = 0; i < membersLE.size(); i++) {
    auto memberRef = membersLE[i];
    auto hostMemberType = linearizeReference(structM->members[i]->type);
    auto memberName = structM->members[i]->name;
    auto ptrLE =
        LLVMBuildStructGEP(builder, innerStructPtrLE, i, memberName.c_str());
    auto memberLE =
        globalState->getRegion(hostMemberType)
            ->checkValidReference(FL(), functionState, builder, hostMemberType, memberRef);
    LLVMBuildStore(builder, memberLE, ptrLE);
  }
}

void Linear::initializeElementInUSA(
    FunctionState *functionState,
    LLVMBuilderRef builder,
    Reference *hostUsaRefMT,
    UnknownSizeArrayT *hostUsaMT,
    Ref hostUsaRef,
    bool arrayRefKnownLive,
    Ref indexRef,
    Ref elementRef) {
  buildFlare(FL(), globalState, functionState, builder);
  assert(hostUsaRefMT->referend == hostUsaMT);
  assert(globalState->getRegion(hostUsaMT) == this);

  buildFlare(FL(), globalState, functionState, builder);
  auto valeReferendMT = valeReferendByHostReferend.find(hostUsaMT)->second;
  auto valeUsaMT = dynamic_cast<UnknownSizeArrayT*>(valeReferendMT);
  assert(valeUsaMT);
  auto valeElementRefMT = globalState->program->getUnknownSizeArray(valeUsaMT->name)->rawArray->elementType;
  auto hostElementRefMT = linearizeReference(valeElementRefMT);
  auto elementRefLE = globalState->getRegion(hostElementRefMT)->checkValidReference(FL(), functionState, builder, hostElementRefMT, elementRef);

  buildFlare(FL(), globalState, functionState, builder);
  auto intMT = globalState->metalCache->intRef;

  auto indexLE = globalState->getRegion(intMT)->checkValidReference(FL(), functionState, builder, intMT, indexRef);

  buildFlare(FL(), globalState, functionState, builder);
  auto usaPtrLE = checkValidReference(FL(), functionState, builder, hostUsaRefMT, hostUsaRef);
  auto hostUsaElementsPtrLE = structs.getUnknownSizeArrayElementsPtr(functionState, builder, usaPtrLE);

  buildFlare(FL(), globalState, functionState, builder, "Storing in: ", indexLE);
//  LLVMBuildStore(
//      builder,
//      elementRefLE,
//      LLVMBuildGEP(
//          builder, hostUsaElementsPtrLE, &indexLE, 1, "indexPtr"));
  buildFlare(FL(), globalState, functionState, builder);
  storeInnerArrayMember(globalState, builder, hostUsaElementsPtrLE, indexLE, elementRefLE);
  buildFlare(FL(), globalState, functionState, builder);
}

Ref Linear::deinitializeElementFromUSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* usaRefMT,
    UnknownSizeArrayT* usaMT,
    Ref arrayRef,
    bool arrayRefKnownLive,
    Ref indexRef) {
  assert(false);
}

void Linear::initializeElementInKSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* hostKsaRefMT,
    KnownSizeArrayT* hostKsaMT,
    Ref hostKsaRef,
    bool arrayRefKnownLive,
    Ref indexRef,
    Ref elementRef) {

  assert(hostKsaRefMT->referend == hostKsaMT);
  assert(globalState->getRegion(hostKsaMT) == this);

  auto valeReferendMT = valeReferendByHostReferend.find(hostKsaMT)->second;
  auto valeKsaMT = dynamic_cast<KnownSizeArrayT*>(valeReferendMT);
  assert(valeKsaMT);
  auto valeElementRefMT = globalState->program->getKnownSizeArray(valeKsaMT->name)->rawArray->elementType;
  auto hostElementRefMT = linearizeReference(valeElementRefMT);
  auto elementRefLE = globalState->getRegion(hostElementRefMT)->checkValidReference(FL(), functionState, builder, hostElementRefMT, elementRef);

  auto intMT = globalState->metalCache->intRef;

  auto indexLE = globalState->getRegion(intMT)->checkValidReference(FL(), functionState, builder, intMT, indexRef);

  auto usaPtrLE = checkValidReference(FL(), functionState, builder, hostKsaRefMT, hostKsaRef);
  auto hostKsaElementsPtrLE = structs.getKnownSizeArrayElementsPtr(functionState, builder, usaPtrLE);

  storeInnerArrayMember(globalState, builder, hostKsaElementsPtrLE, indexLE, elementRefLE);
}

Ref Linear::deinitializeElementFromKSA(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* ksaRefMT,
    KnownSizeArrayT* ksaMT,
    Ref arrayRef,
    bool arrayRefKnownLive,
    Ref indexRef) {
  assert(false);
}

Weakability Linear::getReferendWeakability(Referend* referend) {
  return Weakability::NON_WEAKABLE;
}
