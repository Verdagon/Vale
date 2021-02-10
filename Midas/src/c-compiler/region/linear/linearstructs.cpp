
#include <region/common/common.h>
#include <function/expressions/shared/shared.h>
#include <function/expressions/shared/string.h>
#include "linearstructs.h"


LinearStructs::LinearStructs(GlobalState* globalState_)
  : globalState(globalState_) {

//  auto voidLT = LLVMVoidTypeInContext(globalState->context);
  auto int8LT = LLVMInt8TypeInContext(globalState->context);
  auto int8PtrLT = LLVMPointerType(int8LT, 0);

  stringStructLT = LLVMStructCreateNamed(globalState->context, "ValeStr");
  std::vector<LLVMTypeRef> memberTypesL;
  memberTypesL.push_back(LLVMInt64TypeInContext(globalState->context));
  memberTypesL.push_back(LLVMArrayType(LLVMInt8TypeInContext(globalState->context), 0));
  LLVMStructSetBody(stringStructLT, memberTypesL.data(), memberTypesL.size(), false);
}

LLVMTypeRef LinearStructs::getStructStruct(StructReferend* structReferend) {
  auto structIter = structStructsL.find(structReferend);
  assert(structIter != structStructsL.end());
  return structIter->second;
}

LLVMTypeRef LinearStructs::getKnownSizeArrayStruct(KnownSizeArrayT* ksaMT) {
  auto structIter = knownSizeArrayStructsL.find(ksaMT);
  assert(structIter != knownSizeArrayStructsL.end());
  return structIter->second;
}

LLVMTypeRef LinearStructs::getUnknownSizeArrayStruct(UnknownSizeArrayT* usaMT) {
  auto structIter = unknownSizeArrayStructsL.find(usaMT);
  assert(structIter != unknownSizeArrayStructsL.end());
  return structIter->second;
}

LLVMTypeRef LinearStructs::getInterfaceRefStruct(InterfaceReferend* interfaceReferend) {
  auto structIter = interfaceRefStructsL.find(interfaceReferend);
  assert(structIter != interfaceRefStructsL.end());
  return structIter->second;
}

LLVMTypeRef LinearStructs::getStringStruct() {
  return stringStructLT;
}

void LinearStructs::declareStruct(StructDefinition* structM) {
  auto structL =
      LLVMStructCreateNamed(
          globalState->context, structM->name->name.c_str());
  assert(structStructsL.count(structM->referend) == 0);
  structStructsL.emplace(structM->referend, structL);
}

void LinearStructs::declareEdge(
    Edge* edge) {
  // There aren't edges per se, just tag numbers. That's all we have to do here.

  // Creates one if it doesnt already exist.
  auto* os = &orderedStructsByInterface[edge->interfaceName];

  assert(std::count(os->begin(), os->end(), edge->structName) == 0);
  os->push_back(edge->structName);
}

void LinearStructs::declareInterface(InterfaceDefinition* interface) {
  auto interfaceRefStructL =
      LLVMStructCreateNamed(
          globalState->context, interface->name->name.c_str());
  assert(interfaceRefStructsL.count(interface->referend) == 0);
  interfaceRefStructsL.emplace(interface->referend, interfaceRefStructL);

  // No need to make interface table structs, there are no itables for Linear.
}

void LinearStructs::declareKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT) {
  auto countedStruct = LLVMStructCreateNamed(globalState->context, knownSizeArrayMT->name->name.c_str());
  knownSizeArrayStructsL.emplace(knownSizeArrayMT, countedStruct);
}

void LinearStructs::declareUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT) {
  auto countedStruct = LLVMStructCreateNamed(globalState->context, (unknownSizeArrayMT->name->name + "rc").c_str());
  unknownSizeArrayStructsL.emplace(unknownSizeArrayMT, countedStruct);
}

void LinearStructs::translateStruct(
    StructDefinition* struuct,
    std::vector<LLVMTypeRef> membersLT) {
  LLVMTypeRef structL = getStructStruct(struuct->referend);
  LLVMStructSetBody(structL, membersLT.data(), membersLT.size(), false);
}

void LinearStructs::translateInterface(InterfaceDefinition* interface) {
  LLVMTypeRef refStructL = getInterfaceRefStruct(interface->referend);

  std::vector<LLVMTypeRef> memberTypesL = {
      LLVMInt64TypeInContext(globalState->context),
      LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0),
  };
  LLVMStructSetBody(refStructL, memberTypesL.data(), memberTypesL.size(), false);
}

void LinearStructs::translateEdge(
    Edge* edge,
    std::vector<LLVMTypeRef> interfaceFunctionsLT,
    std::vector<LLVMValueRef> functions) {
}

void LinearStructs::translateUnknownSizeArray(
    UnknownSizeArrayT* unknownSizeArrayMT,
    LLVMTypeRef elementLT) {
  auto unknownSizeArrayStruct = getUnknownSizeArrayStruct(unknownSizeArrayMT);
  auto innerArrayLT = LLVMArrayType(elementLT, 0);

  std::vector<LLVMTypeRef> elementsL;
  elementsL.push_back(LLVMInt64TypeInContext(globalState->context));
  elementsL.push_back(innerArrayLT);
  LLVMStructSetBody(unknownSizeArrayStruct, elementsL.data(), elementsL.size(), false);
}

void LinearStructs::translateKnownSizeArray(
    KnownSizeArrayT* knownSizeArrayMT,
    LLVMTypeRef elementLT) {
  auto knownSizeArrayStruct = getKnownSizeArrayStruct(knownSizeArrayMT);
  auto innerArrayLT = LLVMArrayType(elementLT, knownSizeArrayMT->size);

  std::vector<LLVMTypeRef> elementsL;
  elementsL.push_back(innerArrayLT);
  LLVMStructSetBody(knownSizeArrayStruct, elementsL.data(), elementsL.size(), false);
}


InterfaceFatPtrLE LinearStructs::makeInterfaceFatPtr(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* referenceM_,
    LLVMValueRef ptrLE) {
  auto interfaceReferendM = dynamic_cast<InterfaceReferend*>(referenceM_->referend);
  assert(interfaceReferendM);
  assert(LLVMTypeOf(ptrLE) == getInterfaceRefStruct(interfaceReferendM));

  auto interfaceFatPtrLE = InterfaceFatPtrLE(referenceM_, ptrLE);

  // This is actually a tag number, not a table pointer
  auto tagLE = getTablePtrFromInterfaceRef(builder, interfaceFatPtrLE);

  return interfaceFatPtrLE;
}

LLVMValueRef LinearStructs::getStringBytesPtr(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef ptrLE) {
  auto charsArrayPtrLE = LLVMBuildStructGEP(builder, ptrLE, 1, "charsPtr");

  std::vector<LLVMValueRef> indices = { constI64LE(globalState, 0), constI64LE(globalState, 0) };
  auto firstCharPtrLE =
      LLVMBuildGEP(
          builder, charsArrayPtrLE, indices.data(), indices.size(), "elementPtr");
  assert(LLVMTypeOf(firstCharPtrLE) == LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0));
  return firstCharPtrLE;
}

LLVMValueRef LinearStructs::getStringLen(FunctionState* functionState, LLVMBuilderRef builder, LLVMValueRef ptrLE) {
  auto lenPtrLE = LLVMBuildStructGEP(builder, ptrLE, 0, "lenPtr");
  return LLVMBuildLoad(builder, lenPtrLE, "len");
}
