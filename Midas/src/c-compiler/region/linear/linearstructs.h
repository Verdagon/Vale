#ifndef REGION_COMMON_LINEAR_LINEARSTRUCTS_H_
#define REGION_COMMON_LINEAR_LINEARSTRUCTS_H_

#include <llvm-c/Types.h>
#include <globalstate.h>
#include <iostream>
#include <region/common/primitives.h>
#include <function/expressions/shared/afl.h>
#include <function/function.h>
#include <region/common/defaultlayout/structs.h>

class LinearStructs {
public:
  LinearStructs(GlobalState* globalState_);

  LLVMTypeRef getStructStruct(StructReferend* structReferend);
  LLVMTypeRef getKnownSizeArrayStruct(KnownSizeArrayT* ksaMT);
  LLVMTypeRef getUnknownSizeArrayStruct(UnknownSizeArrayT* usaMT);
  LLVMTypeRef getInterfaceRefStruct(InterfaceReferend* interfaceReferend);
  LLVMTypeRef getStringStruct();

  void translateStruct(
      StructDefinition* struuct,
      std::vector<LLVMTypeRef> membersLT) ;
  void declareStruct(StructDefinition* structM);
  void declareEdge(
      Edge* edge);
  void translateEdge(
      Edge* edge,
      std::vector<LLVMTypeRef> interfaceFunctionsLT,
      std::vector<LLVMValueRef> functions);
  void declareInterface(InterfaceDefinition* interface);
  void translateInterface(InterfaceDefinition* interface);
  void declareKnownSizeArray(
      KnownSizeArrayT* knownSizeArrayMT);
  void declareUnknownSizeArray(
      UnknownSizeArrayT* unknownSizeArrayMT);
  void translateUnknownSizeArray(
      UnknownSizeArrayT* unknownSizeArrayMT,
      LLVMTypeRef elementLT);
  void translateKnownSizeArray(
      KnownSizeArrayT* knownSizeArrayMT,
      LLVMTypeRef elementLT);
  InterfaceFatPtrLE makeInterfaceFatPtr(
      AreaAndFileAndLine checkerAFL,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* referenceM_,
      LLVMValueRef ptrLE);

  LLVMValueRef getStringBytesPtr(
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef ptrLE);
  LLVMValueRef getStringLen(FunctionState* functionState, LLVMBuilderRef builder, LLVMValueRef ptrLE);
  LLVMValueRef getVoidPtrFromInterfacePtr(
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* virtualParamMT,
      InterfaceFatPtrLE virtualArgLE);
private:
  GlobalState* globalState;

  LLVMTypeRef stringStructLT;
  std::unordered_map<InterfaceReferend*, LLVMTypeRef> interfaceRefStructsL;
  std::unordered_map<StructReferend*, LLVMTypeRef> structStructsL;
  std::unordered_map<KnownSizeArrayT*, LLVMTypeRef> knownSizeArrayStructsL;
  std::unordered_map<UnknownSizeArrayT*, LLVMTypeRef> unknownSizeArrayStructsL;

  // The position in the vector is the integer that will be the tag for which actual substruct
  // is being pointed at by an interface ref.
  std::unordered_map<InterfaceReferend*, std::vector<StructReferend*>> orderedStructsByInterface;
};
#endif
