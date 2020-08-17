#ifndef REGION_ASSIST_H_
#define REGION_ASSIST_H_

#include <regions/shared/stringmixin.h>
#include <regions/shared/immmixin.h>
#include "iregion.h"

class AssistRegion : public IRegion {
public:
  AssistRegion();

  LLVMValueRef allocate(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* desiredReference,
      const std::vector<LLVMValueRef>& membersLE) override;

  LLVMValueRef alias(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* sourceRef,
      Reference* targetRef,
      LLVMValueRef expr) override;
  void dealias(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* sourceRef,
      LLVMValueRef expr) override;

  LLVMValueRef loadMember(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* structRefM,
      LLVMValueRef structExpr,
      Mutability mutability,
      Reference* memberType,
      int memberIndex,
      const std::string& memberName) override;

  std::vector<LLVMValueRef> destructure(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* structType,
      LLVMValueRef structLE) override;

  LLVMValueRef storeMember(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* structRefM,
      LLVMValueRef structExpr,
      Mutability mutability,
      Reference* memberType,
      int memberIndex,
      const std::string& memberName,
      LLVMValueRef sourceLE) override;

  void destroyArray(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* arrayType,
      LLVMValueRef arrayWrapperLE) override;


  LLVMValueRef getConcreteRefFromInterfaceRef(LLVMBuilderRef builder, LLVMValueRef refLE) override;


  LLVMValueRef constructKnownSizeArray(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* structTypeM,
      KnownSizeArrayT* referendM,
      const std::vector<LLVMValueRef>& membersLE) override;
  LLVMValueRef getKnownSizeArrayElementsPtr(
      LLVMBuilderRef builder, LLVMValueRef knownSizeArrayWrapperPtrLE) override;

  LLVMValueRef constructUnknownSizeArray(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMTypeRef usaWrapperPtrLT,
      LLVMTypeRef usaElementLT,
      LLVMValueRef sizeLE,
      const std::string& typeName) override;
  LLVMValueRef getUnknownSizeArrayElementsPtr(
      LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) override;
  LLVMValueRef getUnknownSizeArrayLength(
      LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) override;

  LLVMValueRef loadElement(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* structRefM,
      Reference* elementRefM,
      LLVMValueRef sizeLE,
      LLVMValueRef arrayPtrLE,
      Mutability mutability,
      LLVMValueRef indexLE) override;

  LLVMTypeRef getKnownSizeArrayRefType(
      GlobalState* globalState,
      Reference* referenceM,
      KnownSizeArrayT* knownSizeArrayMT) override;

  LLVMTypeRef getUnknownSizeArrayRefType(
      GlobalState* globalState,
      Reference* referenceM,
      UnknownSizeArrayT* unknownSizeArrayMT) override;

  LLVMValueRef storeElement(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* arrayRefM,
      Reference* elementRefM,
      LLVMValueRef sizeLE,
      LLVMValueRef arrayPtrLE,
      Mutability mutability,
      LLVMValueRef indexLE,
      LLVMValueRef sourceLE) override;

  void checkValidReference(
      AreaAndFileAndLine checkerAFL,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef refLE) override;

  LLVMValueRef upcast2(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,

      Reference* sourceStructTypeM,
      StructReferend* sourceStructReferendM,
      LLVMValueRef sourceStructLE,

      Reference* targetInterfaceTypeM,
      InterfaceReferend* targetInterfaceReferendM) override;


  LLVMValueRef lockWeak(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* constraintRefTypeM,
      bool thenResultIsNever,
      bool elseResultIsNever,
      LLVMTypeRef resultOptTypeL,
      LLVMValueRef sourceWeakRefLE,
      std::function<LLVMValueRef(LLVMBuilderRef, LLVMValueRef)> buildThen,
      std::function<LLVMValueRef(LLVMBuilderRef)> buildElse) override;

  void translateStruct(
      GlobalState* globalState,
      StructDefinition* structM) override;

  void declareStruct(
      GlobalState* globalState,
      StructDefinition* structM) override;

  LLVMTypeRef getStructRefType(
      GlobalState* globalState,
      Reference* refM,
      StructReferend* structReferendM) override;

  void translateInterface(
      GlobalState* globalState,
      InterfaceDefinition* interfaceM) override;

  LLVMValueRef constructString(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef lengthLE) override;
  LLVMValueRef getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE) override;
  LLVMValueRef getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE) override;

  LLVMTypeRef getStringRefType() const override;


  LLVMTypeRef translateType(GlobalState* globalState, Reference* referenceM) override;

  void declareEdge(
      GlobalState* globalState,
      Edge* edge) override;

  void translateEdge(
      GlobalState* globalState,
      Edge* edge) override;


  void declareInterface(
      GlobalState* globalState,
      InterfaceDefinition* interfaceM) override;

private:
  LLVMValueRef getStringControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef stringRefLE);

  LLVMValueRef loadInnerArrayMember(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef elemsPtrLE,
      Reference* elementRefM,
      LLVMValueRef indexLE);

  LLVMValueRef storeInnerArrayMember(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef elemsPtrLE,
      Reference* elementRefM,
      LLVMValueRef indexLE,
      LLVMValueRef sourceLE);

  void fillInnerStruct(
      LLVMBuilderRef builder,
      StructDefinition* structM,
      std::vector<LLVMValueRef> membersLE,
      LLVMValueRef innerStructPtrLE);

  LLVMValueRef constructCountedStruct(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMTypeRef structL,
      Reference* structTypeM,
      StructDefinition* structM,
      std::vector<LLVMValueRef> membersLE);

  LLVMValueRef constructInnerStruct(
      LLVMBuilderRef builder,
      StructDefinition* structM,
      LLVMTypeRef valStructL,
      const std::vector<LLVMValueRef>& membersLE);

// A concrete is a struct, known size array, unknown size array, or Str.
  LLVMValueRef getConcreteControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef concretePtrLE);

  LLVMValueRef getInterfaceControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef interfaceRefLE);


  LLVMTypeRef getKnownSizeArrayType(
      GlobalState* globalState,
      KnownSizeArrayT* knownSizeArrayMT);

  // See CRCISFAORC for why we don't take in a mutability.
// Strong means owning or borrow or shared; things that control the lifetime.
  LLVMValueRef getStrongRcPtrFromControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtr);
// See CRCISFAORC for why we don't take in a mutability.
  LLVMValueRef getWrciFromControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtr,
      int controlBlockWrciMemberIndex);

  LLVMValueRef getObjIdFromControlBlockPtr(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtr);

// Strong means owning or borrow or shared; things that control the lifetime.
  LLVMValueRef getStrongRcFromControlBlockPtr(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtrLE);

  LLVMValueRef getControlBlockPtr(
      LLVMBuilderRef builder,
      // This will be a pointer if a mutable struct, or a fat ref if an interface.
      LLVMValueRef referenceLE,
      Reference* refM);

  void flareAdjustStrongRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef controlBlockPtr,
      LLVMValueRef oldAmount,
      LLVMValueRef newAmount);

// Returns the new RC
  LLVMValueRef adjustStrongRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef exprLE,
      Reference* refM,
      int amount);

  LLVMTypeRef makeInnerUnknownSizeArrayLT(GlobalState* globalState, UnknownSizeArrayT* unknownSizeArrayMT);

  LLVMTypeRef translateKnownSizeArrayToWrapperStruct(
      GlobalState* globalState,
      KnownSizeArrayT* knownSizeArrayMT);
  LLVMTypeRef translateInterfaceMethodToFunctionType(
      GlobalState* globalState,
      InterfaceMethod* method);
  LLVMValueRef strongRcIsZero(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef exprLE,
      Reference* refM);

  LLVMValueRef getTypeNameStrPtrFromControlBlockPtr(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtr);

  LLVMValueRef getIsAliveFromWeakRef(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef weakRefLE);

  LLVMValueRef fillControlBlock(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtrLE,
      bool weakable,
      const std::string& typeName);

  LLVMValueRef getConstraintRefFromWeakRef(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef weakRefLE,
      Reference* constraintRefM);

  LLVMValueRef castOwnership(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      Reference* sourceType,
      Ownership targetOwnership,
      LLVMValueRef sourceRefLE);

  void nonOwningDecrementStrongRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef expr);
  void sharingDecrementStrongRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* sourceRef,
      LLVMValueRef expr);
  void incrementWeakRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef expr);
  void decrementWeakRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef expr);
//  LLVMValueRef constructKnownSizeArrayCountedStruct(
//      GlobalState* globalState,
//      FunctionState* functionState,
//      BlockState* blockState,
//      LLVMBuilderRef builder,
//      Reference* generatorType,
//      LLVMValueRef generatorLE,
//      LLVMTypeRef usaWrapperPtrLT,
//      LLVMTypeRef usaElementLT,
//      LLVMValueRef sizeLE,
//      const std::string& typeName);

  LLVMTypeRef makeInnerKnownSizeArrayLT(GlobalState* globalState, KnownSizeArrayT* knownSizeArrayMT);

  LLVMTypeRef getControlBlockStructForStruct(StructDefinition* structM);
  LLVMTypeRef getControlBlockStructForInterface(InterfaceDefinition* interfaceM);
  LLVMTypeRef getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT);

//  int controlBlockTypeStrIndex;
//  int controlBlockObjIdIndex;
//  int controlBlockRcMemberIndex;
//  int controlBlockWrciMemberIndex;
  LLVMTypeRef nonWeakableControlBlockStructL;
  LLVMTypeRef weakableControlBlockStructL;




  // These don't have a ref count.
  // They're used directly for inl imm references, and
  // also used inside the below countedStructs.
  std::unordered_map<std::string, LLVMTypeRef> innerStructs;
  // These contain a ref count and the above val struct. Yon references
  // point to these.
  std::unordered_map<std::string, LLVMTypeRef> countedStructs;
  // These contain a pointer to the interface table struct below and a void*
  // to the underlying struct.
  std::unordered_map<std::string, LLVMTypeRef> interfaceRefStructs;
  // These contain a bunch of function pointer fields.
  std::unordered_map<std::string, LLVMTypeRef> interfaceTableStructs;
  // These contain a pointer to the weak ref count int, and a pointer to the underlying struct.
  std::unordered_map<std::string, LLVMTypeRef> structWeakRefStructs;
  // These contain a pointer to the weak ref count int, and then a regular interface ref struct.
  std::unordered_map<std::string, LLVMTypeRef> interfaceWeakRefStructs;

  std::unordered_map<Edge*, LLVMValueRef> interfaceTablePtrs;

  // These contain a ref count and an array type. Yon references
  // point to these.
  std::unordered_map<Name*, LLVMTypeRef> knownSizeArrayCountedStructs;
  std::unordered_map<Name*, LLVMTypeRef> unknownSizeArrayCountedStructs;

  LLVMTypeRef getInnerStruct(Name* name) {
    auto structIter = innerStructs.find(name->name);
    assert(structIter != innerStructs.end());
    return structIter->second;
  }
  LLVMTypeRef getCountedStruct(Name* name) {
    auto structIter = countedStructs.find(name->name);
    assert(structIter != countedStructs.end());
    return structIter->second;
  }
  LLVMTypeRef getStructWeakRefStruct(Name* name) {
    auto structIter = structWeakRefStructs.find(name->name);
    assert(structIter != structWeakRefStructs.end());
    return structIter->second;
  }
  LLVMTypeRef getInterfaceRefStruct(Name* name) {
    auto structIter = interfaceRefStructs.find(name->name);
    assert(structIter != interfaceRefStructs.end());
    return structIter->second;
  }
  LLVMTypeRef getInterfaceWeakRefStruct(Name* name) {
    auto interfaceIter = interfaceWeakRefStructs.find(name->name);
    assert(interfaceIter != interfaceWeakRefStructs.end());
    return interfaceIter->second;
  }
  LLVMTypeRef getInterfaceTableStruct(Name* name) {
    auto structIter = interfaceTableStructs.find(name->name);
    assert(structIter != interfaceTableStructs.end());
    return structIter->second;
  }

  LLVMValueRef getInterfaceTablePtr(Edge* edge) {
    auto iter = interfaceTablePtrs.find(edge);
    assert(iter != interfaceTablePtrs.end());
    return iter->second;
  }




  ImmMixin immMixin;
};

#endif
