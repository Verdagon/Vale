#ifndef REGION_ASSIST_H_
#define REGION_ASSIST_H_

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
      Ownership targetOwnership,
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

  LLVMValueRef constructString(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef lengthLE) override;
  LLVMValueRef getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE) override;
  LLVMValueRef getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE) override;


  LLVMValueRef constructKnownSizeArray(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* structTypeM,
      LLVMTypeRef structLT,
      const std::vector<LLVMValueRef>& membersLE,
      const std::string& typeName) override;
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

  void translateInterface(
      GlobalState* globalState,
      InterfaceDefinition* interfaceM) override;

private:
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



  void freeConcrete(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      LLVMValueRef concretePtrLE,
      Reference* concreteRefM);

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

  LLVMValueRef getWrciFromWeakRef(
      LLVMBuilderRef builder,
      LLVMValueRef weakRefLE);

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

  LLVMValueRef assembleStructWeakRef(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      Reference* structTypeM,
      StructReferend* structReferendM,
      LLVMValueRef objPtrLE,
      int controlBlockWrciMemberIndex);

  LLVMValueRef castOwnership(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      Reference* sourceType,
      Ownership targetOwnership,
      LLVMValueRef sourceRefLE);

  void incrementStrongRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef expr);
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
  LLVMValueRef constructKnownSizeArrayCountedStruct(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* generatorType,
      LLVMValueRef generatorLE,
      LLVMTypeRef usaWrapperPtrLT,
      LLVMTypeRef usaElementLT,
      LLVMValueRef sizeLE,
      const std::string& typeName);

  LLVMTypeRef makeInnerKnownSizeArrayLT(GlobalState* globalState, KnownSizeArrayT* knownSizeArrayMT);

  LLVMTypeRef getControlBlockStructForStruct(StructDefinition* structM);
  LLVMTypeRef getControlBlockStructForInterface(InterfaceDefinition* interfaceM);
  LLVMTypeRef getControlBlockStructForKnownSizeArray(KnownSizeArrayT* arrMT);
  LLVMTypeRef getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT);

  LLVMValueRef getInnerStrPtrFromWrapperPtr(
      LLVMBuilderRef builder,
      LLVMValueRef strWrapperPtrLE);

  LLVMValueRef getLenPtrFromStrWrapperPtr(
      LLVMBuilderRef builder,
      LLVMValueRef strWrapperPtrLE);

  LLVMValueRef getLenFromStrWrapperPtr(
      LLVMBuilderRef builder,
      LLVMValueRef strWrapperPtrLE);

  LLVMValueRef buildConstantVStr(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      const std::string& contents);

  int controlBlockTypeStrIndex;
  int controlBlockObjIdIndex;
  int controlBlockRcMemberIndex;
  int controlBlockWrciMemberIndex;
  LLVMTypeRef nonWeakableControlBlockStructL;
  LLVMTypeRef weakableControlBlockStructL;
};

#endif
