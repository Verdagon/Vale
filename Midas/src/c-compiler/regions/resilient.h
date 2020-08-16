//#ifndef REGION_RESILIENT_H_
//#define REGION_RESILIENT_H_
//
//#include <function/functionstate.h>
//#include "function/expressions/shared/alias.h"
//#include "iregion.h"
//
//class ResilientRegion : public IRegion {
//public:
//  ResilientRegion();
//
//  LLVMTypeRef getControlBlockStructForStruct(StructDefinition* structM) override;
//  LLVMTypeRef getControlBlockStructForInterface(InterfaceDefinition* interfaceM) override;
//  LLVMTypeRef getControlBlockStructForKnownSizeArray(KnownSizeArrayT* arrMT) override;
//  LLVMTypeRef getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) override;
//
//  LLVMValueRef allocate(
//      AreaAndFileAndLine from,
//      GlobalState* globalState,
//      FunctionState* functionState,
//      LLVMBuilderRef builder,
//      Reference* desiredReference,
//      const std::vector<LLVMValueRef>& membersLE) override;
//
//  LLVMValueRef alias(
//      AreaAndFileAndLine from,
//      GlobalState* globalState,
//      FunctionState* functionState,
//      LLVMBuilderRef builder,
//      Reference* sourceRef,
//      Ownership targetOwnership,
//      LLVMValueRef expr) override;
//
//  void dealias(
//      AreaAndFileAndLine from,
//      GlobalState* globalState,
//      FunctionState* functionState,
//      BlockState* blockState,
//      LLVMBuilderRef builder,
//      Reference* sourceRef,
//      LLVMValueRef expr) override;
//
//  LLVMValueRef loadMember(
//      AreaAndFileAndLine from,
//      GlobalState* globalState,
//      FunctionState* functionState,
//      LLVMBuilderRef builder,
//      Reference* structRefM,
//      LLVMValueRef structExpr,
//      Mutability mutability,
//      Reference* memberType,
//      int memberIndex,
//      const std::string& memberName) override;
//
//  std::vector<LLVMValueRef> destructure(
//      GlobalState* globalState,
//      FunctionState* functionState,
//      BlockState* blockState,
//      LLVMBuilderRef builder,
//      Reference* structType,
//      LLVMValueRef structLE) override;
//
//  LLVMValueRef storeMember(
//      AreaAndFileAndLine from,
//      GlobalState* globalState,
//      FunctionState* functionState,
//      BlockState* blockState,
//      LLVMBuilderRef builder,
//      Reference* structRefM,
//      LLVMValueRef structExpr,
//      Mutability mutability,
//      Reference* memberType,
//      int memberIndex,
//      const std::string& memberName,
//      LLVMValueRef sourceLE) override;
//
//  void destroyArray(
//      GlobalState* globalState,
//      FunctionState* functionState,
//      BlockState* blockState,
//      LLVMBuilderRef builder,
//      Reference* arrayType,
//      LLVMValueRef arrayWrapperLE) override;
//
//  LLVMValueRef constructString(LLVMValueRef sizeLE) override;
//  LLVMValueRef getStringBytesPtr(LLVMValueRef stringRefLE) override;
//  LLVMValueRef getStringLength(LLVMValueRef stringRefLE) override;
//
//  LLVMValueRef constructKnownSizeArray(int size) override;
//  LLVMValueRef getKnownSizeArrayElementsPtr(LLVMValueRef stringRefLE) override;
//  LLVMValueRef constructUnknownSizeArray(LLVMValueRef sizeLE) override;
//  LLVMValueRef getUnknownSizeArrayElementsPtr(LLVMValueRef elementsPtrLE) override;
//  LLVMValueRef getUnknownSizeArrayLength(LLVMValueRef elementsPtrLE) override;
//
//  RcLayoutInfo makeRcLayoutInfo() {
//    return RcLayoutInfo(
//        controlBlockRcMemberIndex,
//        controlBlockObjIdIndex,
//        controlBlockTypeStrIndex);
//  }
//
//public:
//  int controlBlockTypeStrIndex;
//  int controlBlockObjIdIndex;
//  int controlBlockRcMemberIndex;
//  int controlBlockWrciMemberIndex;
//  LLVMTypeRef nonWeakableControlBlockStructL;
//  LLVMTypeRef weakableControlBlockStructL;
//};
//
//#endif
