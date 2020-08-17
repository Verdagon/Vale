#ifndef REGIONS_SHARED_IMM_MIXIN_H
#define REGIONS_SHARED_IMM_MIXIN_H

#include <llvm-c/Core.h>

#include "function/function.h"
#include "globalstate.h"
#include "function/expressions/shared/shared.h"
#include "stringmixin.h"

class ImmMixin {
public:
  ImmMixin();

  LLVMTypeRef getControlBlockStruct() const {
    return controlBlockStructL;
  }

  LLVMValueRef constructString(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef lengthLE) {
    return stringMixin.constructString(globalState, functionState, builder, lengthLE,
        [this, globalState, builder](LLVMValueRef controlBlockPtr, const std::string& typeName) {
          fillControlBlock(globalState, builder, controlBlockPtr, typeName);

          if (globalState->opt->census) {
            LLVMValueRef resultAsVoidPtrLE =
                LLVMBuildBitCast(
                    builder, controlBlockPtr, LLVMPointerType(LLVMVoidType(), 0), "");
            LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
          }
        });
  }

  LLVMValueRef getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
    return stringMixin.getStringBytesPtr(builder, stringRefLE);
  }
  LLVMValueRef getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
    return stringMixin.getStringLength(builder, stringRefLE);
  }
  LLVMValueRef getStringControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef stringRefLE) {
    return stringMixin.getStringControlBlockPtr(builder, stringRefLE);
  }

  LLVMTypeRef getStringRefType() const {
    return stringMixin.getStringRefType();
  }

  LLVMValueRef getControlBlockPtr(
      LLVMBuilderRef builder,
      // This will be a pointer if a mutable struct, or a fat ref if an interface.
      LLVMValueRef referenceLE,
      Reference* refM);

  LLVMValueRef alias(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* targetRef,
      LLVMValueRef sourceLE);

  void sharingDecrementStrongRc(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* sourceRef,
      LLVMValueRef expr);

  void translateStruct(
      GlobalState* globalState,
      IRegion* region,
      StructDefinition* structM);


  LLVMValueRef getUnknownSizeArrayLength(
      LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE);


private:
  LLVMValueRef getConcreteControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef concretePtrLE);
  LLVMValueRef getInterfaceControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef interfaceRefLE);

  void fillControlBlock(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      LLVMValueRef controlBlockPtrLE,
      const std::string& typeName);


  LLVMTypeRef controlBlockStructL;
  int controlBlockObjIdIndex;
  int controlBlockTypeStrIndex;
  int controlBlockRcMemberIndex;

  StringMixin stringMixin;
};

#endif
