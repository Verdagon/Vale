#ifndef REGIONS_SHARED_STRING_MIXIN_H
#define REGIONS_SHARED_STRING_MIXIN_H

#include <llvm-c/Core.h>

#include "function/function.h"
#include "globalstate.h"
#include "function/expressions/shared/shared.h"

class StringMixin {
public:
  StringMixin(LLVMTypeRef nonWeakableControlBlockStructL);

  LLVMValueRef constructString(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMValueRef lengthLE,
      const std::function<void(LLVMValueRef controlBlockPtrLE, const std::string& typeName)>&
          initializeHeader);

  LLVMValueRef getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE);
  LLVMValueRef getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE);
  LLVMValueRef getStringControlBlockPtr(
      LLVMBuilderRef builder,
      LLVMValueRef stringRefLE);

  LLVMTypeRef getStringRefType() const;

private:
  LLVMTypeRef stringHeapStructL;
  LLVMTypeRef stringRefStructL;
};

#endif
