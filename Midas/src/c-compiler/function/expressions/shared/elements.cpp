#include <iostream>

#include "translatetype.h"

#include "shared.h"
#include "branch.h"

void foreachArrayElement(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef sizeLE,
    LLVMValueRef arrayPtrLE,
    std::function<void(LLVMValueRef, LLVMBuilderRef)> iterationBuilder) {
  LLVMValueRef iterationIndexPtrLE = LLVMBuildAlloca(builder, LLVMInt64Type(), "iterationIndex");
  LLVMBuildStore(builder, LLVMConstInt(LLVMInt64Type(), 0, false), iterationIndexPtrLE);

  buildWhile(
      functionState,
      builder,
      [sizeLE, iterationIndexPtrLE](LLVMBuilderRef conditionBuilder) {
        auto iterationIndexLE =
            LLVMBuildLoad(conditionBuilder, iterationIndexPtrLE, "iterationIndex");
        auto isBeforeEndLE =
            LLVMBuildICmp(
                conditionBuilder,LLVMIntSLT,iterationIndexLE,sizeLE,"iterationIndexIsBeforeEnd");
        return isBeforeEndLE;
      },
      [iterationBuilder, iterationIndexPtrLE, arrayPtrLE](LLVMBuilderRef bodyBuilder) {
        auto iterationIndexLE = LLVMBuildLoad(bodyBuilder, iterationIndexPtrLE, "iterationIndex");
        iterationBuilder(iterationIndexLE, bodyBuilder);
        adjustCounter(bodyBuilder, iterationIndexPtrLE, 1);
      });
}
