#include <translatetype.h>
#include "struct.h"

void censusCheckValid(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef refLE,
    const std::function<LLVMValueRef(LLVMBuilderRef, Reference*, LLVMValueRef)>& getAllocationPointer) {
  if (globalState->opt->census) {
    if (refM->ownership == Ownership::OWN) {
      auto controlBlockPtrLE = getAllocationPointer(builder, refM, refLE);
      buildAssertCensusContains(checkerAFL, globalState, functionState, builder, controlBlockPtrLE);
    } else if (refM->ownership == Ownership::SHARE) {
      if (refM->location == Location::INLINE) {
        // Nothing to do, there's no control block or ref counts or anything.
      } else if (refM->location == Location::YONDER) {
        auto controlBlockPtrLE = getAllocationPointer(builder, refM, refLE);

        // We dont check ref count >0 because imm destructors receive with rc=0.
        //      auto rcLE = getRcFromControlBlockPtr(globalState, builder, controlBlockPtrLE);
        //      auto rcPositiveLE = LLVMBuildICmp(builder, LLVMIntSGT, rcLE, constI64LE(0), "");
        //      buildAssert(checkerAFL, globalState, functionState, blockState, builder, rcPositiveLE, "Invalid RC!");

        buildAssertCensusContains(checkerAFL, globalState, functionState, builder, controlBlockPtrLE);
      } else assert(false);
    } else if (refM->ownership == Ownership::BORROW) {
      auto controlBlockPtrLE = getAllocationPointer(builder, refM, refLE);
      buildAssertCensusContains(checkerAFL, globalState, functionState, builder, controlBlockPtrLE);
    } else assert(false);
  }
}
