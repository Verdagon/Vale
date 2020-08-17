#ifndef REGIONS_SHARED_CENSUS_H_
#define REGIONS_SHARED_CENSUS_H_

#include <llvm-c/Core.h>

#include "function/function.h"
#include "globalstate.h"
#include "function/expressions/shared/shared.h"

void censusCheckValid(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef refLE,
    const std::function<LLVMValueRef(LLVMBuilderRef, Reference*, LLVMValueRef)>& getAllocationPointer);

#endif
