#ifndef FUNCTION_EXPRESSIONS_SHARED_ELEMENTS_H_
#define FUNCTION_EXPRESSIONS_SHARED_ELEMENTS_H_

#include <llvm-c/Core.h>

#include <unordered_map>
#include <functional>

#include "metal/ast.h"
#include "metal/instructions.h"
#include "globalstate.h"
#include "function/function.h"

void foreachArrayElement(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef sizeLE,
    LLVMValueRef arrayPtrLE,
    std::function<void(LLVMValueRef, LLVMBuilderRef)> iterationBuilder);


#endif
