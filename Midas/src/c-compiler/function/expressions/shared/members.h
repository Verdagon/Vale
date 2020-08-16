#ifndef FUNCTION_EXPRESSIONS_SHARED_MEMBERS_H_
#define FUNCTION_EXPRESSIONS_SHARED_MEMBERS_H_

#include <llvm-c/Core.h>

#include <unordered_map>
#include <functional>

#include "metal/ast.h"
#include "metal/instructions.h"
#include "globalstate.h"
#include "function/function.h"
#include "shared.h"

void storeInnerStructMember(
    LLVMBuilderRef builder,
    LLVMValueRef innerStructPtrLE,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef newValueLE);

LLVMValueRef swapMember(
    LLVMBuilderRef builder,
    LLVMValueRef structExpr,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef newMemberLE);

std::vector<LLVMValueRef> getMemberPtrsLE(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    StructDefinition* structM,
    LLVMValueRef innerStructPtrLE);

#endif
