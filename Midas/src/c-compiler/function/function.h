#ifndef FUNCTION_H_
#define FUNCTION_H_

#include <llvm-c/Core.h>

#include <unordered_map>
#include <unordered_set>
#include <iostream>

#include "metal/ast.h"
#include "globalstate.h"

void translateFunction(
    GlobalState* globalState,
    Function* functionM);

LLVMValueRef declareFunction(
    GlobalState* globalState,
    Function* functionM);

#endif