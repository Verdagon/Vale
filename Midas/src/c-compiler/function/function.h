#ifndef FUNCTION_H_
#define FUNCTION_H_

#include <llvm-c/Core.h>

#include <unordered_map>
#include <unordered_set>
#include <iostream>

#include "metal/ast.h"
#include "globalstate.h"

class IRegion;

void translateFunction(
    GlobalState* globalState,
    IRegion* defaultRegion,
    Function* functionM);

LLVMValueRef declareFunction(
    GlobalState* globalState,
    IRegion* region,
    Function* functionM);

#endif