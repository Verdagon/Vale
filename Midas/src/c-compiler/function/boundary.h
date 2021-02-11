#ifndef BOUNDARY_H_
#define BOUNDARY_H_

#include "globalstate.h"
#include "boundary.h"

Ref sendHostObjectIntoVale(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* hostRefMT,
    LLVMValueRef hostRef);

LLVMValueRef sendValeObjectIntoHost(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* valeRefMT,
    Ref valeRef);

#endif
