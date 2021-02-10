#ifndef SERIALIZE_H_
#define SERIALIZE_H_

#include "globalstate.h"

Ref getCalculatedSerializedSize(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref);
void defineCalculateSerializedSizeFunc(
    GlobalState* globalState,
    StructReferend* substruct,
    Prototype* substructPrototype,
    LLVMValueRef functionL);
void addCalculateSerializedSizeFunctions(GlobalState* globalState);

#endif
