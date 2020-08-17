#ifndef REGIONS_SHARED_STRUCT_H_
#define REGIONS_SHARED_STRUCT_H_

#include <llvm-c/Core.h>

#include "function/function.h"
#include "globalstate.h"
#include "function/expressions/shared/shared.h"

void translateContentsStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM,
    LLVMTypeRef contentsStructL);

void translateWrapperStruct(
    GlobalState* globalState,
    StructDefinition* structM,
    LLVMTypeRef wrapperStructL,
    LLVMTypeRef contentsStructL,
    LLVMTypeRef controlBlockStructL);

LLVMTypeRef makeWeakRefStruct(
    LLVMTypeRef declaredWeakRefStructL,
    LLVMTypeRef targetWrapperStructL);


LLVMValueRef fillControlBlockObjIdAndTypeStr(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    int controlBlockObjIdIndex,
    int controlBlockTypeStrIndex,
    const std::string& typeName,
    LLVMValueRef newControlBlockLE);

LLVMValueRef fillControlBlockStrongRc(
    LLVMBuilderRef builder,
    LLVMValueRef newControlBlockLE,
    int controlBlockRcMemberIndex);

LLVMValueRef fillControlBlockWrci(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef newControlBlockLE,
    int controlBlockWrciMemberIndex);

LLVMValueRef getWrciFromControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtr,
    int controlBlockWrciMemberIndex);


LLVMValueRef assembleStructWeakRef(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    Reference* structTypeM,
    StructReferend* structReferendM,
    LLVMTypeRef weakRefStructL,
    LLVMValueRef objPtrLE,
    int controlBlockWrciMemberIndex,
    LLVMValueRef controlBlockPtrLE);

LLVMValueRef getWrciFromWeakRef(
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE);

LLVMValueRef getObjPtrFromWeakRef(
    LLVMBuilderRef builder,
    LLVMValueRef weakRefLE);

void incrementWeakRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef expr);

void incrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
    LLVMValueRef expr);

LLVMValueRef adjustStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
    int amount);

LLVMValueRef adjustStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
    int amount);


LLVMValueRef getStrongRcFromControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef structExpr,
    int controlBlockRcMemberIndex);

LLVMValueRef strongRcIsZero(
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    int controlBlockRcMemberIndex);

void decrementWeakRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    LLVMValueRef expr);

void nonOwningDecrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refM,
    int controlBlockRcMemberIndex,
    std::shared_ptr<std::tuple<int, int>> maybeTypeStrAndObjIdIndex,
    LLVMValueRef expr);


void freeConcrete(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    Reference* concreteRefM);

void markWrcDead(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    int controlBlockWrciMemberIndex);

void checkStrongRcZero(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef refLE,
    int controlBlockRcMemberIndex);

#endif
