#include <llvm-c/Core.h>
#include "ref.h"
#include "../../../region/iregion.h"
#include "../../../globalstate.h"

Ref wrap(IRegion* region, Reference* refM, LLVMValueRef exprLE) {
  assert(LLVMTypeOf(exprLE) == region->translateType(refM));
  return Ref(refM, exprLE);
}

Ref wrap(IRegion* region, Reference* refM, WrapperPtrLE wrapperPtr) {
  assert(refM == wrapperPtr.refM);
  assert(LLVMTypeOf(wrapperPtr.refLE) == region->translateType(refM));
  return Ref(refM, wrapperPtr.refLE);
}

Ref wrap(IRegion* region, Reference* refM, InterfaceFatPtrLE interfaceFatPtrLE) {
  assert(refM == interfaceFatPtrLE.refM);
  assert(LLVMTypeOf(interfaceFatPtrLE.refLE) == region->translateType(refM));
  return Ref(refM, interfaceFatPtrLE.refLE);
}

Ref wrap(IRegion* region, Reference* refM, WeakFatPtrLE weakFatPtrLE) {
  assert(refM == weakFatPtrLE.refM);
  assert(LLVMTypeOf(weakFatPtrLE.refLE) == region->translateType(refM));
  return Ref(refM, weakFatPtrLE.refLE);
}

Ref wrap(GlobalState* globalState, Reference* refM, LiveRef liveRef) {
  assert(refM == liveRef.refM);
  return wrap(globalState->getRegion(refM), refM, liveRef.refLE);
}

LiveRef toLiveRef(WrapperPtrLE wrapperPtrLE) {
  return LiveRef(wrapperPtrLE.refM, wrapperPtrLE.wrapperStructLT, wrapperPtrLE.refLE);
}

WrapperPtrLE toWrapperPtr(FunctionState* functionState, LLVMBuilderRef builder, KindStructs* kindStructs, Reference* refMT, LiveRef liveRef) {
  return kindStructs->makeWrapperPtr(FL(), functionState, builder, refMT, liveRef.refLE);
}

LiveRef toLiveRef(AreaAndFileAndLine checkerAFL, FunctionState* functionState, LLVMBuilderRef builder, KindStructs* kindStructs, Reference* refM, LLVMValueRef ptrLE) {
    return toLiveRef(kindStructs->makeWrapperPtr(checkerAFL, functionState, builder, refM, ptrLE));
}

LiveRef toLiveRef(AreaAndFileAndLine checkerAFL, GlobalState* globalState, FunctionState* functionState, LLVMBuilderRef builder, KindStructs* kindStructs, Reference* refM, Ref ref) {
    auto ptrLE = globalState->getRegion(refM)->checkValidReference(checkerAFL, functionState, builder, true, refM, ref);
    return toLiveRef(kindStructs->makeWrapperPtr(checkerAFL, functionState, builder, refM, ptrLE));
}

LLVMValueRef checkValidInternalReference(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    bool expectLive,
    Reference* refM,
    Ref ref) {
  return globalState->getRegion(refM)
      ->checkValidReference(checkerAFL, functionState, builder, expectLive, refM, ref);
}
