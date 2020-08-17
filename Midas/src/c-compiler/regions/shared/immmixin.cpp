#include <function/expressions/shared/branch.h>
#include "immmixin.h"
#include "struct.h"

constexpr int controlBlockTypeStrIndex = 0;
constexpr int controlBlockObjIdIndex = 1;
constexpr int controlBlockRcMemberIndex = 2;

LLVMTypeRef makeImmControlBlockStruct() {
  auto voidLT = LLVMVoidType();
  auto voidPtrLT = LLVMPointerType(voidLT, 0);
  auto int1LT = LLVMInt1Type();
  auto int8LT = LLVMInt8Type();
  auto int64LT = LLVMInt64Type();
  auto int8PtrLT = LLVMPointerType(int8LT, 0);
  auto int64PtrLT = LLVMPointerType(int64LT, 0);

  auto controlBlockStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
  std::vector<LLVMTypeRef> memberTypesL;

  assert(memberTypesL.size() == controlBlockTypeStrIndex); // should match weakable
  memberTypesL.push_back(int8PtrLT);

  assert(memberTypesL.size() == controlBlockObjIdIndex); // should match weakable
  memberTypesL.push_back(int64LT);

  assert(memberTypesL.size() == controlBlockRcMemberIndex); // should match weakable
  memberTypesL.push_back(int64LT);

  LLVMStructSetBody(
      controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
  return controlBlockStructL;
}


ImmMixin::ImmMixin() :
    controlBlockStructL(makeImmControlBlockStruct()),
    stringMixin(controlBlockStructL) {
}

LLVMValueRef ImmMixin::getControlBlockPtr(
    LLVMBuilderRef builder,
    // This will be a pointer if a mutable struct, or a fat ref if an interface.
    LLVMValueRef referenceLE,
    Reference* refM) {
  assert(refM->ownership == Ownership::SHARE);

  if (dynamic_cast<InterfaceReferend *>(refM->referend)) {
    return getInterfaceControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<StructReferend *>(refM->referend)) {
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<KnownSizeArrayT *>(refM->referend)) {
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<UnknownSizeArrayT *>(refM->referend)) {
    return getConcreteControlBlockPtr(builder, referenceLE);
  } else if (dynamic_cast<Str *>(refM->referend)) {
    return getStringControlBlockPtr(builder, referenceLE);
  } else {
    std::cerr << "Unknown: " << typeid(*refM->referend).name() << std::endl;
    assert(false);
    return nullptr;
  }
}

LLVMValueRef ImmMixin::getConcreteControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef concretePtrLE) {
  // Control block is always the 0th element of every concrete struct.
  auto resultLE = LLVMBuildStructGEP(builder, concretePtrLE, 0, "controlPtr");
  assert(LLVMTypeOf(resultLE) == LLVMPointerType(controlBlockStructL, 0));
  return resultLE;
}

LLVMValueRef ImmMixin::getInterfaceControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef interfaceRefLE) {
  // Interface fat pointer's first element points directly at the control block,
  // and we dont have to cast it. We would have to cast if we were accessing the
  // actual object though.
  return LLVMBuildExtractValue(builder, interfaceRefLE, 0, "controlPtr");
}

// Returns object ID
void ImmMixin::fillControlBlock(
    GlobalState* globalState,
    LLVMBuilderRef builder,
    LLVMValueRef controlBlockPtrLE,
    const std::string& typeName) {
  LLVMValueRef newControlBlockLE = LLVMGetUndef(controlBlockStructL);
  newControlBlockLE =
      fillControlBlockObjIdAndTypeStr(
          globalState, builder, controlBlockObjIdIndex, controlBlockTypeStrIndex, typeName, newControlBlockLE);
  newControlBlockLE =
      fillControlBlockStrongRc(builder, newControlBlockLE, controlBlockRcMemberIndex);
  LLVMBuildStore(builder, newControlBlockLE, controlBlockPtrLE);
}

LLVMValueRef ImmMixin::alias(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* targetRef,
    LLVMValueRef sourceLE) {
  if (targetRef->location == Location::INLINE) {
    // Do nothing
  } else if (targetRef->location == Location::YONDER) {
    auto controlBlockPtr = getControlBlockPtr(builder, sourceLE, targetRef);
    assert(LLVMTypeOf(controlBlockPtr) == LLVMPointerType(controlBlockStructL, 0));
    incrementStrongRc(
        from, globalState, functionState, builder, targetRef,
        controlBlockRcMemberIndex,
        std::make_shared<std::tuple<int, int>>(
            std::make_tuple(controlBlockTypeStrIndex, controlBlockObjIdIndex)),
        controlBlockPtr);
  } else assert(false);
  return sourceLE;
}

void ImmMixin::sharingDecrementStrongRc(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    BlockState* blockState,
    LLVMBuilderRef builder,
    Reference* sourceRef,
    LLVMValueRef expr) {
  auto sourceRnd = sourceRef->referend;

  if (dynamic_cast<Int*>(sourceRnd) ||
      dynamic_cast<Bool*>(sourceRnd) ||
      dynamic_cast<Float*>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend*>(sourceRnd)) {
    if (sourceRef->location == Location::INLINE) {
      assert(false); // impl
    } else {
      auto controlBlockPtrLE = getControlBlockPtr(builder, expr, sourceRef);
      auto rcLE =
          adjustStrongRc(
              from, globalState, functionState, builder, controlBlockPtrLE, sourceRef,
              controlBlockRcMemberIndex,
              std::make_shared<std::tuple<int, int>>(
                  std::make_tuple(controlBlockTypeStrIndex, controlBlockObjIdIndex)),
              -1);
      buildIf(
          functionState,
          builder,
          isZeroLE(builder, rcLE),
          [globalState, functionState, expr, interfaceRnd, sourceRef](LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceRef->referend);

            auto interfaceM = globalState->program->getInterface(interfaceRnd->fullName);
            int indexInEdge = -1;
            for (int i = 0; i < interfaceM->methods.size(); i++) {
              if (interfaceM->methods[i]->prototype == immDestructor) {
                indexInEdge = i;
              }
            }
            assert(indexInEdge >= 0);

            std::vector<LLVMValueRef> argExprsL = { expr };
            buildInterfaceCall(functionState->defaultRegion, thenBuilder, argExprsL, 0, indexInEdge);
          });
    }
  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
    if (sourceRef->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      auto controlBlockPtrLE = getControlBlockPtr(builder, expr, sourceRef);
      auto rcLE =
          adjustStrongRc(
              from, globalState, functionState, builder, controlBlockPtrLE, sourceRef,
              controlBlockRcMemberIndex,
              std::make_shared<std::tuple<int, int>>(
                  std::make_tuple(controlBlockTypeStrIndex, controlBlockObjIdIndex)),
              -1);
      buildIf(
          functionState,
          builder,
          isZeroLE(builder, rcLE),
          [from, globalState, functionState, expr, sourceRef](LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceRef->referend);
            auto funcL = globalState->getFunction(immDestructor->name);
            std::vector<LLVMValueRef> argExprsL = { expr };
            return LLVMBuildCall(thenBuilder, funcL, argExprsL.data(), argExprsL.size(), "");
          });
    }
  } else if (dynamic_cast<Str*>(sourceRnd)) {
    auto controlBlockPtrLE = getControlBlockPtr(builder, expr, sourceRef);
    auto rcLE =
        adjustStrongRc(
            from, globalState, functionState, builder, controlBlockPtrLE, sourceRef,
            controlBlockRcMemberIndex,
            std::make_shared<std::tuple<int, int>>(
                std::make_tuple(controlBlockTypeStrIndex, controlBlockObjIdIndex)),
            -1);
    buildIf(
        functionState,
        builder,
        isZeroLE(builder, rcLE),
        [this, globalState, functionState, sourceRef, controlBlockPtrLE](LLVMBuilderRef thenBuilder) {
          freeConcrete(globalState, functionState, thenBuilder, controlBlockPtrLE, sourceRef);
        });
  } else {
    std::cerr << "Unimplemented type in discard: "
        << typeid(*sourceRef->referend).name() << std::endl;
    assert(false);
  }
}

void ImmMixin::translateStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM) {
//  auto contentsStructL = getContentsStruct(structM->name);
//  translateContentsStruct(globalState, region, structM, contentsStructL);
//
//  auto wrapperStructL = getWrapperStruct(structM->name);
//  translateWrapperStruct(
//      globalState, structM, wrapperStructL, contentsStructL, controlBlockStructL);
  assert(false);
}


LLVMValueRef ImmMixin::getUnknownSizeArrayLength(
    LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) {
  auto resultLE =
      LLVMBuildStructGEP(
          builder,
          unknownSizeArrayWrapperPtrLE,
          1, // Length is after the control block and before contents.
          "usaLenPtr");
  assert(LLVMTypeOf(resultLE) == LLVMPointerType(LLVMInt64Type(), 0));
  return LLVMBuildLoad(builder, resultLE, "usaLen");
}
