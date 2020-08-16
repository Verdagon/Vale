//#include <function/expressions/shared/shared.h>
//#include "resilient.h"
//
//ResilientRegion::ResilientRegion() {
//  auto voidLT = LLVMVoidType();
//  auto voidPtrLT = LLVMPointerType(voidLT, 0);
//  auto int1LT = LLVMInt1Type();
//  auto int8LT = LLVMInt8Type();
//  auto int64LT = LLVMInt64Type();
//  auto int8PtrLT = LLVMPointerType(int8LT, 0);
//  auto int64PtrLT = LLVMPointerType(int64LT, 0);
//
//  {
//    auto controlBlockStructL =
//        LLVMStructCreateNamed(
//            LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
//    std::vector<LLVMTypeRef> memberTypesL;
//
//    controlBlockTypeStrIndex = memberTypesL.size();
//    memberTypesL.push_back(int8PtrLT);
//
//    controlBlockObjIdIndex = memberTypesL.size();
//    memberTypesL.push_back(int64LT);
//
//    controlBlockRcMemberIndex = memberTypesL.size();
//    memberTypesL.push_back(int64LT);
//
//    controlBlockWrciMemberIndex = memberTypesL.size();
//    memberTypesL.push_back(int64LT);
//
//    LLVMStructSetBody(
//        controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
//    weakableControlBlockStructL = controlBlockStructL;
//  }
//
//  {
//    auto controlBlockStructL =
//        LLVMStructCreateNamed(
//            LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
//    std::vector<LLVMTypeRef> memberTypesL;
//
//    assert(memberTypesL.size() == controlBlockTypeStrIndex); // should match weakable
//    memberTypesL.push_back(int8PtrLT);
//
//    assert(memberTypesL.size() == controlBlockObjIdIndex); // should match weakable
//    memberTypesL.push_back(int64LT);
//
//    assert(memberTypesL.size() == controlBlockRcMemberIndex); // should match weakable
//    memberTypesL.push_back(int64LT);
//
//    LLVMStructSetBody(
//        controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
//    nonWeakableControlBlockStructL = controlBlockStructL;
//  }
//}
//
//LLVMValueRef ResilientRegion::allocate(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* desiredReference,
//    const std::vector<LLVMValueRef>& membersLE) {
//  assert(false);
//}
//
//LLVMValueRef ResilientRegion::alias(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* resultRef,
//    Ownership targetOwnership,
//    LLVMValueRef expr) {
//  auto sourceRnd = resultRef->referend;
//
//  if (resultRef->ownership == Ownership::SHARE) {
//    incrementStrongRc(from, globalState, functionState, builder, resultRef, expr, makeRcLayoutInfo());
//  } else if (resultRef->ownership == Ownership::BORROW) {
//    incrementWeakRc(from, globalState, functionState, builder, resultRef, expr);
//  } else if (resultRef->ownership == Ownership::WEAK) {
//    incrementWeakRc(from, globalState, functionState, builder, resultRef, expr);
//  } else if (resultRef->ownership == Ownership::OWN) {
//    if (dynamic_cast<InterfaceReferend*>(sourceRnd)) {
//      // We should never acquire an owning reference.
//      // If you trip this, perhaps you're trying to borrow, and you handed in
//      // the wrong thing for resultRef?
//      assert(false);
//    } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
//        dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
//        dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
//      // We might be loading a member as an own if we're destructuring.
//      // Don't adjust the RC, since we're only moving it.
//    } else assert(false);
//  } else assert(false);
//}
//
//void ResilientRegion::dealias(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    BlockState* blockState,
//    LLVMBuilderRef builder,
//    Reference* sourceRef,
//    LLVMValueRef expr) {
//  auto sourceRnd = sourceRef->referend;
//
//  if (sourceRef->ownership == Ownership::SHARE) {
//    sharingDecrementStrongRc(from, globalState, functionState, blockState, builder, sourceRef, expr);
//  } else if (sourceRef->ownership == Ownership::OWN) {
//    if (auto interfaceRnd = dynamic_cast<InterfaceReferend*>(sourceRnd)) {
//      // We can't discard owns, they must be destructured.
//      assert(false); // impl
//    } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
//        dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
//        dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
//      // We can't discard owns, they must be destructured.
//      assert(false);
//    } else assert(false);
//  } else if (sourceRef->ownership == Ownership::BORROW) {
//    decrementWeakRc(from, globalState, functionState, builder, sourceRef, expr);
//  } else if (sourceRef->ownership == Ownership::WEAK) {
//    decrementWeakRc(from, globalState, functionState, builder, sourceRef, expr);
//  } else assert(false);
//}
//
//LLVMValueRef ResilientRegion::loadMember(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* structRefM,
//    LLVMValueRef structExpr,
//    Mutability mutability,
//    Reference* memberType,
//    int memberIndex,
//    const std::string& memberName) {
//  assert(false);
//}
//
//std::vector<LLVMValueRef> ResilientRegion::destructure(
//    GlobalState* globalState,
//    FunctionState* functionState,
//    BlockState* blockState,
//    LLVMBuilderRef builder,
//    Reference* structType,
//    LLVMValueRef structLE) {
//  assert(false);
//}
//
//LLVMValueRef ResilientRegion::storeMember(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    BlockState* blockState,
//    LLVMBuilderRef builder,
//    Reference* structRefM,
//    LLVMValueRef structExpr,
//    Mutability mutability,
//    Reference* memberType,
//    int memberIndex,
//    const std::string& memberName,
//    LLVMValueRef sourceLE) {
//  assert(false);
//}
//
//LLVMTypeRef ResilientRegion::getControlBlockStructForStruct(StructDefinition* structM) {
//  return weakableControlBlockStructL;
//}
//
//LLVMTypeRef ResilientRegion::getControlBlockStructForInterface(InterfaceDefinition* interfaceM) {
//  return weakableControlBlockStructL;
//}
//
//LLVMTypeRef ResilientRegion::getControlBlockStructForKnownSizeArray(KnownSizeArrayT* arrMT) {
//  return weakableControlBlockStructL;
//}
//
//LLVMTypeRef ResilientRegion::getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) {
//  return weakableControlBlockStructL;
//}
