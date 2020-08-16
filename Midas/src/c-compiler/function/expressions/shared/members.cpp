#include <iostream>

#include "translatetype.h"

#include "shared.h"
#include "function/functionstate.h"

LLVMValueRef getStructContentsPtr(
    LLVMBuilderRef builder, LLVMValueRef concretePtrLE) {
  return LLVMBuildStructGEP(
      builder,
      concretePtrLE,
      1, // Inner struct is after the control block.
      "contentsPtr");
}

void storeInnerStructMember(
    LLVMBuilderRef builder,
    LLVMValueRef innerStructPtrLE,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef newValueLE) {
  assert(LLVMGetTypeKind(LLVMTypeOf(innerStructPtrLE)) == LLVMPointerTypeKind);
  LLVMBuildStore(
      builder,
      newValueLE,
      LLVMBuildStructGEP(
          builder, innerStructPtrLE, memberIndex, memberName.c_str()));
}

LLVMValueRef swapMember(
    LLVMBuilderRef builder,
    LLVMValueRef structExpr,
    int memberIndex,
    const std::string& memberName,
    LLVMValueRef newMemberLE) {
  LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder,
      structExpr);

  auto memberPtrLE =
      LLVMBuildStructGEP(
          builder, innerStructPtrLE, memberIndex, memberName.c_str());
  auto oldMemberLE =
      LLVMBuildLoad(
          builder,
          memberPtrLE,
          memberName.c_str());
  // We don't adjust the oldMember's RC here because even though we're acquiring
  // a reference to it, the struct is losing its reference, so it cancels out.

  storeInnerStructMember(
      builder, innerStructPtrLE, memberIndex, memberName, newMemberLE);
  // We don't adjust the newMember's RC here because even though the struct is
  // acquiring a reference to it, we're losing ours, so it cancels out.

  return oldMemberLE;
}

std::vector<LLVMValueRef> getMemberPtrsLE(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    StructDefinition* structM,
    LLVMValueRef innerStructPtrLE) {
  std::vector<LLVMValueRef> membersLE;
  for (int i = 0; i < structM->members.size(); i++) {
    auto memberName = structM->members[i]->name;
    auto memberPtrLE =
        LLVMBuildStructGEP(
            builder, innerStructPtrLE, i, memberName.c_str());
    auto memberLE =
        LLVMBuildLoad(
            builder,
            memberPtrLE,
            memberName.c_str());
    functionState->defaultRegion->checkValidReference(
        FL(), globalState, functionState, builder, structM->members[i]->type, memberLE);
    membersLE.push_back(memberLE);
  }
  return membersLE;
}