#include "stringmixin.h"

StringMixin::StringMixin(LLVMTypeRef nonWeakableControlBlockStructL) {
  auto voidLT = LLVMVoidType();
  auto int8LT = LLVMInt8Type();
  auto int64LT = LLVMInt64Type();

  {
    stringHeapStructL =
        LLVMStructCreateNamed(
            LLVMGetGlobalContext(), "__Str");
    std::vector<LLVMTypeRef> memberTypesL;
    memberTypesL.push_back(nonWeakableControlBlockStructL);
    memberTypesL.push_back(LLVMArrayType(int8LT, 0));
    LLVMStructSetBody(
        stringHeapStructL, memberTypesL.data(), memberTypesL.size(), false);
  }

  {
    stringRefStructL =
        LLVMStructCreateNamed(
            LLVMGetGlobalContext(), "__Str_ref");
    std::vector<LLVMTypeRef> memberTypesL;
    memberTypesL.push_back(int64LT);
    memberTypesL.push_back(LLVMPointerType(stringHeapStructL, 0));
    LLVMStructSetBody(
        stringRefStructL, memberTypesL.data(), memberTypesL.size(), false);
  }
}

LLVMValueRef StringMixin::constructString(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE,
    const std::function<void(LLVMValueRef controlBlockPtrLE, const std::string& typeName)>&
        initializeHeader) {

  // The +1 is for the null terminator at the end, for C compatibility.
  auto sizeBytesLE =
      LLVMBuildAdd(
          builder,
          makeConstIntExpr(
              builder,
              LLVMInt64Type(),
              LLVMABISizeOfType(globalState->dataLayout, stringHeapStructL) + 1),
          lengthLE,
          "strMallocSizeBytes");

  auto destVoidPtrLE =
      LLVMBuildCall(builder, globalState->malloc, &sizeBytesLE, 1, "donePtr");

  adjustCounter(builder, globalState->liveHeapObjCounter, 1);

  auto newStrHeapPtrLE =
      LLVMBuildBitCast(
          builder,
          destVoidPtrLE,
          LLVMPointerType(stringHeapStructL, 0),
          "newStrHeapPtr");

  auto stringRefLE = LLVMGetUndef(stringRefStructL);
  stringRefLE =
      LLVMBuildInsertValue(
          builder,
          stringRefLE,
          lengthLE,
          0,
          "strRefWithLen");
  stringRefLE =
      LLVMBuildInsertValue(
          builder,
          stringRefLE,
          newStrHeapPtrLE,
          1,
          "strRef");

  initializeHeader(getStringControlBlockPtr(builder, stringRefLE), "Str");

  // Remember, the caller still needs to initialize the actual chars inside!

  return stringRefLE;
}

LLVMTypeRef StringMixin::getStringRefType() const {
  return stringRefStructL;
}

LLVMValueRef StringMixin::getStringControlBlockPtr(
    LLVMBuilderRef builder,
    LLVMValueRef stringRefLE) {
  // 0th element of ref is the size, 1th element is the pointer to the heap struct.
  auto heapPtrLE = LLVMBuildExtractValue(builder, stringRefLE, 1, "strHeapPtr");
  // Control block is 0th element of the heap struct.
  return LLVMBuildStructGEP(builder, heapPtrLE, 0, "strControlBlockPtr");
}

LLVMValueRef StringMixin::getStringBytesPtr(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
  assert(LLVMTypeOf(stringRefLE) == stringRefStructL);
  auto heapPtrLE = LLVMBuildExtractValue(builder, stringRefLE, 1, "strHeapPtr");
  assert(LLVMTypeOf(heapPtrLE) == LLVMPointerType(stringHeapStructL, 0));
  // Bytes array is second element of the heap struct.
  auto bytesArrayPtrLE = LLVMBuildStructGEP(builder, heapPtrLE, 1, "strBytesArrayPtr");
  assert(LLVMTypeOf(bytesArrayPtrLE) == LLVMPointerType(LLVMArrayType(LLVMInt8Type(), 0), 0));
  // LLVMBuildGEP and LLVMBuildInBoundsGEP didn't work to turn the above array pointer into a
  // pointer to its first element, it kept crashing. So, as per
  // https://stackoverflow.com/questions/37901866/get-pointer-to-first-element-of-array-in-llvm-ir
  // we're just bitcasting it.
  return LLVMBuildPointerCast(builder, bytesArrayPtrLE, LLVMPointerType(LLVMInt8Type(), 0), "strBytesPtr");
}

LLVMValueRef StringMixin::getStringLength(LLVMBuilderRef builder, LLVMValueRef stringRefLE) {
  return LLVMBuildExtractValue(builder, stringRefLE, 0, "len");
}

