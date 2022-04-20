#ifndef _UTILS_CALL_H_
#define _UTILS_CALL_H_

LLVMValueRef buildCall(
    LLVMBuilderRef builder, LLVMValueRef function,
    std::vector<LLVMValueRef> args,
    const char *name = "");

#endif
