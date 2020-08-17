#ifndef GLOBALSTATE_H_
#define GLOBALSTATE_H_

#include <llvm-c/Core.h>

#include <unordered_map>
#include <metal/metalcache.h>
#include "valeopts.h"

#define CONTROL_BLOCK_STRUCT_NAME "__ControlBlock"

class GlobalState {
public:
  LLVMTargetMachineRef machine;
  LLVMContextRef context;
  LLVMDIBuilderRef dibuilder;
  LLVMMetadataRef compileUnit;
  LLVMMetadataRef difile;

  ValeOptions *opt;

  LLVMTargetDataRef dataLayout;
  LLVMModuleRef mod;
  int ptrSize;

  MetalCache metalCache;

  Program* program;
  LLVMValueRef objIdCounter;
  LLVMValueRef liveHeapObjCounter;
  LLVMValueRef mutDerefCounter;
  LLVMValueRef mutRcAccessCounter;
  LLVMValueRef malloc, free, assert, exit, assertI64Eq, flareI64, printCStr,
      getch, printInt, printBool, initStr, addStr, eqStr, strncpy, printStr, intToCStr,
      strlen, censusContains, censusAdd, censusRemove, panic;

  LLVMValueRef allocWrc, incrementWrc, decrementWrc, wrcIsLive, markWrcDead, getNumWrcs;

  LLVMBuilderRef stringConstantBuilder;
  std::unordered_map<std::string, LLVMValueRef> stringConstants;

  std::unordered_map<std::string, LLVMValueRef> functions;

  LLVMValueRef getFunction(Name* name) {
    auto functionIter = functions.find(name->name);
    assert(functionIter != functions.end());
    return functionIter->second;
  }

  LLVMValueRef getOrMakeStringConstant(const std::string& str) {
    auto iter = stringConstants.find(str);
    if (iter == stringConstants.end()) {

      iter =
          stringConstants.emplace(
              str,
              LLVMBuildGlobalStringPtr(
                  stringConstantBuilder,
                  str.c_str(),
                  (std::string("conststr") + std::to_string(stringConstants.size())).c_str()))
          .first;
    }
    return iter->second;
  }
};

#endif
