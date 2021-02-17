#ifndef GLOBALSTATE_H_
#define GLOBALSTATE_H_

#include <llvm-c/Core.h>

#include <unordered_map>
#include <metal/metalcache.h>
#include <region/common/defaultlayout/structs.h>

#include "metal/ast.h"
#include "metal/instructions.h"
#include "valeopts.h"

class IRegion;
class IReferendStructsSource;
class IWeakRefStructsSource;
class ControlBlock;
class Linear;

constexpr int LGT_ENTRY_MEMBER_INDEX_FOR_GEN = 0;
constexpr int LGT_ENTRY_MEMBER_INDEX_FOR_NEXT_FREE = 1;

class GlobalState {
public:
  LLVMTargetMachineRef machine = nullptr;
  LLVMContextRef context = nullptr;
  LLVMDIBuilderRef dibuilder = nullptr;
  LLVMMetadataRef compileUnit = nullptr;
  LLVMMetadataRef difile = nullptr;

  ValeOptions *opt = nullptr;

  LLVMTargetDataRef dataLayout = nullptr;
  LLVMModuleRef mod = nullptr;
  int ptrSize = 0;

  MetalCache* metalCache;

  LLVMTypeRef ram64Struct = nullptr;

  Program* program = nullptr;

  LLVMValueRef numMainArgs = nullptr;
  LLVMValueRef mainArgs = nullptr;

  LLVMValueRef objIdCounter = nullptr;
  LLVMValueRef liveHeapObjCounter = nullptr;
  LLVMValueRef derefCounter = nullptr;
  LLVMValueRef mutRcAdjustCounter = nullptr;
  LLVMValueRef livenessCheckCounter = nullptr;
  // an i64 pointer to null.
  LLVMValueRef ram64 = nullptr;
  LLVMValueRef writeOnlyGlobal = nullptr;
  LLVMValueRef crashGlobal = nullptr;
  // Initialized to &writeOnlyGlobal / 8 in main.
  // We can use this to easily write an i64 into NULL or the write only global at runtime.
  LLVMValueRef ram64IndexToWriteOnlyGlobal = nullptr;
  LLVMValueRef malloc = nullptr, free = nullptr, assert = nullptr, exit = nullptr,
      assertI64Eq = nullptr, printCStr = nullptr,
      getch = nullptr, printInt = nullptr,
      strlen = nullptr, censusContains = nullptr, censusAdd = nullptr, censusRemove = nullptr,
      newVStr = nullptr,
  getStrCharsFunc = nullptr,
      getStrNumBytesFunc = nullptr;


  LLVMTypeRef wrcTableStructLT = nullptr;
  LLVMValueRef expandWrcTable = nullptr, checkWrci = nullptr, getNumWrcs = nullptr;

  LLVMTypeRef lgtTableStructLT, lgtEntryStructLT = nullptr; // contains generation and next free
  LLVMValueRef expandLgt = nullptr, checkLgti = nullptr, getNumLiveLgtEntries = nullptr;

  LLVMValueRef strncpy = nullptr;
  LLVMValueRef memcpy = nullptr;

  LLVMValueRef genMalloc = nullptr, genFree = nullptr;


  // This is a global, we can return this when we want to return never. It should never actually be
  // used as an input to any expression in any function though.
  LLVMValueRef neverPtr = nullptr;

  LLVMBuilderRef stringConstantBuilder = nullptr;
  std::unordered_map<std::string, LLVMValueRef> stringConstants;

  std::unordered_map<Edge*, LLVMValueRef> interfaceTablePtrs;

  std::unordered_map<std::string, LLVMValueRef> functions;
  std::unordered_map<std::string, LLVMValueRef> externFunctions;

  // These contain the extra interface methods that Midas adds to particular interfaces.
  // For example, for every immutable, Midas needs to add a serialize() method that
  // adds it to an outgoing linear buffer.
  std::unordered_map<InterfaceReferend*, std::vector<InterfaceMethod*>> interfaceExtraMethods;
  std::unordered_map<Edge*, Edge*> extraAdditionsEdges;
  std::unordered_map<Prototype*, LLVMValueRef> extraFunctions;

  std::unordered_map<Name*, StructDefinition*> extraStructs;
  std::unordered_map<Name*, InterfaceDefinition*> extraInterfaces;

  StructDefinition* lookupStruct(Name* name) {
    auto structI = extraStructs.find(name);
    if (structI != extraStructs.end()) {
      return structI->second;
    }
    return program->getStruct(name);
  }

  InterfaceDefinition* lookupInterface(Name* name) {
    auto interfaceI = extraInterfaces.find(name);
    if (interfaceI != extraInterfaces.end()) {
      return interfaceI->second;
    }
    return program->getInterface(name);
  }

  LLVMValueRef lookupFunction(Prototype* prototype) {
    auto iter = extraFunctions.find(prototype);
    if (iter != extraFunctions.end()) {
      return iter->second;
    }
    auto funcIter = functions.find(prototype->name->name);
    assert(funcIter != functions.end());
    return funcIter->second;
  }

  int getInterfaceMethodIndex(InterfaceReferend* interfaceReferendM, Prototype* prototype) {
    auto interfaceDefM = program->getInterface(interfaceReferendM->fullName);
    for (int i = 0; i < interfaceDefM->methods.size(); i++) {
      if (interfaceDefM->methods[i]->prototype == prototype) {
        return i;
      }
    }
    auto iter = interfaceExtraMethods.find(interfaceReferendM);
    assert(iter != interfaceExtraMethods.end());
    auto extraMethods = iter->second;
    for (int i = 0; i < extraMethods.size(); i++) {
      if (extraMethods[i]->prototype == prototype) {
        return i;
      }
    }
    assert(false);
  }


  Weakability getReferendWeakability(Referend* referend);

  Mutability getReferendMutability(Referend* referendM);

  Ref constI64(int64_t x);
  Ref constI1(bool b);
  Ref buildAdd(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b);
  Ref buildMod(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b);
  Ref buildMultiply(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b);
  Ref buildDivide(FunctionState* functionState, LLVMBuilderRef builder, Ref a, Ref b);

  Name* getReferendName(Referend* referend);

  Name* measureName = nullptr;
  Name* serializeName = nullptr;
  Name* unserializeName = nullptr;

  LLVMBuilderRef valeMainBuilder = nullptr;

  IRegion* rcImm = nullptr;
  IRegion* mutRegion = nullptr;
  IRegion* unsafeRegion = nullptr;
  IRegion* assistRegion = nullptr;
  Linear* linearRegion = nullptr;


  std::tuple<std::vector<LLVMTypeRef>, std::vector<LLVMValueRef>>
  getEdgeFunctionTypesAndFunctions(Edge* edge);


  IRegion* getRegion(Reference* referenceM);
  IRegion* getRegion(RegionId* regionId);
  LLVMValueRef getFunction(Name* name);
  LLVMValueRef getInterfaceTablePtr(Edge* edge);
  LLVMValueRef getOrMakeStringConstant(const std::string& str);
};

#endif
