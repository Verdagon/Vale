#include <iostream>
#include <regions/iregion.h>

#include "struct.h"

#include "translatetype.h"

void declareStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM) {

  auto innerStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), structM->name->name.c_str());
  assert(globalState->innerStructs.count(structM->name->name) == 0);
  globalState->innerStructs.emplace(structM->name->name, innerStructL);

  auto countedStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (structM->name->name + "rc").c_str());
  assert(globalState->countedStructs.count(structM->name->name) == 0);
  globalState->countedStructs.emplace(structM->name->name, countedStructL);

  auto structWeakRefStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (structM->name->name + "w").c_str());
  assert(globalState->structWeakRefStructs.count(structM->name->name) == 0);
  globalState->structWeakRefStructs.emplace(structM->name->name, structWeakRefStructL);
}

void translateStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM) {
  return region->translateStruct(globalState, structM);
}

void declareEdge(
    GlobalState* globalState,
    Edge* edge) {

  auto interfaceTableStructL =
      globalState->getInterfaceTableStruct(edge->interfaceName->fullName);

  auto edgeName =
      edge->structName->fullName->name + edge->interfaceName->fullName->name;
  auto itablePtr =
      LLVMAddGlobal(globalState->mod, interfaceTableStructL, edgeName.c_str());
  LLVMSetLinkage(itablePtr, LLVMExternalLinkage);

  globalState->interfaceTablePtrs.emplace(edge, itablePtr);
}

void translateEdge(
    GlobalState* globalState,
    Edge* edge) {

  auto interfaceTableStructL =
      globalState->getInterfaceTableStruct(edge->interfaceName->fullName);

  auto builder = LLVMCreateBuilder();
  auto itableLE = LLVMGetUndef(interfaceTableStructL);
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    itableLE = LLVMBuildInsertValue(
        builder,
        itableLE,
        globalState->getFunction(funcName),
        i,
        std::to_string(i).c_str());
  }
  LLVMDisposeBuilder(builder);

  auto itablePtr = globalState->getInterfaceTablePtr(edge);
  LLVMSetInitializer(itablePtr,  itableLE);
}
