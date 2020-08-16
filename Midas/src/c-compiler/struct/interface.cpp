#include <iostream>
#include <regions/iregion.h>

#include "interface.h"

#include "translatetype.h"

void declareInterface(
    GlobalState* globalState,
    IRegion* region,
    InterfaceDefinition* interfaceM) {

  auto interfaceRefStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), interfaceM->name->name.c_str());
  assert(globalState->interfaceRefStructs.count(interfaceM->name->name) == 0);
  globalState->interfaceRefStructs.emplace(interfaceM->name->name, interfaceRefStructL);

  auto interfaceTableStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (interfaceM->name->name + "itable").c_str());
  assert(globalState->interfaceTableStructs.count(interfaceM->name->name) == 0);
  globalState->interfaceTableStructs.emplace(interfaceM->name->name, interfaceTableStructL);

  auto interfaceWeakRefStructL =
      LLVMStructCreateNamed(
          LLVMGetGlobalContext(), (interfaceM->name->name + "w").c_str());
  assert(globalState->interfaceWeakRefStructs.count(interfaceM->name->name) == 0);
  globalState->interfaceWeakRefStructs.emplace(interfaceM->name->name, interfaceWeakRefStructL);
}

void translateInterface(
    GlobalState* globalState,
    IRegion* region,
    InterfaceDefinition* interfaceM) {
  region->translateInterface(globalState, interfaceM);
}
