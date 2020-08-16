#ifndef STRUCT_INTERFACE_H_
#define STRUCT_INTERFACE_H_

#include <llvm-c/Core.h>

#include <unordered_map>

#include "metal/ast.h"
#include "globalstate.h"

class IRegion;

void declareInterface(
    GlobalState* globalState,
    IRegion* region,
    InterfaceDefinition* interfaceM);

void translateInterface(
    GlobalState* globalState,
    IRegion* region,
    InterfaceDefinition* interfaceM);

#endif
