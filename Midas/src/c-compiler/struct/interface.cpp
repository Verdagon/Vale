#include <iostream>
#include <regions/iregion.h>

#include "interface.h"

#include "translatetype.h"

void declareInterface(
    GlobalState* globalState,
    IRegion* region,
    InterfaceDefinition* interfaceM) {
  region->declareInterface(globalState, interfaceM);
}

void translateInterface(
    GlobalState* globalState,
    IRegion* region,
    InterfaceDefinition* interfaceM) {
  region->translateInterface(globalState, interfaceM);
}
