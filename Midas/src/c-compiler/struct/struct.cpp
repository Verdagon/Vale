#include <iostream>
#include <regions/iregion.h>

#include "struct.h"

#include "translatetype.h"

void declareStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM) {
  region->declareStruct(globalState, structM);
}

void translateStruct(
    GlobalState* globalState,
    IRegion* region,
    StructDefinition* structM) {
  return region->translateStruct(globalState, structM);
}

void declareEdge(
    GlobalState* globalState,
    IRegion* region,
    Edge* edge) {
  region->declareEdge(globalState, edge);
}

void translateEdge(
    GlobalState* globalState,
    IRegion* region,
    Edge* edge) {
  region->translateEdge(globalState, edge);
}
