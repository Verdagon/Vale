
#include <function/expressions/shared/shared.h>
#include "globalstate.h"
#include "translatetype.h"

std::tuple<std::vector<LLVMTypeRef>, std::vector<LLVMValueRef>>
GlobalState::getEdgeFunctionTypesAndFunctions(Edge* edge) {
  auto interfaceM = program->getInterface(edge->interfaceName->fullName);

  std::vector<LLVMTypeRef> interfaceFunctionsLT;
  std::vector<LLVMValueRef> edgeFunctionsL;
  for (int i = 0; i < edge->structPrototypesByInterfaceMethod.size(); i++) {
    auto interfaceFunctionLT =
        translateInterfaceMethodToFunctionType(
            region, interfaceM->methods[i]);
    interfaceFunctionsLT.push_back(interfaceFunctionLT);

    assert(false); // add in the extra methods from the extra edges

    auto funcName = edge->structPrototypesByInterfaceMethod[i].second->name;
    auto edgeFunctionL = getFunction(funcName);
    edgeFunctionsL.push_back(edgeFunctionL);
  }
  return std::make_tuple(interfaceFunctionsLT, edgeFunctionsL);
}
