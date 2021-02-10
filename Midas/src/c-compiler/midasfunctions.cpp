
#include "midasfunctions.h"
#include "serialize.h"
#include "region/iregion.h"

void declareExtraInterfaceMethod(
    GlobalState* globalState,
    InterfaceReferend* referend,
    InterfaceMethod* newMethod,
    std::function<LLVMValueRef(Prototype*)> declarer,
    std::function<void(StructReferend*, Prototype*)> bodyGenerator) {
  auto program = globalState->program;

  std::vector<Prototype*> substructPrototypes;

  auto interfaceExtraMethodsI = globalState->interfaceExtraMethods.find(referend);
  assert(interfaceExtraMethodsI != globalState->interfaceExtraMethods.end());
  auto interfaceExtraMethods = interfaceExtraMethodsI->second;
  interfaceExtraMethods.push_back(newMethod);

  for (auto edgeAndAdditionsEdge : globalState->extraAdditionsEdges) {
    if (edgeAndAdditionsEdge.first->interfaceName == referend) {
      auto substruct = edgeAndAdditionsEdge.first->structName;

      auto additionsEdge = edgeAndAdditionsEdge.second;
      Reference* substructReference = nullptr; assert(false); // TODO
      Prototype* substructPrototype = nullptr; assert(false); // TODO
      LLVMTypeRef functionLT = nullptr; assert(false); // TODO
      // globalState->region->translateType(substructReference).c_str()

      additionsEdge->structPrototypesByInterfaceMethod.emplace_back(
          std::make_pair(newMethod, substructPrototype));

      auto functionL = declarer(substructPrototype);
      assert(globalState->extraFunctions.find(substructPrototype) != globalState->extraFunctions.end());

      substructPrototypes.push_back(substructPrototype);
    }
  }

  for (auto substructPrototype : substructPrototypes) {
    auto substruct =
        dynamic_cast<StructReferend*>(
            substructPrototype->params[newMethod->virtualParamIndex]->referend);
    assert(substruct);
    bodyGenerator(substruct, substructPrototype);
  }
}

void addExtraFunctions(GlobalState* globalState) {
  auto program = globalState->program;

  for (auto nameAndInterface : program->interfaces) {
    auto interfaceDefinition = nameAndInterface.second;
    globalState->interfaceExtraMethods.insert(
        std::make_pair(interfaceDefinition->referend, std::vector<InterfaceMethod *>{}));
  }

  for (auto nameAndStruct : program->structs) {
    auto struuct = nameAndStruct.second;
    for (auto edge : struuct->edges) {
      auto newExtraEdge = new Edge(edge->structName, edge->interfaceName, {});
      globalState->extraAdditionsEdges.insert(std::make_pair(edge, newExtraEdge));
    }
  }

  auto intMT = globalState->metalCache.intRef;
  auto intLT = globalState->getExternRegion(intMT)->translateType(intMT);

  addCalculateSerializedSizeFunctions(globalState);

  for (auto nameAndInterface : program->interfaces) {
    auto interfaceDefinition = nameAndInterface.second;
//    if (interfaceDefinition->mutability == Mutability::IMMUTABLE) {
//      auto serializeMethod =
//          new InterfaceMethod(
//              globalState->metalCache.getPrototype(
//                  globalState->serializeName,
//                  globalState->metalCache.emptyTupleStructRef,
//                  {
//                      globalState->metalCache.getReference(
//                          Ownership::SHARE, Location::YONDER, interfaceDefinition->referend)
//                  }),
//              0);
//      declareExtraInterfaceMethod(
//          globalState,
//          interfaceDefinition->referend,
//          serializeMethod,
//          [globalState](StructReferend* substruct, Prototype* substructPrototype) {
//            auto extraFunctionLT = globalState->lookupFunction(substructPrototype);
//            defineSerializeFunc(globalState, substruct, substructPrototype, extraFunctionLT);
//          });
//
//      auto unserializeMethod =
//          new InterfaceMethod(
//              globalState->metalCache.getPrototype(
//                  globalState->unserializeName,
//                  globalState->metalCache.emptyTupleStructRef,
//                  {
//                      globalState->metalCache.getReference(
//                          Ownership::SHARE, Location::YONDER, interfaceDefinition->referend)
//                  }),
//              0);
//      declareExtraInterfaceMethod(
//          globalState,
//          interfaceDefinition->referend,
//          unserializeMethod,
//          [globalState](StructReferend* substruct, Prototype* substructPrototype) {
//            auto extraFunctionLT = globalState->lookupFunction(substructPrototype);
//            defineDeserializeFunc(globalState, substruct, substructPrototype, extraFunctionLT);
//          });
//    }
  }
}
