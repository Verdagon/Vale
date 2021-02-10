#include <region/common/heap.h>
#include <function/expressions/shared/members.h>
#include <function/expressions/expressions.h>
#include "region/iregion.h"
#include "region/linear/linear.h"
#include "serialize.h"
#include "midasfunctions.h"

Ref roundUpToMultipleOf16(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref n) {
  // See https://math.stackexchange.com/a/291481
  return globalState->buildMultiply(functionState, builder,
      globalState->buildAdd(functionState, builder,
          globalState->buildDivide(functionState, builder,
              globalState->buildAdd(functionState, builder,
                  n,
                  globalState->constI64(-1)),
              globalState->constI64(0x10)),
          globalState->constI64(1)),
      globalState->constI64(0x10));
}

Ref addTrailingPadding(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Ref size) {
  // Round it out to a multiple of 16, because 16 is the lowest safe number to put the next struct at.
  // This can be better; from https://stackoverflow.com/a/38144117: "Before each individual member, there will be
  // padding so that to make it start at an address that is divisible by its size."
  return roundUpToMultipleOf16(globalState, functionState, builder, size);
}

// This should NOT be called on anything inline, because it adds padding at the end.
Ref getCalculatedSerializedSize(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* refMT,
    Ref ref) {
  auto intMT = globalState->metalCache.intRef;
  if (refMT == globalState->metalCache.intRef) {
    return globalState->constI64(LLVMABISizeOfType(globalState->dataLayout, LLVMInt64TypeInContext(globalState->context)));
  } else if (refMT == globalState->metalCache.boolRef) {
    return globalState->constI64(LLVMABISizeOfType(globalState->dataLayout, LLVMInt1TypeInContext(globalState->context)));
  } else if (refMT == globalState->metalCache.floatRef) {
    return globalState->constI64(LLVMABISizeOfType(globalState->dataLayout, LLVMFloatTypeInContext(globalState->context)));
  } else if (refMT == globalState->metalCache.strRef) {
    auto prototype = globalState->metalCache.getPrototype(globalState->calculateSerializedSizeName, intMT, {refMT});
    auto unpaddedSize = buildCall(globalState, functionState, builder, prototype, {ref});
    return addTrailingPadding(globalState, functionState, builder, unpaddedSize);
  } else if (auto structReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
    if (refMT->location == Location::INLINE) {
      if (refMT == globalState->metalCache.emptyTupleStructRef) {
        // Return immediately, dont add padding.
        return globalState->constI64(0);
      } else {
        assert(false); // impl
      }
    } else {
      auto prototype = globalState->metalCache.getPrototype(globalState->calculateSerializedSizeName, intMT, {refMT});
      auto unpaddedSize = buildCall(globalState, functionState, builder, prototype, {ref});
      return addTrailingPadding(globalState, functionState, builder, unpaddedSize);
    }
  } else if (auto interfaceReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
    auto prototype = globalState->metalCache.getPrototype(globalState->calculateSerializedSizeName, intMT, { refMT });
    auto unpaddedSize = buildInterfaceCall(globalState, functionState, builder, prototype, { ref }, 0);
    return addTrailingPadding(globalState, functionState, builder, unpaddedSize);
  } else assert(false);
}

void defineCalculateSerializedSizeFunc(
    GlobalState* globalState,
    StructReferend* substruct,
    Prototype* substructPrototype,
    LLVMValueRef functionL) {
  auto intMT = globalState->metalCache.intRef;
  auto nameM = globalState->calculateSerializedSizeName;

  LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(globalState->context, functionL, "entry");
  LLVMBuilderRef builder = LLVMCreateBuilderInContext(globalState->context);
  LLVMPositionBuilderAtEnd(builder, block);
  // This is unusual because normally we have a separate localsBuilder which points to a separate
  // block at the beginning. This is a simple function which should require no locals, so this
  // should be fine.
  LLVMBuilderRef localsBuilder = builder;

  FunctionState functionState(
      globalState->serializeName->name,
      functionL,
      LLVMGetReturnType(LLVMTypeOf(functionL)),
      localsBuilder);

  auto structRefM = substructPrototype->params[0];
//  auto region = globalState->linearRegion;
//  assert(region == globalState->getExternRegion(structRefM));
  auto structRefLE = LLVMGetParam(functionL, 0);
  auto structRef = wrap(globalState->getRegion(structRefM), structRefM, structRefLE);

  auto structDefM = globalState->program->getStruct(substruct->fullName);
  Ref substructSize =
       globalState->linearRegion->predictShallowSize(
           &functionState, builder, structRefM->referend, globalState->constI64(0));
  Ref totalSizeRef = substructSize;

  for (int i = 0; i < structDefM->members.size(); i++) {
    auto memberM = structDefM->members[i];
    auto memberRef =
        loadMember(FL(), globalState, &functionState, builder, structRefM, structRef, true, Mutability::IMMUTABLE, memberM->type, i, memberM->type, memberM->name);
    auto memberSizeIntRef =
        getCalculatedSerializedSize(globalState, &functionState, builder, memberM->type, memberRef);
    totalSizeRef = globalState->buildAdd(&functionState, builder, totalSizeRef, memberSizeIntRef);
  }

  LLVMBuildRet(
      builder,
      checkValidInternalReference(FL(), globalState, &functionState, builder, intMT, totalSizeRef));

  LLVMDisposeBuilder(builder);
}

void addCalculateSerializedSizeFunctions(GlobalState* globalState) {
  auto program = globalState->program;

  auto intMT = globalState->metalCache.intRef;
  auto intLT = globalState->getExternRegion(intMT)->translateType(intMT);

  // The actual LLVM name will be different, disambiguated.
  auto nameM = globalState->calculateSerializedSizeName;

  std::vector<Prototype*> prototypes;

  {
    auto strRefMT = globalState->metalCache.strRef;
    auto strStructL = globalState->getRegion(strRefMT)->translateType(strRefMT);
    auto prototype = globalState->metalCache.getPrototype(nameM, intMT, {strRefMT});
    std::vector<LLVMTypeRef> params = {strStructL};
    auto functionLT = LLVMFunctionType(intLT, params.data(), params.size(), false);
    auto nameL = std::string("__calculateSerializedSize_str");
    auto functionL = LLVMAddFunction(globalState->mod, prototype->name->name.c_str(), functionLT);
    // Don't define it yet, we're just declaring them right now.
    globalState->extraFunctions.emplace(std::make_pair(prototype, functionL));
    prototypes.push_back(prototype);


    LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(globalState->context, functionL, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilderInContext(globalState->context);
    LLVMPositionBuilderAtEnd(builder, block);
    // This is unusual because normally we have a separate localsBuilder which points to a separate
    // block at the beginning. This is a simple function which should require no locals, so this
    // should be fine.
    LLVMBuilderRef localsBuilder = builder;

    FunctionState functionState(
        globalState->serializeName->name,
        functionL,
        LLVMGetReturnType(LLVMTypeOf(functionL)),
        localsBuilder);

    auto strRef =
        wrap(globalState->getRegion(globalState->metalCache.strRef), globalState->metalCache.strRef, LLVMGetParam(functionL, 0));
    auto strSizeIntRef =
        wrap(globalState->getRegion(globalState->metalCache.intRef), globalState->metalCache.intRef,
             globalState->getRegion(globalState->metalCache.strRef)->getStringLen(&functionState, builder, strRef));
    Ref substructSize =
        globalState->linearRegion->predictShallowSize(
            &functionState, builder, globalState->metalCache.str, strSizeIntRef);
    Ref totalSizeRef = substructSize;

    LLVMBuildRet(
        builder,
        checkValidInternalReference(FL(), globalState, &functionState, builder, intMT, totalSizeRef));

    LLVMDisposeBuilder(builder);
  }

  for (auto nameAndStruct : program->structs) {
    auto structDefinition = nameAndStruct.second;
    if (structDefinition->mutability == Mutability::IMMUTABLE) {
      auto structRefMT =
          globalState->metalCache.getReference(
              Ownership::SHARE, Location::YONDER, structDefinition->referend);
      auto structRefLT = globalState->getRegion(structRefMT)->translateType(structRefMT);
      auto prototype = globalState->metalCache.getPrototype(nameM, intMT, {structRefMT});
      std::vector<LLVMTypeRef> params = {structRefLT};
      auto functionLT = LLVMFunctionType(intLT, params.data(), params.size(), false);
      auto nameL = std::string("__calculateSerializedSize_") + structDefinition->name->name;
      auto functionL = LLVMAddFunction(globalState->mod, prototype->name->name.c_str(), functionLT);
      // Don't define it yet, we're just declaring them right now.
      globalState->extraFunctions.emplace(std::make_pair(prototype, functionL));
      prototypes.push_back(prototype);
    }
  }

  for (auto nameAndInterface : program->interfaces) {
    auto interfaceDefinition = nameAndInterface.second;
    if (interfaceDefinition->mutability == Mutability::IMMUTABLE) {
      auto interfaceRefMT =
          globalState->metalCache.getReference(
              Ownership::SHARE, Location::YONDER, interfaceDefinition->referend);
      auto interfacePrototype = globalState->metalCache.getPrototype(nameM, intMT, {interfaceRefMT});
      declareExtraInterfaceMethod(
          globalState,
          interfaceDefinition->referend,
          new InterfaceMethod(interfacePrototype, 0),
          [globalState](Prototype *substructPrototype) { return globalState->lookupFunction(substructPrototype); },
          [](StructReferend *substruct, Prototype *substructPrototype) {});
    }
  }

  for (auto prototype : prototypes) {
    if (auto referend = dynamic_cast<StructReferend*>(prototype->params[0]->referend)) {
      auto functionL = globalState->lookupFunction(prototype);
      defineCalculateSerializedSizeFunc(globalState, referend, prototype, functionL);
    }
  }
}

void defineSerializeFunc(
    GlobalState* globalState,
    StructReferend* substruct,
    Prototype* substructPrototype,
    LLVMValueRef functionL) {
  assert(false);
}

void defineDeserializeFunc(
    GlobalState* globalState,
    StructReferend* substruct,
    Prototype* substructPrototype,
    LLVMValueRef extraFunctionLT) {
  assert(false); // add body here
}
