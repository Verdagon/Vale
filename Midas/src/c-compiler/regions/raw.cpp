//#include <translatetype.h>
//#include <regions/shared/construct.h>
//#include "function/expressions/shared/shared.h"
//#include "function/expressions/shared/members.h"
//#include "function/expressions/shared/branch.h"
//#include "regions/shared/heap.h"
//#include "function/expressions/shared/controlblock.h"
//#include "raw.h"
//
//RawRegion::RawRegion() {
//  auto voidLT = LLVMVoidType();
//  auto voidPtrLT = LLVMPointerType(voidLT, 0);
//  auto int1LT = LLVMInt1Type();
//  auto int8LT = LLVMInt8Type();
//  auto int64LT = LLVMInt64Type();
//  auto int8PtrLT = LLVMPointerType(int8LT, 0);
//  auto int64PtrLT = LLVMPointerType(int64LT, 0);
//
//  {
//    auto controlBlockStructL =
//        LLVMStructCreateNamed(
//            LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
//    std::vector<LLVMTypeRef> memberTypesL;
//
//    controlBlockTypeStrIndex = memberTypesL.size();
//    memberTypesL.push_back(int8PtrLT);
//
//    controlBlockObjIdIndex = memberTypesL.size();
//    memberTypesL.push_back(int64LT);
//
//    controlBlockRcMemberIndex = memberTypesL.size();
//    memberTypesL.push_back(int64LT);
//
//    controlBlockWrciMemberIndex = memberTypesL.size();
//    memberTypesL.push_back(int64LT);
//
//    LLVMStructSetBody(
//        controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
//    weakableControlBlockStructL = controlBlockStructL;
//  }
//
//  {
//    auto controlBlockStructL =
//        LLVMStructCreateNamed(
//            LLVMGetGlobalContext(), CONTROL_BLOCK_STRUCT_NAME);
//    std::vector<LLVMTypeRef> memberTypesL;
//
//    assert(memberTypesL.size() == controlBlockTypeStrIndex); // should match weakable
//    memberTypesL.push_back(int8PtrLT);
//
//    assert(memberTypesL.size() == controlBlockObjIdIndex); // should match weakable
//    memberTypesL.push_back(int64LT);
//
//    assert(memberTypesL.size() == controlBlockRcMemberIndex); // should match weakable
//    memberTypesL.push_back(int64LT);
//
//    LLVMStructSetBody(
//        controlBlockStructL, memberTypesL.data(), memberTypesL.size(), false);
//    nonWeakableControlBlockStructL = controlBlockStructL;
//  }
//}
//
//LLVMValueRef RawRegion::allocate(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* desiredReference,
//    const std::vector<LLVMValueRef>& membersLE) {
//  auto structReferend =
//      dynamic_cast<StructReferend*>(desiredReference->referend);
//  assert(structReferend);
//
//  auto structM = globalState->program->getStruct(structReferend->fullName);
//
//  switch (structM->mutability) {
//    case Mutability::MUTABLE: {
//      auto countedStructL = globalState->getCountedStruct(structReferend->fullName);
//      return constructCountedStruct(
//          from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
//    }
//    case Mutability::IMMUTABLE: {
//      if (desiredReference->location == Location::INLINE) {
//        auto valStructL =
//            globalState->getInnerStruct(structReferend->fullName);
//        return constructInnerStruct(
//            builder, structM, valStructL, membersLE);
//      } else {
//        auto countedStructL =
//            globalState->getCountedStruct(structReferend->fullName);
//        return constructCountedStruct(
//            from, globalState, functionState, builder, countedStructL, desiredReference, structM, membersLE);
//      }
//    }
//    default:
//      assert(false);
//      return nullptr;
//  }
//  assert(false);
//}
//
//std::vector<LLVMValueRef> RawRegion::destructure(
//    GlobalState* globalState,
//    FunctionState* functionState,
//    BlockState* blockState,
//    LLVMBuilderRef builder,
//    Reference* structType,
//    LLVMValueRef structLE) {
//
//  auto mutability = ownershipToMutability(structType->ownership);
//
//  auto structReferend =
//      dynamic_cast<StructReferend *>(structType->referend);
//  assert(structReferend);
//
//  auto structM = globalState->program->getStruct(structReferend->fullName);
//
//  if (structM->weakable) {
//    auto controlBlockPtrLE = getControlBlockPtr(builder, structLE, structType);
//    auto wrciLE = getWrciFromControlBlockPtr(builder, controlBlockPtrLE, controlBlockWrciMemberIndex);
//    LLVMBuildCall(builder, globalState->markWrcDead, &wrciLE, 1, "");
//  }
//
//  std::vector<LLVMValueRef> membersLE =
//      getMemberPtrsLE(globalState, functionState, builder, structM, structLE);
//
//  freeConcrete(
//      AFL("Destroy freeing"),
//      globalState,
//      functionState,
//      blockState,
//      builder,
//      structLE,
//      structType);
//
//  return membersLE;
//}
//
//LLVMValueRef RawRegion::alias(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* resultRef,
//    Ownership targetOwnership,
//    LLVMValueRef expr) {
//  if (resultRef->ownership == Ownership::SHARE) {
//    incrementStrongRc(from, globalState, functionState, builder, resultRef, expr, makeRcLayoutInfo());
//  } else if (resultRef->ownership == Ownership::OWN) {
//    // Do nothing
//  } else if (resultRef->ownership == Ownership::BORROW) {
//    // Do nothing
//  } else if (resultRef->ownership == Ownership::WEAK) {
//    incrementWeakRc(from, globalState, functionState, builder, resultRef, expr);
//  } else {
//    // Do nothing!
//  }
//}
//
//void RawRegion::dealias(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    BlockState* blockState,
//    LLVMBuilderRef builder,
//    Reference* sourceRef,
//    LLVMValueRef expr) {
//  auto sourceRnd = sourceRef->referend;
//
//  if (dynamic_cast<Int*>(sourceRnd) ||
//      dynamic_cast<Bool*>(sourceRnd) ||
//      dynamic_cast<Float*>(sourceRnd)) {
//    // Do nothing for these, they're always inlined and copied.
//  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend*>(sourceRnd)) {
//    if (sourceRef->ownership == Ownership::SHARE) {
//      if (sourceRef->location == Location::INLINE) {
//        assert(false); // impl
//      } else {
//        auto rcLE =
//            adjustStrongRc(
//                from, globalState, functionState, builder, expr, sourceRef, -1, makeRcLayoutInfo());
//        buildIf(
//            functionState,
//            builder,
//            isZeroLE(builder, rcLE),
//            [globalState, expr, interfaceRnd, sourceRef](LLVMBuilderRef thenBuilder) {
//              auto immDestructor = globalState->program->getImmDestructor(sourceRef->referend);
//
//              auto interfaceM = globalState->program->getInterface(interfaceRnd->fullName);
//              int indexInEdge = -1;
//              for (int i = 0; i < interfaceM->methods.size(); i++) {
//                if (interfaceM->methods[i]->prototype == immDestructor) {
//                  indexInEdge = i;
//                }
//              }
//              assert(indexInEdge >= 0);
//
//              std::vector<LLVMValueRef> argExprsL = { expr };
//              buildInterfaceCall(thenBuilder, argExprsL, 0, indexInEdge);
//            });
//      }
//    }
//  } else if (dynamic_cast<StructReferend*>(sourceRnd) ||
//      dynamic_cast<KnownSizeArrayT*>(sourceRnd) ||
//      dynamic_cast<UnknownSizeArrayT*>(sourceRnd)) {
//    if (sourceRef->ownership == Ownership::SHARE) {
//      if (sourceRef->location == Location::INLINE) {
//        // Do nothing, we can just let inline structs disappear
//      } else {
//        auto rcLE = adjustStrongRc(from, globalState, functionState, builder, expr, sourceRef, -1, makeRcLayoutInfo());
//        buildIf(
//            functionState,
//            builder,
//            isZeroLE(builder, rcLE),
//            [from, globalState, functionState, expr, sourceRef](LLVMBuilderRef thenBuilder) {
//              auto immDestructor = globalState->program->getImmDestructor(sourceRef->referend);
//              auto funcL = globalState->getFunction(immDestructor->name);
//              std::vector<LLVMValueRef> argExprsL = { expr };
//              return LLVMBuildCall(thenBuilder, funcL, argExprsL.data(), argExprsL.size(), "");
//            });
//      }
//    } else assert(false);
//  } else if (dynamic_cast<Str*>(sourceRnd)) {
//    assert(sourceRef->ownership == Ownership::SHARE);
//    auto rcLE = adjustStrongRc(from, globalState, functionState, builder, expr, sourceRef, -1, makeRcLayoutInfo());
//    buildIf(
//        functionState,
//        builder,
//        isZeroLE(builder, rcLE),
//        [from, globalState, functionState, blockState, expr, sourceRef](LLVMBuilderRef thenBuilder) {
//          freeConcrete(from, globalState, functionState, blockState, thenBuilder, expr, sourceRef);
//        });
//  } else {
//    std::cerr << "Unimplemented type in discard: "
//        << typeid(*sourceRef->referend).name() << std::endl;
//    assert(false);
//  }
//}
//
//LLVMValueRef RawRegion::loadMember(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    LLVMBuilderRef builder,
//    Reference* structRefM,
//    LLVMValueRef structExpr,
//    Mutability mutability,
//    Reference* memberType,
//    int memberIndex,
//    const std::string& memberName) {
//  if (mutability == Mutability::IMMUTABLE) {
//    if (structRefM->location == Location::INLINE) {
//      return LLVMBuildExtractValue(
//          builder, structExpr, memberIndex, memberName.c_str());
//    } else {
//      LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder, structExpr);
//      auto memberPtrLE =
//          LLVMBuildStructGEP(
//              builder, innerStructPtrLE, memberIndex, memberName.c_str());
//      auto sourceRefLE =
//          LLVMBuildLoad(
//              builder,
//              memberPtrLE,
//              memberName.c_str());
//      return alias(from, globalState, functionState, builder, memberType, Ownership::BORROW, sourceRefLE);
//    }
//  } else if (mutability == Mutability::MUTABLE) {
//    LLVMValueRef innerStructPtrLE = getStructContentsPtr(builder, structExpr);
//    auto memberPtrLE =
//        LLVMBuildStructGEP(
//            builder, innerStructPtrLE, memberIndex, memberName.c_str());
//    auto sourceRefLE =
//        LLVMBuildLoad(
//            builder,
//            memberPtrLE,
//            memberName.c_str());
//    return alias(from, globalState, functionState, builder, memberType, Ownership::BORROW, sourceRefLE);
//  } else {
//    assert(false);
//    return nullptr;
//  }
//}
//
//LLVMValueRef RawRegion::storeMember(
//    AreaAndFileAndLine from,
//    GlobalState* globalState,
//    FunctionState* functionState,
//    BlockState* blockState,
//    LLVMBuilderRef builder,
//    Reference* structRefM,
//    LLVMValueRef structExpr,
//    Mutability mutability,
//    Reference* memberType,
//    int memberIndex,
//    const std::string& memberName,
//    LLVMValueRef sourceLE) {
//
//  assert(mutability == Mutability::MUTABLE);
//  if (structRefM->ownership == Ownership::BORROW) {
//    adjustCounter(builder, globalState->mutDerefCounter, 1);
//  }
//
//  auto oldMemberLE =
//      swapMember(
//          builder, structExpr, memberIndex, memberName, sourceLE);
//  checkValidReference(from, globalState, functionState, builder, memberType, structExpr);
//  functionState->defaultRegion->dealias(
//      AFL("MemberStore discard struct"), globalState, functionState, blockState, builder,
//      structRefM, structExpr);
//
//  return oldMemberLE;
//}
//
//LLVMTypeRef RawRegion::getControlBlockStructForStruct(StructDefinition* structM) {
//  if (structM->weakable) {
//    return weakableControlBlockStructL;
//  } else {
//    return nonWeakableControlBlockStructL;
//  }
//}
//
//LLVMTypeRef RawRegion::getControlBlockStructForInterface(InterfaceDefinition* interfaceM) {
//  if (interfaceM->weakable) {
//    return weakableControlBlockStructL;
//  } else {
//    return nonWeakableControlBlockStructL;
//  }
//}
//
//LLVMTypeRef RawRegion::getControlBlockStructForKnownSizeArray(KnownSizeArrayT* arrMT) {
//  return nonWeakableControlBlockStructL;
//}
//
//LLVMTypeRef RawRegion::getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) {
//  return nonWeakableControlBlockStructL;
//}
