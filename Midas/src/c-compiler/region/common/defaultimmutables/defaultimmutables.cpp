#include <function/expressions/shared/shared.h>
#include <utils/counters.h>
#include <utils/branch.h>
#include <region/common/controlblock.h>
#include <region/common/heap.h>
#include <function/expressions/shared/string.h>
#include <region/common/common.h>
#include <sstream>
#include "defaultimmutables.h"

ControlBlock makeImmControlBlock(GlobalState* globalState) {
  ControlBlock controlBlock(globalState, LLVMStructCreateNamed(globalState->context, "immControlBlock"));
  controlBlock.addMember(ControlBlockMember::STRONG_RC);
  // This is where we put the size in the current generational heap, we can use it for something
  // else until we get rid of that.
  controlBlock.addMember(ControlBlockMember::UNUSED_32B);
  if (globalState->opt->census) {
    controlBlock.addMember(ControlBlockMember::CENSUS_TYPE_STR);
    controlBlock.addMember(ControlBlockMember::CENSUS_OBJ_ID);
  }
  controlBlock.build();
  return controlBlock;
}

DefaultImmutables::DefaultImmutables(GlobalState* globalState_, ReferendStructs* wrappedStructs_)
  : globalState(globalState_),
    referendStructs(wrappedStructs_) {
  LLVMTypeRef structL = LLVMStructCreateNamed(globalState->context, "ValeStr");
  std::vector<LLVMTypeRef> memberTypesL;
  memberTypesL.push_back(LLVMInt64TypeInContext(globalState->context));
  memberTypesL.push_back(LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0));
  LLVMStructSetBody(structL, memberTypesL.data(), memberTypesL.size(), false);

  externalStructLByReferend.insert(
      std::pair<Referend*, LLVMTypeRef>(globalState->metalCache.str, structL));
}

void DefaultImmutables::discard(
    AreaAndFileAndLine from,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* sourceMT,
    Ref sourceRef) {
  auto sourceRnd = sourceMT->referend;

  if (dynamic_cast<Int *>(sourceRnd) ||
      dynamic_cast<Bool *>(sourceRnd) ||
      dynamic_cast<Float *>(sourceRnd)) {
    // Do nothing for these, they're always inlined and copied.
  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend *>(sourceRnd)) {
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      assert(false); // impl
    } else {
      auto rcLE =
          adjustStrongRc(
              from, globalState, functionState, referendStructs, builder, sourceRef, sourceMT, -1);
      buildIf(
          globalState, functionState,
          builder,
          isZeroLE(builder, rcLE),
          [globalState, functionState, sourceRef, interfaceRnd, sourceMT](
              LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceMT->referend);

            auto interfaceM = globalState->program->getInterface(interfaceRnd->fullName);
            int indexInEdge = -1;
            for (int i = 0; i < interfaceM->methods.size(); i++) {
              if (interfaceM->methods[i]->prototype == immDestructor) {
                indexInEdge = i;
              }
            }
            assert(indexInEdge >= 0);

            std::vector<Ref> argExprsL = {sourceRef};
            buildInterfaceCall(
                globalState, functionState, thenBuilder, immDestructor, argExprsL, 0, indexInEdge);
          });
    }
  } else if (dynamic_cast<StructReferend *>(sourceRnd) ||
      dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
      dynamic_cast<UnknownSizeArrayT *>(sourceRnd)) {
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      // Do nothing, we can just let inline structs disappear
    } else {
      auto rcLE =
          adjustStrongRc(
              from, globalState, functionState, referendStructs, builder, sourceRef, sourceMT, -1);
      buildIf(
          globalState, functionState,
          builder,
          isZeroLE(builder, rcLE),
          [from, globalState, functionState, sourceRef, sourceMT](LLVMBuilderRef thenBuilder) {
            auto immDestructor = globalState->program->getImmDestructor(sourceMT->referend);
            auto funcL = globalState->getFunction(immDestructor->name);

            auto sourceLE =
                globalState->region->checkValidReference(FL(),
                    functionState, thenBuilder, sourceMT, sourceRef);
            std::vector<LLVMValueRef> argExprsL = {sourceLE};
            return LLVMBuildCall(thenBuilder, funcL, argExprsL.data(), argExprsL.size(), "");
          });
    }
  } else if (dynamic_cast<Str *>(sourceRnd)) {
    assert(sourceMT->ownership == Ownership::SHARE);
    auto rcLE =
        adjustStrongRc(
            from, globalState, functionState, referendStructs, builder, sourceRef, sourceMT, -1);
    buildIf(
        globalState, functionState,
        builder,
        isZeroLE(builder, rcLE),
        [this, from, globalState, functionState, sourceRef, sourceMT](
            LLVMBuilderRef thenBuilder) {
          buildFlare(from, globalState, functionState, thenBuilder, "Freeing shared str!");
          innerDeallocate(from, globalState, functionState, referendStructs, thenBuilder, sourceMT, sourceRef);
        });
  } else {
    std::cerr << "Unimplemented type in discard: "
        << typeid(*sourceMT->referend).name() << std::endl;
    assert(false);
  }
}


LLVMTypeRef DefaultImmutables::translateType(GlobalState* globalState, Reference* referenceM) {
  if (primitives.isPrimitive(referenceM)) {
    return primitives.translatePrimitive(globalState, referenceM);
  } else {
    if (dynamic_cast<Str *>(referenceM->referend) != nullptr) {
      assert(referenceM->location != Location::INLINE);
      assert(referenceM->ownership == Ownership::SHARE);
      return LLVMPointerType(referendStructs->stringWrapperStructL, 0);
    } else if (auto knownSizeArrayMT = dynamic_cast<KnownSizeArrayT *>(referenceM->referend)) {
      assert(referenceM->location != Location::INLINE);
      auto knownSizeArrayCountedStructLT = referendStructs->getKnownSizeArrayWrapperStruct(knownSizeArrayMT);
      return LLVMPointerType(knownSizeArrayCountedStructLT, 0);
    } else if (auto unknownSizeArrayMT =
        dynamic_cast<UnknownSizeArrayT *>(referenceM->referend)) {
      assert(referenceM->location != Location::INLINE);
      auto unknownSizeArrayCountedStructLT =
          referendStructs->getUnknownSizeArrayWrapperStruct(unknownSizeArrayMT);
      return LLVMPointerType(unknownSizeArrayCountedStructLT, 0);
    } else if (auto structReferend =
        dynamic_cast<StructReferend *>(referenceM->referend)) {
      if (referenceM->location == Location::INLINE) {
        auto innerStructL = referendStructs->getInnerStruct(structReferend);
        return innerStructL;
      } else {
        auto countedStructL = referendStructs->getWrapperStruct(structReferend);
        return LLVMPointerType(countedStructL, 0);
      }
    } else if (auto interfaceReferend =
        dynamic_cast<InterfaceReferend *>(referenceM->referend)) {
      assert(referenceM->location != Location::INLINE);
      auto interfaceRefStructL =
          referendStructs->getInterfaceRefStruct(interfaceReferend);
      return interfaceRefStructL;
    } else if (dynamic_cast<Never*>(referenceM->referend)) {
      auto result = LLVMPointerType(makeNeverType(globalState), 0);
      assert(LLVMTypeOf(globalState->neverPtr) == result);
      return result;
    } else {
      std::cerr << "Unimplemented type: " << typeid(*referenceM->referend).name() << std::endl;
      assert(false);
      return nullptr;
    }
  }
}

//
//LLVMTypeRef DefaultImmutables::getControlBlockStruct(Referend* referend) {
//  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
//    auto structM = globalState->program->getStruct(structReferend->fullName);
//    assert(structM->mutability == Mutability::IMMUTABLE);
//  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(referend)) {
//    auto interfaceM = globalState->program->getInterface(interfaceReferend->fullName);
//    assert(interfaceM->mutability == Mutability::IMMUTABLE);
//  } else if (auto ksaMT = dynamic_cast<KnownSizeArrayT*>(referend)) {
//    assert(ksaMT->rawArray->mutability == Mutability::IMMUTABLE);
//  } else if (auto usaMT = dynamic_cast<UnknownSizeArrayT*>(referend)) {
//    assert(usaMT->rawArray->mutability == Mutability::IMMUTABLE);
//  } else if (auto strMT = dynamic_cast<Str*>(referend)) {
//  } else {
//    assert(false);
//  }
//  return referendStructs->controlBlock.getStruct();
//}

ControlBlock* DefaultImmutables::getControlBlock(Referend* referend) {
  if (auto structReferend = dynamic_cast<StructReferend*>(referend)) {
    auto structM = globalState->program->getStruct(structReferend->fullName);
    assert(structM->mutability == Mutability::IMMUTABLE);
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(referend)) {
    auto interfaceM = globalState->program->getInterface(interfaceReferend->fullName);
    assert(interfaceM->mutability == Mutability::IMMUTABLE);
  } else if (auto ksaMT = dynamic_cast<KnownSizeArrayT*>(referend)) {
    assert(ksaMT->rawArray->mutability == Mutability::IMMUTABLE);
  } else if (auto usaMT = dynamic_cast<UnknownSizeArrayT*>(referend)) {
    assert(usaMT->rawArray->mutability == Mutability::IMMUTABLE);
  } else if (auto strMT = dynamic_cast<Str*>(referend)) {
  } else {
    assert(false);
  }
  return &referendStructs->controlBlock;
}


LoadResult DefaultImmutables::loadMember(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* structRefMT,
    Ref structRef,
    int memberIndex,
    Reference* expectedMemberType,
    Reference* targetType,
    const std::string& memberName) {
  if (structRefMT->location == Location::INLINE) {
    auto innerStructLE =
        globalState->region->checkValidReference(
            FL(), functionState, builder, structRefMT, structRef);
    auto memberLE =
        LLVMBuildExtractValue(builder, innerStructLE, memberIndex, memberName.c_str());
    return LoadResult{wrap(functionState->defaultRegion, expectedMemberType, memberLE)};
  } else {
    return regularLoadStrongMember(globalState, functionState, builder, referendStructs, structRefMT, structRef, memberIndex, expectedMemberType, targetType, memberName);
  }
}

void DefaultImmutables::checkValidReference(
    AreaAndFileAndLine checkerAFL,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    IReferendStructsSource* referendStructs,
    Reference* refM,
    LLVMValueRef refLE) {
  regularCheckValidReference(checkerAFL, globalState, functionState, builder, referendStructs, refM, refLE);
}

std::string DefaultImmutables::getRefNameC(Reference* sourceMT) {
  auto sourceRnd = sourceMT->referend;
  if (dynamic_cast<Int *>(sourceRnd)) {
    return "int64_t";
  } else if (dynamic_cast<Bool *>(sourceRnd)) {
    return "int8_t";
  } else if (dynamic_cast<Float *>(sourceRnd)) {
    return "double";
  } else if (dynamic_cast<Str *>(sourceRnd)) {
    return "ValeStr*";
  } else if (auto interfaceRnd = dynamic_cast<InterfaceReferend *>(sourceRnd)) {
    auto baseName = globalState->program->getExportedName(interfaceRnd->fullName);
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      return baseName + "Inl";
    } else {
      return baseName + "Ref";
    }
  } else if (sourceRnd == globalState->metalCache.emptyTupleStruct) {
    return "void";
  } else if (auto structRnd = dynamic_cast<StructReferend *>(sourceRnd)) {
    auto baseName = globalState->program->getExportedName(structRnd->fullName);
    assert(sourceMT->ownership == Ownership::SHARE);
    if (sourceMT->location == Location::INLINE) {
      return baseName + "Inl";
    } else {
      return baseName + "Ref";
    }
  } else if (dynamic_cast<KnownSizeArrayT *>(sourceRnd) ||
             dynamic_cast<UnknownSizeArrayT *>(sourceRnd)) {
    assert(false); // impl
  } else {
    std::cerr << "Unimplemented type in immutables' getRefNameC: "
              << typeid(*sourceMT->referend).name() << std::endl;
    assert(false);
  }
}

void DefaultImmutables::generateStructDefsC(std::unordered_map<std::string, std::string>* cByExportedName, StructDefinition* structDefM) {
  auto name = globalState->program->getExportedName(structDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused; } " << name << ";" << std::endl;

  // For inlines
  s << "typedef struct " << name << "Inl {";
  for (int i = 0; i < structDefM->members.size(); i++) {
    auto member = structDefM->members[i];
    s << getRefNameC(member->type) << " unused" << i << ";";
  }
  s << " } " + name + ";" << std::endl;

  cByExportedName->insert(std::make_pair(name, s.str()));
}

void DefaultImmutables::generateInterfaceDefsC(std::unordered_map<std::string, std::string>* cByExportedName, InterfaceDefinition* interfaceDefM) {
  auto name = globalState->program->getExportedName(interfaceDefM->referend->fullName);
  std::stringstream s;
  s << "typedef struct " << name << "Ref { void* unused1; void* unused2; } " << name << ";";
  cByExportedName->insert(std::make_pair(name, s.str()));
}

LLVMTypeRef DefaultImmutables::getExternalType(
    Reference* refMT) {
  assert(refMT->ownership == Ownership::SHARE);

  if (refMT == globalState->metalCache.intRef) {
    return LLVMInt64TypeInContext(globalState->context);
  } else if (refMT == globalState->metalCache.boolRef) {
    return LLVMInt8TypeInContext(globalState->context);
  } else if (refMT == globalState->metalCache.floatRef) {
    return LLVMDoubleTypeInContext(globalState->context);
  } else if (refMT == globalState->metalCache.strRef) {
    auto structLIter = externalStructLByReferend.find(globalState->metalCache.str);
    assert(structLIter != externalStructLByReferend.end());
    auto structL = structLIter->second;
    return LLVMPointerType(structL, 0);
  } else if (refMT == globalState->metalCache.neverRef) {
    assert(false); // How can we hand a never into something?
    return nullptr;
  } else if (refMT == globalState->metalCache.emptyTupleStructRef) {
    return LLVMVoidTypeInContext(globalState->context);
  } else if (auto usa = dynamic_cast<UnknownSizeArrayT*>(refMT->referend)) {
    auto structLIter = externalStructLByReferend.find(globalState->metalCache.str);
    assert(structLIter != externalStructLByReferend.end());
    auto structL = structLIter->second;
    return LLVMPointerType(structL, 0);
  } else if (auto structReferend = dynamic_cast<StructReferend*>(refMT->referend)) {
    assert(false); // impl
    return nullptr;
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(refMT->referend)) {
    assert(false); // impl
    return nullptr;
  } else {
    std::cerr << "Invalid type for extern!" << std::endl;
    assert(false);
    return nullptr;
  }

  assert(false);
  return nullptr;
}

Ref DefaultImmutables::copyAlien(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    IRegion* sourceRegion,
    IRegion* targetRegion,
    Reference* sourceRefMT,
    Ref sourceReff) {
  assert(sourceRefMT->ownership == Ownership::SHARE);
  // Someday when we include the region in the coord, this line will change.
  auto targetRefMT = sourceRefMT;

  auto sourceLE = sourceRegion->checkValidReference(FL(), functionState, builder, sourceRefMT, sourceReff);

  if (sourceRefMT == globalState->metalCache.intRef) {
    return wrap(targetRegion, targetRefMT, sourceLE);
  } else if (sourceRefMT == globalState->metalCache.boolRef) {
    return wrap(targetRegion, targetRefMT, sourceLE);
  } else if (sourceRefMT == globalState->metalCache.floatRef) {
    return wrap(targetRegion, targetRefMT, sourceLE);
  } else if (sourceRefMT == globalState->metalCache.strRef) {
//    auto structLIter = externalStructLByReferend.find(globalState->metalCache.str);
//    assert(structLIter != externalStructLByReferend.end());
//    auto structL = structLIter->second;

    auto sourceStrBytesPtrLE = sourceRegion->getStringBytesPtr(functionState, builder, sourceReff);
    auto sourceStrLenLE = sourceRegion->getStringLen(functionState, builder, sourceReff);

    auto targetStrLE = mallocStr(functionState, builder, sourceStrLenLE);
    auto targetStrBytesPtrLE = referendStructs->getStringBytesPtr(functionState, builder, targetStrLE);

    std::vector<LLVMValueRef> strncpyArgs = {
        targetStrBytesPtrLE,
        sourceStrBytesPtrLE,
        sourceStrLenLE
    };
    LLVMBuildCall(builder, globalState->strncpy, strncpyArgs.data(), strncpyArgs.size(), "");

    return wrap(targetRegion, targetRefMT, targetStrLE);
  } else if (sourceRefMT == globalState->metalCache.neverRef) {
    assert(false); // How can we hand a never into something?
    auto targetLE = makeEmptyTuple(globalState, functionState, builder);
    return wrap(targetRegion, targetRefMT, targetLE);
  } else if (sourceRefMT == globalState->metalCache.emptyTupleStructRef) {
    assert(false); // How can we hand a void into something?
    auto targetLE = makeEmptyTuple(globalState, functionState, builder);
    return wrap(targetRegion, targetRefMT, targetLE);
  } else if (auto structReferend = dynamic_cast<StructReferend*>(sourceRefMT->referend)) {
    assert(false); // impl
    auto targetLE = makeEmptyTuple(globalState, functionState, builder);
    return wrap(targetRegion, targetRefMT, targetLE);

//    if (sourceRefMT->location == Location::INLINE) {
//      functionState->defaultRegion->checkInlineStructType(functionState, builder, sourceRefMT, ref);
//    } else {
////            std::cerr << "Can only pass inline imm structs between C and Vale currently." << std::endl;
//      assert(false); // impl
//      return nullptr;
//    }
  } else if (auto interfaceReferend = dynamic_cast<InterfaceReferend*>(sourceRefMT->referend)) {

    assert(false); // impl
    auto targetLE = makeEmptyTuple(globalState, functionState, builder);
    return wrap(targetRegion, targetRefMT, targetLE);
  } else {
    std::cerr << "Invalid type for extern!" << std::endl;
    assert(false);
    auto targetLE = makeEmptyTuple(globalState, functionState, builder);
    return wrap(targetRegion, targetRefMT, targetLE);
  }

  assert(false);
}

void DefaultImmutables::fillControlBlock(
    AreaAndFileAndLine from,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Referend* referendM,
    ControlBlockPtrLE controlBlockPtrLE,
    const std::string& typeName) {
  LLVMValueRef newControlBlockLE = LLVMGetUndef(referendStructs->getControlBlock(referendM)->getStruct());
  newControlBlockLE =
      fillControlBlockCensusFields(
          from, globalState, functionState, referendStructs, builder, referendM, newControlBlockLE, typeName);
  newControlBlockLE =
      insertStrongRc(globalState, builder, referendStructs, referendM, newControlBlockLE);
  LLVMBuildStore(
      builder,
      newControlBlockLE,
      controlBlockPtrLE.refLE);
}

WrapperPtrLE DefaultImmutables::mallocStr(
    FunctionState* functionState,
    LLVMBuilderRef builder,
    LLVMValueRef lengthLE) {
  // The +1 is for the null terminator at the end, for C compatibility.
  auto sizeBytesLE =
      LLVMBuildAdd(
          builder,
          lengthLE,
          LLVMBuildAdd(
              builder,
              constI64LE(globalState, 1),
              constI64LE(globalState, LLVMABISizeOfType(globalState->dataLayout, referendStructs->getStringWrapperStruct())),
              "lenPlus1"),
          "strMallocSizeBytes");

  auto destCharPtrLE = callMalloc(globalState, builder, LLVMBuildZExt(builder, sizeBytesLE, LLVMInt64TypeInContext(globalState->context), "lenPlus1As64"));

  if (globalState->opt->census) {
    adjustCounter(globalState, builder, globalState->liveHeapObjCounter, 1);

    LLVMValueRef resultAsVoidPtrLE =
        LLVMBuildBitCast(
            builder, destCharPtrLE, LLVMPointerType(LLVMInt8TypeInContext(globalState->context), 0), "");
    LLVMBuildCall(builder, globalState->censusAdd, &resultAsVoidPtrLE, 1, "");
  }

  auto newStrWrapperPtrLE =
      referendStructs->makeWrapperPtr(
          FL(), functionState, builder, globalState->metalCache.strRef,
          LLVMBuildBitCast(
              builder,
              destCharPtrLE,
              LLVMPointerType(referendStructs->getStringWrapperStruct(), 0),
              "newStrWrapperPtr"));

  fillControlBlock(
      FL(),
      functionState,
      builder,
      globalState->metalCache.str,
      referendStructs->getConcreteControlBlockPtr(FL(), functionState, builder, globalState->metalCache.strRef, newStrWrapperPtrLE),
      "str");
  LLVMBuildStore(builder, LLVMBuildZExt(builder, lengthLE, LLVMInt64TypeInContext(globalState->context), ""), getLenPtrFromStrWrapperPtr(builder, newStrWrapperPtrLE));

  // Set the null terminating character to the 0th spot and the end spot, just to guard against bugs
  auto charsBeginPtr = getCharsPtrFromWrapperPtr(globalState, builder, newStrWrapperPtrLE);
  LLVMBuildStore(builder, constI8LE(globalState, 0), charsBeginPtr);
  auto charsEndPtr = LLVMBuildGEP(builder, charsBeginPtr, &lengthLE, 1, "charsEndPtr");
  LLVMBuildStore(builder, constI8LE(globalState, 0), charsEndPtr);

  // The caller still needs to initialize the actual chars inside!

  return newStrWrapperPtrLE;
}
