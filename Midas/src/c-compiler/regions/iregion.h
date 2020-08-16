#ifndef REGION_IREGION_H_
#define REGION_IREGION_H_

#include <llvm-c/Core.h>
#include <function/expressions/shared/afl.h>
#include "globalstate.h"
#include "function/functionstate.h"

class FunctionState;
class BlockState;
class GlobalState;

class IRegion {
public:
  virtual ~IRegion() = default;

  virtual LLVMValueRef allocate(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* desiredReference,
      const std::vector<LLVMValueRef>& membersLE) = 0;

  virtual LLVMValueRef alias(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* sourceRef,
      Ownership targetOwnership,
      LLVMValueRef expr) = 0;

  virtual void dealias(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* sourceRef,
      LLVMValueRef expr) = 0;

  virtual LLVMValueRef loadMember(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* structRefM,
      LLVMValueRef structExpr,
      Mutability mutability,
      Reference* memberType,
      int memberIndex,
      const std::string& memberName) = 0;

  virtual LLVMValueRef storeMember(
      AreaAndFileAndLine from,
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* structRefM,
      LLVMValueRef structExpr,
      Mutability mutability,
      Reference* memberType,
      int memberIndex,
      const std::string& memberName,
      LLVMValueRef sourceLE) = 0;

  virtual std::vector<LLVMValueRef> destructure(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* structType,
      LLVMValueRef structLE) = 0;

  virtual LLVMTypeRef getControlBlockStructForInterface(InterfaceDefinition* interfaceM) = 0;
  virtual LLVMTypeRef getControlBlockStructForStruct(StructDefinition* structM) = 0;
  virtual LLVMTypeRef getControlBlockStructForKnownSizeArray(KnownSizeArrayT* arrMT) = 0;
  virtual LLVMTypeRef getControlBlockStructForUnknownSizeArray(UnknownSizeArrayT* arrMT) = 0;

  // Returns a LLVMValueRef for a ref to the string object.
  // The caller should then use getStringBytesPtr to then fill the string's contents.
  virtual LLVMValueRef constructString(LLVMValueRef sizeLE) = 0;

  // Returns a LLVMValueRef for a pointer to the strings contents bytes
  virtual LLVMValueRef getStringBytesPtr(LLVMValueRef stringRefLE) = 0;

  virtual LLVMValueRef getStringLength(LLVMValueRef stringRefLE) = 0;



  // Returns a LLVMValueRef for a ref to the string object.
  // The caller should then use getStringBytesPtr to then fill the string's contents.
  virtual LLVMValueRef constructKnownSizeArray(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* structTypeM,
      LLVMTypeRef structLT,
      const std::vector<LLVMValueRef>& membersLE,
      const std::string& typeName);


  // Returns a LLVMValueRef for a ref to the string object.
  // The caller should then use getStringBytesPtr to then fill the string's contents.
  virtual LLVMValueRef constructUnknownSizeArray(
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      LLVMTypeRef usaWrapperPtrLT,
      LLVMTypeRef usaElementLT,
      LLVMValueRef sizeLE,
      const std::string& typeName) = 0;

  virtual LLVMValueRef getKnownSizeArrayElementsPtr(
      LLVMBuilderRef builder, LLVMValueRef knownSizeArrayWrapperPtrLE) = 0;
  virtual LLVMValueRef getUnknownSizeArrayElementsPtr(
      LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) = 0;
  virtual LLVMValueRef getUnknownSizeArrayLength(
      LLVMBuilderRef builder, LLVMValueRef unknownSizeArrayWrapperPtrLE) = 0;

  virtual void destroyArray(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* arrayType,
      LLVMValueRef arrayWrapperLE) = 0;


  virtual void checkValidReference(
      AreaAndFileAndLine checkerAFL,
      GlobalState* globalState,
      FunctionState* functionState,
      LLVMBuilderRef builder,
      Reference* refM,
      LLVMValueRef refLE) = 0;

  virtual LLVMValueRef loadElement(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* structRefM,
      Reference* elementRefM,
      LLVMValueRef sizeLE,
      LLVMValueRef arrayPtrLE,
      Mutability mutability,
      LLVMValueRef indexLE) = 0;

  virtual LLVMValueRef storeElement(
      GlobalState* globalState,
      FunctionState* functionState,
      BlockState* blockState,
      LLVMBuilderRef builder,
      Reference* arrayRefM,
      Reference* elementRefM,
      LLVMValueRef sizeLE,
      LLVMValueRef arrayPtrLE,
      Mutability mutability,
      LLVMValueRef indexLE,
      LLVMValueRef sourceLE) = 0;

//  LLVMValueRef initStr, addStr, eqStr, printVStr;
};

#endif
