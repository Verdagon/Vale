#ifndef FUNCTION_EXPRESSIONS_SHARED_REF_H_
#define FUNCTION_EXPRESSIONS_SHARED_REF_H_

#include <llvm-c/Core.h>
#include "../../../metal/types.h"
#include "afl.h"

class FunctionState;
class GlobalState;
class IRegion;

// A type-system token to assure certain functions that we indeed checked the bounds of an array
// we're about to access.
struct InBoundsLE {
  LLVMValueRef refLE;
};

struct WrapperPtrLE {
  Reference* const refM;
  // TODO rename to ptrLE
  LLVMValueRef const refLE;

  WrapperPtrLE(Reference* refM_, LLVMValueRef refLE_)
      : refM(refM_), refLE(refLE_) { }
};


struct ControlBlockPtrLE {
  Kind* const kindM;
  // TODO rename to ptrLE
  LLVMValueRef const refLE;

  ControlBlockPtrLE(Kind* refM_, LLVMValueRef refLE_)
    : kindM(refM_), refLE(refLE_) { }
};



struct InterfaceFatPtrLE {
  Reference* const refM;
  // TODO rename to ptrLE
  LLVMValueRef const refLE;

  InterfaceFatPtrLE(Reference* refM_, LLVMValueRef refLE_)
      : refM(refM_), refLE(refLE_) { }
};


struct WeakFatPtrLE {
  Reference* const refM;
  // TODO rename to ptrLE
  LLVMValueRef const refLE;

  WeakFatPtrLE(Reference* refM_, LLVMValueRef refLE_)
      : refM(refM_), refLE(refLE_) { }
};

// An LLVM register, which contains a reference.
struct Ref {
  Ref(Reference* refM_, LLVMValueRef refLE_) : refM(refM_), refLE(refLE_) {}

  void assertOwnership(Ownership ownership) {
    assert(refM->ownership == ownership);
  }

private:
  // This is private to keep us from just grabbing this to hand in to checkValidReference.
  // We should instead always pipe through the code the actual expected type.
  Reference* refM;

  LLVMValueRef refLE;

  friend std::tuple<Reference*, LLVMValueRef> megaGetRefInnardsForChecking(Ref ref);
  friend std::tuple<Reference*, LLVMValueRef> hgmGetRefInnardsForChecking(Ref ref);
  friend std::tuple<Reference*, LLVMValueRef> lgtGetRefInnardsForChecking(Ref ref);
  friend std::tuple<Reference*, LLVMValueRef> wrcGetRefInnardsForChecking(Ref ref);

  friend void buildPrint(
      GlobalState* globalState,
      LLVMBuilderRef builder,
      Ref ref);
};

// When we load something from an array, for example an owning reference,
// we still need to alias it to a constraint reference. This wrapper serves
// as a reminder that we need to do that.
struct LoadResult {
public:
  explicit LoadResult(Ref ref) : ref(ref) {}

  // This method is used when we intended to move the result, so no transformation
  // or aliasing is needed.
  Ref move() { return ref; }

  // This is just a getter for the ref for the methods that actually implement the
  // aliasing. It should ONLY be used by them.
  Ref extractForAliasingInternals() { return ref; }

private:
  Ref ref;
};

// A Ref that we're sure is alive right now.
struct LiveRef {
  Ref inner;

  explicit LiveRef(Ref inner_) : inner(inner_) { }
};

Ref wrap(IRegion* region, Reference* refM, LLVMValueRef exprLE);
Ref wrap(IRegion* region, Reference* refM, WrapperPtrLE exprLE);
Ref wrap(IRegion* region, Reference* refM, InterfaceFatPtrLE exprLE);
Ref wrap(IRegion* region, Reference* refM, WeakFatPtrLE exprLE);

LLVMValueRef checkValidInternalReference(
    AreaAndFileAndLine checkerAFL,
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    bool expectLive,
    Reference* refM,
    Ref ref);


#endif
