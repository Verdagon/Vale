#include <globalstate.h>
#include "boundary.h"
#include "region/iregion.h"

Ref sendHostObjectIntoVale(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* hostRefMT,
    Reference* valeRefMT,
    LLVMValueRef hostRefLE) {
  // - For example, in:
  //     fn fly(ship 'hgm Spaceship) extern;
  //   when we call it with an object from 'hgm, we're not moving/copying
  //   it between regions, but we do need to encrypt it. So, we'll call
  //   encryptAndSendFamiliarReference.
  // - For example, in:
  //     fn fly(ship 'hgm Spaceship) export { ... }
  //   when the outside world calls it with an object from 'hgm, we're not
  //   moving/copying between regions, but we do need to decrypt it. So,
  //   we'll call receiveAndDecryptFamiliarReference.
  // - For example, in:
  //     fn fly(pattern Pattern) extern;
  //   regardless of whether Pattern is a val or inst, we'll be moving/
  //   copying between regions, so we'll call
  //   receiveUnencryptedAlienReference. HOWEVER, we don't yet support
  //   moving instances between regions, so this is only for vals for now.
  if (hostRefMT->ownership == Ownership::SHARE) {
    auto hostRef =
        wrap(globalState->getRegion(hostRefMT), hostRefMT, hostRefLE);
    return globalState->getRegion(valeRefMT)
        ->receiveUnencryptedAlienReference(
            functionState, builder, hostRefMT, hostRef);
  } else {
    auto valeRef =
        wrap(globalState->getRegion(hostRefMT), hostRefMT, hostRefLE);

    // Alias when receiving from the outside world, see DEPAR.
    globalState->getRegion(hostRefMT)
        ->alias(FL(), functionState, builder, hostRefMT, valeRef);

    return valeRef;
  }
}

LLVMValueRef sendValeObjectIntoHost(
    GlobalState* globalState,
    FunctionState* functionState,
    LLVMBuilderRef builder,
    Reference* valeRefMT,
    Reference* hostRefMT,
    Ref valeRef) {
  // - For example, in:
  //     fn fly(ship 'hgm Spaceship) extern;
  //   when we call it with an object from 'hgm, we're not moving/copying
  //   it between regions, but we do need to encrypt it. So, we'll call
  //   encryptAndSendFamiliarReference.
  // - For example, in:
  //     fn fly(ship 'hgm Spaceship) export { ... }
  //   when the outside world calls it with an object from 'hgm, we're not
  //   moving/copying between regions, but we do need to decrypt it. So,
  //   we'll call receiveAndDecryptFamiliarReference.
  // - For example, in:
  //     fn fly(pattern Pattern) extern;
  //   regardless of whether Pattern is a val or inst, we'll be moving/
  //   copying between regions, so we'll call
  //   receiveUnencryptedAlienReference. HOWEVER, we don't yet support
  //   moving instances between regions, so this is only for vals for now.
  if (valeRefMT->ownership == Ownership::SHARE) {
    auto hostArgRef =
        globalState->getRegion(hostRefMT)
            ->receiveUnencryptedAlienReference(
                functionState, builder, valeRefMT, valeRef);
    auto hostArgLE =
        globalState->getRegion(hostRefMT)
            ->checkValidReference(FL(), functionState, builder, hostRefMT, hostArgRef);
    return hostArgLE;
  } else {
    // When we actually encrypt, this line will change.
    auto encryptedValeRef = valeRef;

    // Dealias when sending to the outside world, see DEPAR.
    globalState->getRegion(valeRefMT)
        ->dealias(FL(), functionState, builder, valeRefMT, encryptedValeRef);

    auto encryptedValeRefLE =
        globalState->getRegion(valeRefMT)
            ->checkValidReference(FL(), functionState, builder, valeRefMT, encryptedValeRef);
    return encryptedValeRefLE;
  }
}
