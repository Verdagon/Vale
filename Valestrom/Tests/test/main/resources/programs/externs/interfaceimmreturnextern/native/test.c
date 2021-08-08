#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "tmod/IShip.h"
#include "tmod/Firefly.h"
#include "tmod/cMakeShip.h"

tmod_IShip tmod_cMakeShip() {
  tmod_Firefly* firefly = (tmod_Firefly*)malloc(sizeof(tmod_Firefly));
  firefly->fuel = 42;

  // If the enum isnt 64 bits, we run into some undefined padding when there
  // are only 1 or 2 values in the enum.
  // Oddly, when we have 3 values in the enum, the problem disappears.
  // Anyway, we generate a tmod_IShip_Type_MAX_VALUE = 0x7FFFFFFFFFFFFFFF to
  // force this and fix it for good.
  // This assert is to check that it's 64 bits even though there's only one
  // entry in the enum.
  // Nevermind, it doesn't work for windows!

  tmod_IShip shipRef;
  shipRef.obj = firefly;
  shipRef.type = tmod_IShip_Type_Firefly;

  return shipRef;
}
