#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "Flamscrankle.h"

int64_t extFunc(Flamscrankle* flam) {
  int64_t result = flam->a + flam->c;
  ValeReleaseMessage(flam);
  return result;
}
