#include <stdint.h>
#include <string.h>
#include <stdio.h>

#include "Flamscrankle.h"
#include "Bogglewoggle.h"

int64_t extFunc(Flamscrankle* flam) {
  int64_t result = flam->a + flam->b->a + flam->c;
  free(flam);
  return result;
}
