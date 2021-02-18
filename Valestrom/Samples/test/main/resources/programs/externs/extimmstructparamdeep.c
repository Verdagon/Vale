#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "Bogglewoggle.h"
#include "Flamscrankle.h"

int64_t extFunc(Flamscrankle* flam) {
  printf("flam %p flam->a %ld flam->b %p flam->b->a %ld flam->c %ld\n", flam, flam->a, flam->b, flam->b->a, flam->c);
  int64_t result = flam->a + flam->b->a + flam->c;
  assert(
      (((size_t)(void*)flam->b - sizeof(Flamscrankle)) & 0xFFFFFFFFFFFFFFF0) ==
      (size_t)(void*)flam);
  free(flam);
  return result;
}
