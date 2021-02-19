#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "Bogglewoggle.h"
#include "Flamscrankle.h"

int64_t extFunc(Flamscrankle* flam) {
  int64_t result = flam->a + flam->b->a + flam->c;

  size_t flamAddr = (size_t)(void*)flam;
  size_t bogAddr = (size_t)(void*)flam->b;
  // Make sure that they're both at addresses that are multiples of 16
  assert(flamAddr == (flamAddr & ~0xF));
  assert(bogAddr == (bogAddr & ~0xF));
  // A more efficient but less intuitive way of writing the condition after this one
  assert(((bogAddr - sizeof(Flamscrankle)) & ~0xF) == flamAddr);
  // Make sure that the Bogglewoggle is at the next multiple of 16 past the end of the Flamscrankle
  assert(((flamAddr + sizeof(Flamscrankle)) | 0xF) + 1 == bogAddr);

  free(flam);

  return result;
}
