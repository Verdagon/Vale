#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "Spigglewigget.h"
#include "Bogglewoggle.h"
#include "Flamscrankle.h"

size_t nextMultipleOf16(size_t x) {
  return ((x - 1) | 15) + 1;
}
size_t floorMultipleOf16(size_t x) {
  return x & ~0xF;
}

int64_t extFunc(Flamscrankle* flam) {
  // Make sure the root pointer is at a multiple of 16.
  // If this fails, that means we have a bug, or malloc is breaking our assumptions
  // about alignment.
  assert(((size_t)(void*)flam & 0xF) == 0);

  // Most basic test, try to dereference the thing and make sure it contains something we expect.
  assert(flam->x == 7);

  size_t flamAddr = (size_t)(void*)flam;
  // AP = And Padding; to get the next multiple of 16 from the end of the Flamscrankle.
  size_t flamAPEndAddr = nextMultipleOf16(flamAddr + sizeof(Flamscrankle));

  // The root object (Flamscrankle here) always has a 16B "metadata block" before it, which contains
  // the start address and the size of the message.
  size_t rootMetadataAPEndAddr = flamAddr;
  size_t rootMetadataAddr = floorMultipleOf16(rootMetadataAPEndAddr - 16);

  // Bogglewoggle is before the Flamscrankle, but at a multiple of 16.
  size_t bogAPEndAddr = rootMetadataAddr;
  size_t bogAddr = floorMultipleOf16(bogAPEndAddr - sizeof(Bogglewoggle));

  // Spigglewigget is before the Bogglewoggle, but at a multiple of 16.
  size_t spigAPEndAddr = bogAddr;
  size_t spigAddr = floorMultipleOf16(spigAPEndAddr - sizeof(Spigglewigget));

  // Start metadata is before the Spigglewigget, but at a multiple of 16.
  size_t startMetadataAPEndAddr = spigAddr;
  size_t startMetadataAddr = floorMultipleOf16(startMetadataAPEndAddr - 24);

  {
    // The things in this block more just test the test itself, but thats fine.

    // Make sure that they're all at addresses that are multiples of 16
    assert(flamAddr == (flamAddr & ~0xF));
    assert(flamAPEndAddr == (flamAPEndAddr & ~0xF));
    assert(bogAddr == (bogAddr & ~0xF));
    assert(bogAPEndAddr == (bogAPEndAddr & ~0xF));
    assert(spigAddr == (spigAddr & ~0xF));
    assert(spigAPEndAddr == (spigAPEndAddr & ~0xF));
  }

  assert((size_t)(void*)flam->b == bogAddr);
  assert((size_t)(void*)flam->b->s == spigAddr);

  int64_t result = flam->x + flam->b->s->x + flam->b->s->y + flam->b->s->z + flam->b->x + flam->y;
  assert(result == 42);

  size_t startAddrFromRootMetadata = ((uint64_t*)(void*)rootMetadataAddr)[0];
  assert(startAddrFromRootMetadata == startMetadataAddr);
  size_t sizeFromRootMetadata = ((uint64_t*)(void*)rootMetadataAddr)[1];
  assert(startMetadataAddr + sizeFromRootMetadata == flamAPEndAddr);

  // Make sure the metadata block has the correct size in it, which should be the size of all of them
  // and the metadata block and the padding after each.
  uint64_t sizeFromStartMetadata = ((uint64_t*)(void*)startMetadataAddr)[0];
  assert(sizeFromStartMetadata == sizeFromRootMetadata);
  uint64_t startAddrFromStartMetadata = ((uint64_t*)(void*)startMetadataAddr)[1];
  assert(startAddrFromStartMetadata == startAddrFromRootMetadata);
  uint64_t rootAddrFromStartMetadata = ((uint64_t*)(void*)startMetadataAddr)[2];
  assert(rootAddrFromStartMetadata == flamAddr);

  ValeReleaseMessage(flam);
  return result;
}
