#include <stdint.h>
#include <stdio.h>
#include "ImmIntArray.h"

int64_t sumBytes(ImmIntArray* arr) {
  int64_t total = 0;
  printf("length: %lld\n", arr->length);
  for (int i = 0; i < arr->length; i++) {
    printf("this element: %lld\n", arr->elements[i]);
    total += arr->elements[i];
  }
  printf("total: %lld\n", total);
  return total;
}
