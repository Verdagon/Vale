#include <stdint.h>
#include <string.h>
#include <stdio.h>

typedef struct ValeStr {
  uint64_t length;
  char chars[0];
} ValeStr;

int64_t extStrLen(ValeStr* haystackContainerStr) {
  printf("Header says len %lld\n", haystackContainerStr->length);
  char* haystackContainerChars = haystackContainerStr->chars;
  int64_t result = strlen(haystackContainerChars);
  printf("Returning str len %lld\n", result);
  return result;
}
