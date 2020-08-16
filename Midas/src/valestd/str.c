#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define FALSE 0
#define TRUE 1


//struct String { // 24b
//  isInline : 1;
//  lengthIfInline : 7;
//
//  int lengthIfYonder;
//  StringContents* contentsIfYonder;
//}
//struct StringContents {
//  int rc;
//  char bytes[];
//}
//if some byte is 0, then its inline.
//- length is the next 7 bits, why not.
//- then is the byte contents.


//// Note that this is NOT __Str_rc, which is what Midas usually handles. This
//// lives inside __Str_rc.
//typedef struct {
//  int64_t length;
//  uint8_t chars[];
//} __Str;

void __vinitStr(const char * newStr, const char* chars) {
  for (int i = 0; ; i++) {
    newStr[i] = chars[i];
    // This is at the end because we want to copy the null terminating char too.
    if (chars[i] == 0) {
      break;
    }
  }
}

void __vaddStr(const char * a, const char * b, const char * dest) {
  int a_len = a->length;
  int b_len = b->length;
  dest->length = a_len + b_len;
  for (int i = 0; i < a_len; i++) {
    dest[i] = a[i];
  }
  for (int i = 0; i < b_len; i++) {
    dest[i + a_len] = b[i];
  }
  // Add a null terminating char for compatibility with C.
  // Midas should allocate an extra byte to accommodate this.
  dest[dest->length] = 0;
}

uint8_t __veqStr(const char * a, const char * b) {
  int a_len = a->length;
  int b_len = b->length;
  if (a_len != b_len) {
    return FALSE;
  }
  for (int i = 0; i < a_len; i++) {
    if (a[i] != b[i]) {
      return FALSE;
    }
  }
  return TRUE;
}

void __vprintStr(const char * a) {
  printf("%s", a);
}

void __vintToCStr(int n, char* dest, int destSize) {
  snprintf(dest, destSize, "%d", n);
}
