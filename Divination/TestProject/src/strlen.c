#pragma rsuse Str = &str
#pragma rsuse String = std::string::String
#pragma rsfn String_fromStr = String::from(&str)
#pragma rsfn String_len = String::len
#pragma rsfn String_drop = String::drop
#include <rust_deps/rust_deps.h>
#include <stdio.h>

int main() {
  Str b = rust_StrFromCStr("bork");
  String s = String_fromStr(b);
  int len = String_len(&s);
  printf("Length: %d\n", len);
  String_drop(s);
  return 0;
}
