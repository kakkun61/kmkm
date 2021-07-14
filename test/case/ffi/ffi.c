#include "ffi.h"
#include <stdio.h>

enum ffi_unit_tag {
  ffi_unit_tag
};

struct ffi_unit {
  enum ffi_unit_tag tag;
};

struct ffi_unit const ffi_unit = { ffi_unit_tag };

void ffi_hello(void)
{
  printf("hello\n");
}
