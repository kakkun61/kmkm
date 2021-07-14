#ifndef FFI_H
#define FFI_H

#include <stdio.h>

enum ffi_unit_tag {
  ffi_unit_tag
};

struct ffi_unit {
  enum ffi_unit_tag tag;
};

struct ffi_unit const ffi_unit;

void ffi_hello(void);

#endif
