#ifndef FFI_H
#define FFI_H
#include "kmkm/prim.h"
#include "kmkm/unit.h"


kmkm_prim_int ffi_increment(kmkm_prim_int const v);

#include<stdio.h>

struct kmkm_unit_unit ffi_hello(void);
#endif

typedef int ffi_int;
