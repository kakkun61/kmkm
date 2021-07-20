#include "ffi.h"
#include "kmkm/prim.h"
#include "kmkm/unit.h"



kmkm_prim_int ffi_increment(kmkm_prim_int const v)
{

  return v + 1;

}

#include<stdio.h>

struct kmkm_unit_unit ffi_hello(void)
{

  printf("hello\n");
  return kmkm_unit_unit;

}
