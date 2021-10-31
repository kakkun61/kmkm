#include "parametricPolymorphism.h"
#include "kmkm/prim.h"

void const (* parametricPolymorphism_id(void const (* const a)))
{
  return a;
}

kmkm_prim_string parametricPolymorphism_hello(void)
{
  return parametricPolymorphism_id(u8"Hello world!");
}
