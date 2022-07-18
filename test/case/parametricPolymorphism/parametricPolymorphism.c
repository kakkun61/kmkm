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

struct parametricPolymorphism_solo parametricPolymorphism_solo(void const (* const parametricPolymorphism_item))
{
  return (struct parametricPolymorphism_solo) { parametricPolymorphism_item };
}
