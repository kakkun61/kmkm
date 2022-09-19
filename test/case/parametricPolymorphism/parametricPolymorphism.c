#include "parametricPolymorphism.h"
#include "kmkm/prim.h"

void const (* parametricPolymorphism_id(void const (* const a)))
{
  return a;
}

kmkm_prim_string parametricPolymorphism_hello(void)
{
  kmkm_prim_string _v0 = u8"Hello world!";
  return *(((kmkm_prim_string (* (*)(kmkm_prim_string *)))parametricPolymorphism_id)(&_v0));
}

kmkm_prim_int parametricPolymorphism_zero(void)
{
  kmkm_prim_int _v0 = 0;
  return ((kmkm_prim_int (* (*)(kmkm_prim_int *))) parametricPolymorphism_id)(&_v0);
}

struct parametricPolymorphism_solo parametricPolymorphism_solo(void const (* const parametricPolymorphism_item))
{
  return (struct parametricPolymorphism_solo) { parametricPolymorphism_item };
}
