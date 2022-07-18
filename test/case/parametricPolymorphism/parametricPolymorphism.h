#ifndef PARAMETRICPOLYMORPHISM_H
#define PARAMETRICPOLYMORPHISM_H

#include "kmkm/prim.h"

void const (* parametricPolymorphism_id(void const (* const a)));

kmkm_prim_string parametricPolymorphism_hello(void);

struct parametricPolymorphism_solo {
  void (* parametricPolymorphism_item);
};

struct parametricPolymorphism_solo parametricPolymorphism_solo(void const (* const parametricPolymorphism_item));

#endif
