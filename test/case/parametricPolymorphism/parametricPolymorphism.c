#include "parametricPolymorphism.h"

void const (* parametricPolymorphism_id(void const (* const a)))
{
  return a;
}
