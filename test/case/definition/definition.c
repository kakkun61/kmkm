#include "definition.h"
#include "kmkm/prim.h"

struct definition_myBool const definition_myFalse = { definition_myFalse_tag };

struct definition_myBool const definition_myTrue = { definition_myTrue_tag };

struct definition_fraction definition_fraction(kmkm_prim_int const definition_numerator, kmkm_prim_int const definition_denominator)
{
  return (struct definition_fraction) { definition_numerator, definition_denominator };
}

struct definition_passenger definition_person(kmkm_prim_uint const definition_class)
{
  return (struct definition_passenger) { definition_person_tag, definition_class };
}

struct definition_passenger definition_pet(kmkm_prim_uint const definition_species)
{
  return (struct definition_passenger) { definition_pet_tag, definition_species };
}

struct definition_unit const definition_unit = { definition_unit_tag };
