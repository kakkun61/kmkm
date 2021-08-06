#ifndef DEFINITION_H
#define DEFINITION_H
#include "kmkm/prim.h"
enum definition_myBool_tag {
  definition_myFalse_tag,
  definition_myTrue_tag
};

struct definition_myBool {
  enum definition_myBool_tag tag;
};

struct definition_myBool extern const definition_myFalse;

struct definition_myBool extern const definition_myTrue;

struct definition_fraction {
  kmkm_prim_int definition_numerator;
  kmkm_prim_int definition_denominator;
};

struct definition_fraction definition_fraction(kmkm_prim_int const definition_numerator, kmkm_prim_int const definition_denominator);

enum definition_passenger_tag {
  definition_person_tag,
  definition_pet_tag
};

struct definition_passenger {
  enum definition_passenger_tag tag;
  union {
    struct {
      kmkm_prim_uint definition_class;
    } definition_person;
    struct {
      kmkm_prim_uint definition_species;
    } definition_pet;
  } body;
};

struct definition_passenger definition_person(kmkm_prim_uint const definition_class);

struct definition_passenger definition_pet(kmkm_prim_uint const definition_species);

enum definition_unit_tag {
  definition_unit_tag
};

struct definition_unit {
  enum definition_unit_tag tag;
};

struct definition_unit extern const definition_unit;
#endif
