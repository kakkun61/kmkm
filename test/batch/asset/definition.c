enum myBool_tag {
  myFalse_tag,
  myTrue_tag
};
struct myBool {
  enum myBool_tag tag;
};
const struct myBool myFalse = { myFalse_tag };
const struct myBool myTrue = { myTrue_tag };
struct fraction {
  int numerator;
  int denominator;
};
struct fraction fraction(const int numerator, const int denominator) {
  return (struct fraction) { numerator, denominator };
}
enum passenger_tag {
  person_tag,
  pet_tag
};
struct passenger {
  enum passenger_tag tag;
  union {
    struct {
      unsigned int class;
    } person;
    struct {
      unsigned int species;
    } pet;
  } body;
};
struct passenger person(const unsigned int class) {
  return (struct passenger) { person_tag, class };
}
struct passenger pet(const unsigned int species) {
  return (struct passenger) { pet_tag, species };
}
enum unit_tag {
  unit_tag
};
struct unit {
  enum unit_tag tag;
};
const struct unit unit = { unit_tag };
