enum myBool_tag {
  myFalse_tag,
  myTrue_tag
};
struct myBool {
  enum myBool_tag tag;
};
struct myBool const myFalse = { myFalse_tag };
struct myBool const myTrue = { myTrue_tag };
struct fraction {
  int numerator;
  int denominator;
};
struct fraction fraction(int const numerator, int const denominator) {
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
struct passenger person(unsigned int const class) {
  return (struct passenger) { person_tag, class };
}
struct passenger pet(unsigned int const species) {
  return (struct passenger) { pet_tag, species };
}
enum unit_tag {
  unit_tag
};
struct unit {
  enum unit_tag tag;
};
struct unit const unit = { unit_tag };
