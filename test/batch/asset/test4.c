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
