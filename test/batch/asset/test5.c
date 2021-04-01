enum unit_tag {
  unit_tag
};
struct unit {
  enum unit_tag tag;
};
const struct unit unit = { unit_tag };
