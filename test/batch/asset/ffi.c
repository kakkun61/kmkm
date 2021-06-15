enum unit_tag {
  unit_tag
};
struct unit {
  enum unit_tag tag;
};
struct unit const unit = { unit_tag };
void hello(void) {
  printf("hello\n");
}
