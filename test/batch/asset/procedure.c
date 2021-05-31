enum unit_tag {
  unit_tag
};
struct unit {
  enum unit_tag tag;
};
struct unit const unit = { unit_tag };
struct unit rec0()
{
  return rec0();
}
struct unit rec1()
{
  return ({
    rec0();
  });
}
