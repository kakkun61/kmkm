enum unit_tag {
  unit_tag
};
struct unit {
  enum unit_tag tag;
};
struct unit const unit = { unit_tag };
struct unit procedure_rec0()
{
  return procedure_rec0();
}
struct unit procedure_rec1()
{
  return ({
    procedure_rec0();
  });
}
