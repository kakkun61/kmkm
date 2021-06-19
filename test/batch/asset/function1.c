int function1_add(int const a, int const b)
{
  return function1_add(a, b);
}
int function1_succ(int const _a0)
{
  return function1_add(1, _a0);
}
int function1_closure1(void)
{
  int _l0(int const a)
  {
    return a;
  }
  return _l0(0);
}
int function1_closure2(int const a)
{
  int _l0(int const b)
  {
    return a;
  }
  return _l0(0);
}
int (* function1_succ2(void))(int const)
{
  return function1_succ;
}
int function1_higher1(int (* const a)(int const))
{
  return a(0);
}
int function1_two1(void)
{
  return function1_succ(1);
}
int function1_two2(void)
{
  return function1_succ2()(1);
}
