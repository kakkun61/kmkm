int add(int const a, int const b)
{
  return add(a, b);
}
int succ(int const _a0)
{
  return add(1, _a0);
}
int closure1()
{
  int _l0(int const a)
  {
    return a;
  }
  return _l0(0);
}
int closure2(int const a)
{
  int _l0(int const b)
  {
    return a;
  }
  return _l0(0);
}
int (* succ2())(int const)
{
  return succ;
}
int higher1(int (* const a)(int const))
{
  return a(0);
}
int two1()
{
  return succ(1);
}
int two2()
{
  return succ2()(1);
}
