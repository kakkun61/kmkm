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
  int _l1(int const b)
  {
    return a;
  }
  return _l1(0);
}
