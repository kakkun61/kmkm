int a()
{
  int _l0(int const x)
  {
    return b;
  }
  return _l0(c());
}
int b()
{
  return d();
}
int c()
{
  int _l0(int const x)
  {
    return d;
  }
  return _l0(e());
}
int d()
{
  return 0;
}
int e()
{
  return 1;
}
