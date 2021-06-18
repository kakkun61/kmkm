int dependency_a()
{
  int _l0(int const x)
  {
    return dependency_b();
  }
  return _l0(dependency_c());
}
int dependency_b()
{
  return dependency_d();
}
int dependency_c()
{
  int _l0(int const x)
  {
    return dependency_d();
  }
  return _l0(dependency_e());
}
int dependency_d()
{
  return dependency_b();
}
int dependency_e()
{
  return 1;
}
