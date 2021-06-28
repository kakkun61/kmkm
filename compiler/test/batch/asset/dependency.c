int dependency_a(void)
{
  int _l0(int const x)
  {
    return dependency_b();
  }
  return _l0(dependency_c());
}
int dependency_b(void)
{
  return dependency_d();
}
int dependency_c(void)
{
  int _l0(int const x)
  {
    return dependency_d();
  }
  return _l0(dependency_e());
}
int dependency_d(void)
{
  return dependency_b();
}
int dependency_e(void)
{
  return 1;
}
