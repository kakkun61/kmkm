#include "dependency.h"
#include "kmkm/prim.h"

kmkm_prim_int dependency_a(void)
{
  kmkm_prim_int _l0(kmkm_prim_int const x)
  {
    return dependency_b();
  }
  return _l0(dependency_c());
}

kmkm_prim_int dependency_b(void)
{
  return dependency_d();
}

kmkm_prim_int dependency_c(void)
{
  kmkm_prim_int _l0(kmkm_prim_int const x)
  {
    return dependency_d();
  }
  return _l0(dependency_e());
}

kmkm_prim_int dependency_d(void)
{
  return dependency_b();
}

kmkm_prim_int dependency_e(void)
{
  return 1;
}
