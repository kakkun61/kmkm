#include "func.h"
#include "kmkm/prim.h"

kmkm_prim_int func_add(kmkm_prim_int const a, kmkm_prim_int const b)
{
  return func_add(a, b);
}

kmkm_prim_int func_succ(kmkm_prim_int const _a0)
{
  return func_add(1, _a0);
}

kmkm_prim_int func_closure1(void)
{
  kmkm_prim_int _l0(kmkm_prim_int const a)
  {
    return a;
  }
  return _l0(0);
}

kmkm_prim_int func_closure2(kmkm_prim_int const a)
{
  kmkm_prim_int _l0(kmkm_prim_int const b)
  {
    return a;
  }
  return _l0(0);
}

kmkm_prim_int (* func_succ2(void))(kmkm_prim_int const)
{
  return func_succ;
}

kmkm_prim_int func_higher1(kmkm_prim_int const (* const a)(kmkm_prim_int const))
{
  return a(0);
}

kmkm_prim_int func_two1(void)
{
  return func_succ(1);
}

kmkm_prim_int func_two2(void)
{
  return func_succ2()(1);
}
