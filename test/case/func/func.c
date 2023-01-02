#include "func.h"
#include "kmkm/prim.h"

kmkm_prim_int func_add(kmkm_prim_int const a, kmkm_prim_int const b)
{
  return func_add(a, b);
}

kmkm_prim_int func_succ(kmkm_prim_int const _a0)
{
  kmkm_prim_int _l0 = 1;
  return func_add(_l0, _a0);
}

kmkm_prim_int func_closure1(void)
{
  kmkm_prim_int _l0(kmkm_prim_int const a)
  {
    return a;
  }
  kmkm_prim_int _l1 = 0;
  return _l0(_l1);
}

kmkm_prim_int func_closure2(kmkm_prim_int const a)
{
  kmkm_prim_int _l0(kmkm_prim_int const b)
  {
    return a;
  }
  kmkm_prim_int _l1 = 0;
  return _l0(_l1);
}

kmkm_prim_int const (* func_succ2(void))(kmkm_prim_int const)
{
  return func_succ;
}

kmkm_prim_int func_higher1(kmkm_prim_int const (* const a)(kmkm_prim_int const))
{
  kmkm_prim_int _l0 = 0;
  return a(_l0);
}

kmkm_prim_int func_two1(void)
{
  kmkm_prim_int _l0 = 1;
  return func_succ(_l0);
}

kmkm_prim_int func_two2(void)
{
  kmkm_prim_int _l0 = 1;
  return func_succ2()(_l0);
}
