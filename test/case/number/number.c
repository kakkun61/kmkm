#include "number.h"
#include "kmkm/prim.h"
kmkm_prim_int number_piIntDecimal(void)
{
  kmkm_prim_int _l0 = 3;
  return _l0;
}
kmkm_prim_int number_piIntBinary(void)
{
  kmkm_prim_int _l0 = 0x3;
  return _l0;
}
kmkm_prim_int number_piIntOctal(void)
{
  kmkm_prim_int _l0 = 03;
  return _l0;
}
kmkm_prim_int number_piIntHexadecimal(void)
{
  kmkm_prim_int _l0 = 0x3;
  return _l0;
}
kmkm_prim_frac2 number_piDoubleDecimal(void)
{
  kmkm_prim_frac2 _l0 = 3.1415926535e0;
  return _l0;
}
kmkm_prim_frac2 number_piDoubleHexadecimal(void)
{
  kmkm_prim_frac2 _l0 = 0x3.243f6a8822e87c199acbp0;
  return _l0;
}
kmkm_prim_frac2 number_test1DoubleHexadecimal(void)
{
  kmkm_prim_frac2 _l0 = 0x1p1;
  return _l0;
}
kmkm_prim_frac2 number_test2DoubleHexadecimal(void)
{
  kmkm_prim_frac2 _l0 = 0x1p-1;
  return _l0;
}
kmkm_prim_frac2 number_test3DoubleHexadecimal(void)
{
  kmkm_prim_frac2 _l0 = 0x1.1p1;
  return _l0;
}
