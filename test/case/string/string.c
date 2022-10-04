#include "string.h"
#include "kmkm/prim.h"

kmkm_prim_string string_hello(void)
{
  kmkm_prim_string _l0 = u8"hello";
  return _l0;
}

kmkm_prim_string string_helloEs(void)
{
  kmkm_prim_string _l0 = u8"\U000000a1Hola!";
  return _l0;
}

kmkm_prim_string string_helloJa(void)
{
  kmkm_prim_string _l0 = u8"\U00003053\U00003093\U0000306b\U00003061\U0000306f";
  return _l0;
}

kmkm_prim_string string_triple1(void)
{
  kmkm_prim_string _l0 = u8"hello";
  return _l0;
}

kmkm_prim_string string_triple2(void)
{
  kmkm_prim_string _l0 = u8"printf(\"\");";
  return _l0;
}
