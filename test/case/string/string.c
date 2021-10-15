#include "string.h"
#include "kmkm/prim.h"

kmkm_prim_string string_hello(void)
{
  return u8"hello";
}

kmkm_prim_string string_helloEs(void)
{
  return u8"\U000000a1Hola!";
}

kmkm_prim_string string_helloJa(void)
{
  return u8"\U00003053\U00003093\U0000306b\U00003061\U0000306f";
}

kmkm_prim_string string_triple1(void)
{
  return u8"hello";
}

kmkm_prim_string string_triple2(void)
{
  return u8"printf(\"\");";
}
