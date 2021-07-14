#include "number.h"
#include "kmkm/prim.h"
kmkm_prim_int number_piIntDecimal(void)
                                           {
                                               return 3;
                                           }
                                           kmkm_prim_int number_piIntBinary(void)
                                           {
                                               return 0x3;
                                           }
                                           kmkm_prim_int number_piIntOctal(void)
                                           {
                                               return 03;
                                           }
                                           kmkm_prim_int number_piIntHexadecimal(void)
                                           {
                                               return 0x3;
                                           }
                                           kmkm_prim_frac2 number_piDoubleDecimal(void)
                                           {
                                               return 3.1415926535e0;
                                           }
                                           kmkm_prim_frac2 number_piDoubleHexadecimal(void)
                                           {
                                               return 0x3.243f6a8822e87c199acbp0;
                                           }
                                           kmkm_prim_frac2 number_test1DoubleHexadecimal(void)
                                           {
                                               return 0x1p1;
                                           }
                                           kmkm_prim_frac2 number_test2DoubleHexadecimal(void)
                                           {
                                               return 0x1p-1;
                                           }
                                           kmkm_prim_frac2 number_test3DoubleHexadecimal(void)
                                           {
                                               return 0x1.1p1;
                                           }
