#ifndef FUNC_H
#define FUNC_H

#include "kmkm/prim.h"

kmkm_prim_int func_add(kmkm_prim_int const a, kmkm_prim_int const b);
kmkm_prim_int func_succ(kmkm_prim_int const _a0);
kmkm_prim_int func_closure1(void);
kmkm_prim_int func_closure2(kmkm_prim_int const a);
kmkm_prim_int const (* func_succ2(void))(kmkm_prim_int const);
kmkm_prim_int func_higher1(kmkm_prim_int const (* const a)(kmkm_prim_int const));
kmkm_prim_int func_two1(void);
kmkm_prim_int func_two2(void);

#endif
