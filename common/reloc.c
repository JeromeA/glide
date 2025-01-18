#include "includes.h"

#ifdef RELOC
reloc_name_type reloc_names = {
#define S(sym, a1, a2, a3, a4) .f_ ## sym = { sizeof(#sym) + 1, #sym },
#include "symbols.inc"
#undef S
};

long (*reloc_functions[NUM_FUNCTIONS])();
#endif

