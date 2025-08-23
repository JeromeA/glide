#pragma once

// In addition to the symbol name, each symbol from the ELF relocation table has a 48 byte overhead:
// - 24 bytes in the rela section
// - 24 bytes in the dynsym section.
// - the relocation table itself is in the bss section, so it is not included in the binary.
// Our reloc function consumes 100 bytes, but saves 48 bytes per symbol, so it is worth it if we have at least 3
// symbols.

#ifdef RELOC

#include <dlfcn.h>

enum {
#define S(symbol, a1, a2, a3, a4) IDX_##symbol,
#include "symbols.inc"
#undef S
  NUM_FUNCTIONS
};

extern long (*reloc_functions[NUM_FUNCTIONS])();

#define RETURN(RTYPE, E) return (RTYPE)E
#define VOID(RTYPE, e) e
#define NORETURN(RTYPE, e) e; __builtin_unreachable()
#define S(symbol, RETURNVOID, RTYPE, DECL_PARAMS, CALL_PARAMS)         \
  extern inline __attribute__((gnu_inline)) RTYPE symbol DECL_PARAMS {               \
    RETURNVOID(RTYPE, reloc_functions[IDX_##symbol] CALL_PARAMS);  \
  }
#include "symbols.inc"
#undef S

typedef struct {
#define S(sym, a1, a2, a3, a4) struct { unsigned char len; char text[sizeof(#sym)]; } f_ ## sym;
#include "symbols.inc"
#undef S
} reloc_name_type;
extern reloc_name_type reloc_names;

#endif

static inline void relocate() {
#ifdef RELOC
  char *p = (char *)&reloc_names;
  for(int i=0 ; i<NUM_FUNCTIONS ; i++) {
    reloc_functions[i] = dlsym(RTLD_DEFAULT, p+1);
#ifdef DEBUG
    printf("Relocating symbol %d/%d: %s\n", i+1, NUM_FUNCTIONS, p+1);
    if (reloc_functions[i] == NULL) {
      printf("Failed to find symbol\n");
      exit(1);
    }
#endif
    p += *p;
  }
#endif
}

