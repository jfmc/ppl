
#include <sicstus/sicstus.h>

typedef SP_term_ref Prolog_term_ref;

inline Prolog_term_ref
Prolog_new_term_ref() {
  return SP_new_term_ref();
}

inline void
Prolog_put_term(Prolog_term_ref t, Prolog_term_ref u) {
  SP_put_term(t, u);
}

inline void
Prolog_put_integer(Prolog_term_ref t, long i) {
  SP_put_integer(t, i);
}

inline void
Prolog_put_string(Prolog_term_ref t, const char* s) {
  SP_put_string(t, s);
}

#include "../ppl_prolog.outlines.hh"
