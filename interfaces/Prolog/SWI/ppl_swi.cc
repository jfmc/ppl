
#include <SWI-Prolog.h>

typedef term_t Prolog_term_ref;

inline Prolog_term_ref
Prolog_new_term_ref() {
  return PL_new_term_ref();
}

inline void
Prolog_put_term(Prolog_term_ref t, Prolog_term_ref u) {
  PL_put_term(t, u);
}

inline void
Prolog_put_integer(Prolog_term_ref t, long i) {
  PL_put_integer(t, i);
}

inline void
Prolog_put_string(Prolog_term_ref t, const char* s) {
  PL_put_string_chars(t, s);
}

#include "../ppl_prolog.outlines.hh"
