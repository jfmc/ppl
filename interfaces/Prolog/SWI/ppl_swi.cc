
#include <SWI-Prolog.h>

typedef term_t Prolog_term_ref;
typedef atom_t Prolog_atom;

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

inline Prolog_atom
Prolog_atom_from_string(const char* s) {
  return PL_new_atom(s);
}

inline void
Prolog_construct_functor(Prolog_term_ref t, Prolog_atom name, int arity ...) {
  va_list ap;
  va_start(ap, arity);
  PL_cons_functor(t, PL_new_functor(name, arity), ap);
  va_end(ap);
}

inline void
Prolog_raise_exception(Prolog_term_ref t) {
  PL_raise_exception(t);
}

#include "../ppl_prolog.outlines.hh"
