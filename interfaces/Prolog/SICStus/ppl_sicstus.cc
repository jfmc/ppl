
#include <sicstus/sicstus.h>

typedef SP_term_ref Prolog_term_ref;
typedef SP_atom Prolog_atom;

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


inline Prolog_atom
Prolog_atom_from_string(const char* s) {
  return SP_atom_from_string(s);
}

inline void
Prolog_construct_functor(Prolog_term_ref t, Prolog_atom name, int arity ...) {
  va_list ap;
  va_start(ap, arity);
  // FIXME: do not ignore the return value!
  SP_cons_functor(t, name, arity, ap);
  va_end(ap);
}

inline void
Prolog_raise_exception(Prolog_term_ref t) {
  SP_raise_exception(t);
}

#include "../ppl_prolog.outlines.hh"
