
#include "ppl_install.hh"
#include <SWI-Prolog.h>
#include <cassert>
// Temporary: just for abort().
#include <cstdlib>

#define SWI 1

namespace PPL = Parma_Polyhedra_Library;

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
Prolog_construct_list(Prolog_term_ref l,
		      Prolog_term_ref h, Prolog_term_ref t) {
  // Apparently, PL_cons_list() cannot fail.
  PL_cons_list(l, h, t);
}

inline void
Prolog_raise_exception(Prolog_term_ref t) {
  PL_raise_exception(t);
}

inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return PL_is_integer(t) != 0;
}

inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return PL_is_compound(t) != 0;
}

inline bool
Prolog_is_list(Prolog_term_ref t) {
  return PL_is_list(t) != 0;
}

inline long
Prolog_get_long(Prolog_term_ref t) {
  assert(Prolog_is_integer(t));
  long v;
  if (PL_get_long(t, &v) != 0)
    return v;
  else
    //throw integer_out_of_range(t);
    abort();
}

inline void
Prolog_get_name_arity(Prolog_term_ref t, Prolog_atom& name, int& arity) {
  if (PL_get_name_arity(t, &name, &arity) == 0)
    abort();
}

inline void
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref a) {
  if (PL_get_arg(i, t, a) == 0)
    abort();
}

inline void
Prolog_get_list(Prolog_term_ref l, Prolog_term_ref h, Prolog_term_ref t) {
  if (PL_get_list(l, h, t) == 0)
    abort();
}

inline void
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  // Apparently, PL_put_atom() cannot fail.
  PL_put_atom(t, a);
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does SWI support unlimited precision integer?
  return PPL::Integer(Prolog_get_long(t));
}

#include "../ppl_prolog.outlines.hh"
