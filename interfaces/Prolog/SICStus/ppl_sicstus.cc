
#include "ppl_install.hh"
#include <sicstus/sicstus.h>
#include <cassert>
// Temporary: just for abort().
#include <cstdlib>

#define SICSTUS 1

namespace PPL = Parma_Polyhedra_Library;

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
Prolog_construct_list(Prolog_term_ref l,
		      Prolog_term_ref h, Prolog_term_ref t) {
  if (SP_cons_list(l, h, t) == 0)
    abort();
}

inline void
Prolog_raise_exception(Prolog_term_ref t) {
  SP_raise_exception(t);
}

inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return SP_is_integer(t) != 0;
}

inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return SP_is_compound(t) != 0;
}

inline bool
Prolog_is_list(Prolog_term_ref t) {
  return SP_is_list(t) != 0;
}

inline long
Prolog_get_long(Prolog_term_ref t) {
  assert(Prolog_is_integer(t));
  long v;
  if (SP_get_integer(t, &v) != 0)
    return v;
  else
    //throw integer_out_of_range(t);
    abort();
}

inline void
Prolog_get_name_arity(Prolog_term_ref t, Prolog_atom& name, int& arity) {
  if (SP_get_functor(t, &name, &arity) == 0)
    abort();
}

inline void
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref a) {
  if (SP_get_arg(i, t, a) == 0)
    abort();
}

inline void
Prolog_get_list(Prolog_term_ref l, Prolog_term_ref h, Prolog_term_ref t) {
  if (SP_get_list(l, h, t) == 0)
    abort();
}

inline void
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  if (SP_put_atom(t, a) == 0)
    abort();
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: how can we get the unlimited precision integer?
  assert(SP_is_integer(t));
  long v;
  if (SP_get_integer(t, &v))
    return PPL::Integer(v);
  else
    //throw integer_out_of_range(t);
    abort();
}

#include "../ppl_prolog.outlines.hh"
