
#include "ppl_install.hh"
#include <SWI-Prolog.h>
#include <cassert>
// Temporary: just for abort().
#include <cstdlib>

#define SWI 1
#define PARANOID 1

namespace PPL = Parma_Polyhedra_Library;

typedef term_t Prolog_term_ref;
typedef atom_t Prolog_atom;

#include "../exceptions.hh"

static inline Prolog_term_ref
Prolog_new_term_ref() {
  return PL_new_term_ref();
}

static inline void
Prolog_put_term(Prolog_term_ref t, Prolog_term_ref u) {
  PL_put_term(t, u);
}

static inline void
Prolog_put_long(Prolog_term_ref t, long i) {
  PL_put_integer(t, i);
}

static inline void
Prolog_put_string(Prolog_term_ref t, const char* s) {
  PL_put_string_chars(t, s);
}

static inline Prolog_atom
Prolog_atom_from_string(const char* s) {
  return PL_new_atom(s);
}

static inline void
Prolog_construct_functor(Prolog_term_ref t, Prolog_atom name, int arity ...) {
  va_list ap;
  va_start(ap, arity);
  PL_cons_functor(t, PL_new_functor(name, arity), ap);
  va_end(ap);
}

static inline void
Prolog_construct_list(Prolog_term_ref l,
		      Prolog_term_ref h, Prolog_term_ref t) {
  PL_cons_list(l, h, t);
}

static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  (void) PL_raise_exception(t);
}

static inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return PL_is_integer(t) != 0;
}

static inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return PL_is_compound(t) != 0;
}

static inline bool
Prolog_is_list(Prolog_term_ref t) {
  return PL_is_list(t) != 0;
}

static inline long
Prolog_get_long(Prolog_term_ref t) {
  assert(Prolog_is_integer(t));
  long v;
  if (PL_get_long(t, &v) == 0)
    throw_integer_out_of_range(t);
  return v;
}

static inline void
Prolog_get_name_arity(Prolog_term_ref t, Prolog_atom& name, int& arity) {
  assert(Prolog_is_compound(t));
  CHECK_STATUS(PL_get_name_arity(t, &name, &arity));
}

static inline void
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref a) {
  assert(Prolog_is_compound(t));
  CHECK_STATUS(PL_get_arg(i, t, a));
}

static inline void
Prolog_get_list(Prolog_term_ref l, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_list(t));
  CHECK_STATUS(PL_get_list(l, h, t));
}

static inline void
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  PL_put_atom(t, a);
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does SWI support unlimited precision integer?
  return PPL::Integer(Prolog_get_long(t));
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  // FIXME: does SWI support unlimited precision integer?
  Prolog_term_ref t = Prolog_new_term_ref();
  if (!n.fits_slong_p())
    throw_unknown_interface_error();
  PL_put_integer(t, n.get_si());
  return t;
}

#include "../ppl_prolog.outlines.hh"

#if 0
extern "C" foreign_t
ppl_new_polyhedron(Prolog_term_ref tp, Prolog_term_ref td) {
  try {
    if (!Prolog_is_integer(td))
      throw not_an_integer(td);
    PPL::Polyhedron* ret = new PPL::Polyhedron(get_size_t(num_dimensions));
    REGISTER(ret);
    return TRUE;
  }
  CATCH_ALL;
  return FALSE;

extern "C" void*
ppl_new_empty_polyhedron(long num_dimensions) {
  try {
    PPL::Polyhedron* ret = new PPL::Polyhedron(get_size_t(num_dimensions),
					       PPL::Polyhedron::EMPTY);
    REGISTER(ret);
    return ret;
  }
  CATCH_ALL;
  return 0;
}

extern "C" void*
ppl_copy_polyhedron(const void* pp) {
  try {
    CHECK(pp);
    PPL::Polyhedron* ret
      = new PPL::Polyhedron(*static_cast<const PPL::Polyhedron*>(pp));
    REGISTER(ret);
    return ret;
  }
  CATCH_ALL;
  return 0;
}

extern "C" void
ppl_delete_polyhedron(void* pp) {
  // If destructors throw it is a catastrophy.
  // Anyway...
  try {
    UNREGISTER(pp);
    delete static_cast<PPL::Polyhedron*>(pp);
  }
  CATCH_PPL;
}

extern "C" long
ppl_space_dimension(const void* pp) {
  CHECK(pp);
  // Polyhedron::space_dimension() cannot throw.
  return static_cast<const PPL::Polyhedron*>(pp)->space_dimension();
}

#endif

/*
static PL_extension predicates[] = {
{ "ppl_new_polyhedron",        1,      ppl_new_polyhedron, 0 },
{ "ppl_new_empty_polyhedron",        1,      ppl_new_empty_polyhedron, 0 },
{ NULL,         0,      NULL,   0 }
};

void install() {
  PL_register_extensions(predicates);
}
*/
