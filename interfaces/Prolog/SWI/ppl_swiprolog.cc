
#include "ppl_install.hh"
#include <SWI-Prolog.h>
#include <cassert>

#define SWI 1
#define PARANOID 1

typedef term_t Prolog_term_ref;
typedef atom_t Prolog_atom;
typedef foreign_t Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = TRUE;
static const Prolog_foreign_return_type PROLOG_FAILURE = FALSE;

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

/*!
  Return a new term reference.
*/
static inline Prolog_term_ref
Prolog_new_term_ref() {
  return PL_new_term_ref();
}

/*!
  Make \p t be a reference to the same term referenced by \p u,
  i.e., assign \p u to \p t.
*/
static inline bool
Prolog_put_term(Prolog_term_ref t, Prolog_term_ref u) {
  PL_put_term(t, u);
  return true;
}

/*!
  Assign to \p t a Prolog integer with value \p i.
*/
static inline bool
Prolog_put_long(Prolog_term_ref t, long i) {
  PL_put_integer(t, i);
  return true;
}

/*!
  Assign to \p t an atom whose name is given by the null-terminated string \s.
*/
static inline bool
Prolog_put_atom_chars(Prolog_term_ref t, const char* s) {
  PL_put_atom_chars(t, s);
  return true;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline bool
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  PL_put_atom(t, a);
  return true;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline bool
Prolog_put_address(Prolog_term_ref t, void* p) {
  PL_put_pointer(t, p);
  return true;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  return PL_new_atom(s);
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1) {
  PL_cons_functor(t, PL_new_functor(f, 1), a1);
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  PL_cons_functor(t, PL_new_functor(f, 2), a1, a2);
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  PL_cons_functor(t, PL_new_functor(f, 3), a1, a2, a3);
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  PL_cons_functor(t, PL_new_functor(f, 4), a1, a2, a3, a4);
  return true;
}

/*!
  Assign to \p l a Prolog list whose head is \p h and tail is \p t. 
*/
static inline bool
Prolog_construct_list(Prolog_term_ref l,
		      Prolog_term_ref h, Prolog_term_ref t) {
  PL_cons_list(l, h, t);
  return true;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  (void) PL_raise_exception(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise. 
*/
static inline bool
Prolog_is_variable(Prolog_term_ref t) {
  return PL_is_variable(t) != 0;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise. 
*/
static inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return PL_is_integer(t) != 0;
}

/*!
  Return true if \p t is the representation of an address, false otherwise. 
*/
static inline bool
Prolog_is_address(Prolog_term_ref t) {
  return PL_is_integer(t) != 0;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise. 
*/
static inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return PL_is_compound(t) != 0;
}

/*!
  Return true if \p t is a Prolog list, false otherwise. 
*/
static inline bool
Prolog_is_list(Prolog_term_ref t) {
  return PL_is_list(t) != 0;
}

/*!
  Assuming \p t is a Prolog integer, return true if its value fits
  in a long, in which case the value is assigned to \p v,
  return false otherwise.  The behavior is undefined if \p t is
  not a Prolog integer.
*/
static inline bool
Prolog_get_long(Prolog_term_ref t, long& v) {
  assert(Prolog_is_integer(t));
  return PL_get_long(t, &v) != 0;
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
static inline bool
Prolog_get_address(Prolog_term_ref t, void*& p) {
  assert(Prolog_is_address(t));
  return PL_get_pointer(t, &p) != 0;
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline bool
Prolog_get_name_arity(Prolog_term_ref t, Prolog_atom& name, int& arity) {
  assert(Prolog_is_compound(t));
  return PL_get_name_arity(t, &name, &arity) != 0;
}

/*!
  If \p t is a Prolog compound term and \p i is a positive integer
  less than or equal to its arity, return true and assign to \p a the
  i-th (principal) argument of \p t.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline bool
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref a) {
  assert(Prolog_is_compound(t));
  return PL_get_arg(i, t, a) != 0;
}

/*!
  If \p l is a Prolog list, assign its head and tail to \p h and \p t,
  respectively.
  The behavior is undefined if \p l is not a Prolog list.
*/
static inline bool
Prolog_get_list(Prolog_term_ref l, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_list(t));
  return PL_get_list(l, h, t) != 0;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline bool
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return PL_unify(t, u) != 0;
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does SWI support unlimited precision integer?
  long v;
  Prolog_get_long(t, v);
  return PPL::Integer(v);
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  // FIXME: does SWI support unlimited precision integer?
  if (!n.fits_slong_p())
    throw_unknown_interface_error("Integer_to_integer_term()");
  Prolog_term_ref t = Prolog_new_term_ref();
  PL_put_integer(t, n.get_si());
  return t;
}

#include "../ppl_prolog.outlines.hh"

extern "C" Prolog_foreign_return_type
ppl_new_polyhedron(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    PPL::Polyhedron* ph
      = new PPL::Polyhedron(term_to_unsigned_int(t_nd));
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_new_empty_polyhedron(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    PPL::Polyhedron* ph
      = new PPL::Polyhedron(term_to_unsigned_int(t_nd),
			    PPL::Polyhedron::EMPTY);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_copy_polyhedron(Prolog_term_ref t_ph, Prolog_term_ref t_source) {
  try {
    void* source;
    if (!Prolog_get_address(t_source, source))
      return PROLOG_FAILURE;
    CHECK(source);
    PPL::Polyhedron* ph
      = new PPL::Polyhedron(*static_cast<const PPL::Polyhedron*>(source));
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_delete_polyhedron(Prolog_term_ref t_ph) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    UNREGISTER(ph);
    delete static_cast<PPL::Polyhedron*>(ph);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_space_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    size_t sd = static_cast<const PPL::Polyhedron*>(ph)->space_dimension();
    Prolog_term_ref tmp = Prolog_new_term_ref();
    if (sd <= LONG_MAX
	&& Prolog_put_long(tmp, long(sd)) 
	&& Prolog_unify(t_sd, tmp))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_insert_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    static_cast<PPL::Polyhedron*>(ph)->insert(build_constraint(t_c));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_insert_generator(Prolog_term_ref t_ph, Prolog_term_ref t_g) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    static_cast<PPL::Polyhedron*>(ph)->insert(build_generator(t_g));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_constraints_and_minimize(Prolog_term_ref t_ph,
				 Prolog_term_ref t_clist) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    PPL::ConSys cs;
    Prolog_term_ref c = Prolog_new_term_ref();
    while (Prolog_is_list(t_clist)) {
      Prolog_get_list(t_clist, c, t_clist);
      cs.insert(build_constraint(c));
    }
    if (static_cast<PPL::Polyhedron*>(ph)->add_constraints_and_minimize(cs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_check_empty(Prolog_term_ref t_ph) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    if (static_cast<PPL::Polyhedron*>(ph)->check_empty())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_intersection_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.intersection_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_intersection_assign_and_minimize(Prolog_term_ref t_lhs,
				     Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.intersection_assign_and_minimize(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_hull_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_hull_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_hull_assign_and_minimize(Prolog_term_ref t_lhs,
				    Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_hull_assign_and_minimize(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_difference_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_difference_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_difference_assign_and_minimize(Prolog_term_ref t_lhs,
					  Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_difference_assign_and_minimize(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_widening_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    void* lhs;
    void* rhs;
    if (!Prolog_get_address(t_lhs, lhs) || !Prolog_get_address(t_rhs, rhs))
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.widening_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_get_constraints(Prolog_term_ref t_ph, Prolog_term_ref t_clist) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);

    const PPL::ConSys& cs
      = static_cast<const PPL::Polyhedron*>(ph)->constraints();

    for (PPL::ConSys::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      Prolog_term_ref new_tail = Prolog_new_term_ref();
      Prolog_construct_list(new_tail, constraint_term(*i), tail);
      tail = new_tail;
    }

    if (Prolog_unify(t_clist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_get_generators(Prolog_term_ref t_ph, Prolog_term_ref t_glist) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);

    const PPL::GenSys& gs
      = static_cast<const PPL::Polyhedron*>(ph)->generators();

    for (PPL::GenSys::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i) {
      Prolog_term_ref new_tail = Prolog_new_term_ref();
      Prolog_construct_list(new_tail, generator_term(*i), tail);
      tail = new_tail;
    }

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_remove_dimensions(Prolog_term_ref t_ph, Prolog_term_ref t_vlist) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    std::set<PPL::Variable> dead_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_list(t_vlist)) {
      Prolog_get_list(t_vlist, v, t_vlist);
      dead_variables.insert(get_variable(v));
    }
    static_cast<PPL::Polyhedron*>(ph)->remove_dimensions(dead_variables);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_remove_higher_dimensions(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    static_cast<PPL::Polyhedron*>(ph)
      ->remove_higher_dimensions(term_to_unsigned_int(t_nd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_dimensions_and_project(Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    static_cast<PPL::Polyhedron*>(ph)
      ->add_dimensions_and_project(term_to_unsigned_int(t_nnd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_dimensions_and_embed(Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  try {
    void* ph;
    if (!Prolog_get_address(t_ph, ph))
      return PROLOG_FAILURE;
    CHECK(ph);
    static_cast<PPL::Polyhedron*>(ph)
      ->add_dimensions_and_embed(term_to_unsigned_int(t_nnd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

#define PL_EXTENSION_ENTRY(name, arity) { #name, arity, (void*) name, 0 },

static PL_extension predicates[] = {
  PL_EXTENSION_ENTRY(ppl_new_polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_new_empty_polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_copy_polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_delete_polyhedron, 1)
  PL_EXTENSION_ENTRY(ppl_space_dimension, 2)
  PL_EXTENSION_ENTRY(ppl_insert_constraint, 2)
  PL_EXTENSION_ENTRY(ppl_insert_generator, 2)
  PL_EXTENSION_ENTRY(ppl_add_constraints_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_check_empty, 1)
  PL_EXTENSION_ENTRY(ppl_intersection_assign, 2)
  PL_EXTENSION_ENTRY(ppl_intersection_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_convex_hull_assign, 2)
  PL_EXTENSION_ENTRY(ppl_convex_hull_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_convex_difference_assign, 2)
  PL_EXTENSION_ENTRY(ppl_convex_difference_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_widening_assign, 2)
  PL_EXTENSION_ENTRY(ppl_get_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_get_generators, 2)
  PL_EXTENSION_ENTRY(ppl_remove_dimensions, 2)
  PL_EXTENSION_ENTRY(ppl_remove_higher_dimensions, 2)
  PL_EXTENSION_ENTRY(ppl_add_dimensions_and_project, 2)
  PL_EXTENSION_ENTRY(ppl_add_dimensions_and_embed, 2)
  { NULL, 0, NULL, 0 }
};

install_t
install() {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i) {
    Prolog_atom a = Prolog_atom_from_string(prolog_atoms[i].name);
    *prolog_atoms[i].p_atom = a;
  }
  PL_register_extensions(predicates);
}
