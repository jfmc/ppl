
#include "ppl_install.hh"
// GNU Prolog 1.2.8 uses the C++ reserved word "template" in gprolog.h.
// Moreover, it misses the `extern "C"' wrapper.
#define template templ
extern "C" {
#include <gprolog.h>
}
#undef template
#include <cassert>

#define GNU 1
#define PARANOID 1

typedef PlTerm Prolog_term_ref;
typedef int Prolog_atom;
typedef Bool Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = TRUE;
static const Prolog_foreign_return_type PROLOG_FAILURE = FALSE;

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

/*!
  Return a new term reference.
*/
static inline Prolog_term_ref
Prolog_new_term_ref() {
  return Mk_Variable();
}

/*!
  Make \p t be a reference to the same term referenced by \p u,
  i.e., assign \p u to \p t.
*/
static inline bool
Prolog_put_term(Prolog_term_ref& t, Prolog_term_ref u) {
  t = u;
  return true;
}

/*!
  Assign to \p t a Prolog integer with value \p i.
*/
static inline bool
Prolog_put_long(Prolog_term_ref t, long i) {
  return Un_Integer(i, t) != FALSE;
}

/*!
  Assign to \p t an atom whose name is given by the null-terminated string \s.
*/
static inline bool
Prolog_put_atom_chars(Prolog_term_ref t, const char* s) {
  // FIXME: the following cast is really a bug in GNU Prolog.
  return Un_Chars(const_cast<char*>(s), t) != FALSE;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline bool
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  return Un_Atom(a, t) != FALSE;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline bool
Prolog_put_address(Prolog_term_ref t, void* p) {
  return Un_Integer(reinterpret_cast<long>(p), t) != FALSE;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  // FIXME: the following cast is really a bug in GNU Prolog.
  return Create_Allocate_Atom(const_cast<char*>(s));
}

static Prolog_term_ref args[4];

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1) {
  args[0] = a1;
  return Un_Compound(f, 1, args, t) != FALSE;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  args[0] = a1;
  args[1] = a2;
  return Un_Compound(f, 2, args, t) != FALSE;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  args[0] = a1;
  args[1] = a2;
  args[2] = a3;
  return Un_Compound(f, 3, args, t) != FALSE;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  args[0] = a1;
  args[1] = a2;
  args[2] = a3;
  args[3] = a4;
  return Un_Compound(f, 4, args, t) != FALSE;
}

/*!
  Assign to \p l a Prolog list whose head is \p h and tail is \p t. 
*/
static inline bool
Prolog_construct_list(Prolog_term_ref l,
		      Prolog_term_ref h, Prolog_term_ref t) {
  args[0] = h;
  args[1] = t;
  return Un_List(args, l) != FALSE;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  // ???
  // (void) PL_raise_exception(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise. 
*/
static inline bool
Prolog_is_variable(Prolog_term_ref t) {
  return Blt_Var(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise. 
*/
static inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return Blt_Integer(t) != FALSE;
}

/*!
  Return true if \p t is the representation of an address, false otherwise. 
*/
static inline bool
Prolog_is_address(Prolog_term_ref t) {
  return Blt_Integer(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise. 
*/
static inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return Blt_Compound(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise. 
*/
static inline bool
Prolog_is_list(Prolog_term_ref t) {
  return Blt_List(t) != FALSE;
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
  v = Rd_Integer(t);
  return true;
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
static inline bool
Prolog_get_address(Prolog_term_ref t, void*& p) {
  assert(Prolog_is_address(t));
  p = reinterpret_cast<void*>(Rd_Integer(t));
  return true;
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline bool
Prolog_get_name_arity(Prolog_term_ref t, Prolog_atom& name, int& arity) {
  assert(Prolog_is_compound(t));
  Rd_Compound(t, &name, &arity);
  return true;
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
  static Prolog_atom dummy_name;
  static int dummy_arity;
  a = Rd_Compound(t, &dummy_name, &dummy_arity)[i-1];
  return true;
}

/*!
  If \p l is a Prolog list, assign its head and tail to \p h and \p t,
  respectively.
  The behavior is undefined if \p l is not a Prolog list.
*/
static inline bool
Prolog_get_list(Prolog_term_ref l, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_list(t));
  Prolog_term_ref* ht = Rd_List(l);
  h = ht[0];
  t = ht[1];
  return true;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline bool
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return Unify(t, u) != FALSE;
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
  return Mk_Integer(n.get_si());
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

extern "C" Prolog_foreign_return_type
ppl_init() {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i) {
    Prolog_atom a = Prolog_atom_from_string(prolog_atoms[i].name);
    *prolog_atoms[i].p_atom = a;
  }
  return PROLOG_SUCCESS;
}
