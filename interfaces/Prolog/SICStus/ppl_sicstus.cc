
#include "ppl_install.hh"
#include <sicstus/sicstus.h>
#include <cassert>

#define SICSTUS 1
#define PARANOID 1

#ifdef PARANOID

#define CHECK_STATUS(f) \
  do { if (!(f)) throw_unknown_interface_error("???"); } while(false)

#else

#define CHECK_STATUS(f) do { f; } while(false)

#endif

namespace PPL = Parma_Polyhedra_Library;

typedef SP_term_ref Prolog_term_ref;
typedef SP_atom Prolog_atom;
typedef int Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = SP_SUCCESS;
static const Prolog_foreign_return_type PROLOG_FAILURE = SP_FAILURE;

#include "../exceptions.hh"

/*!
  Return a new term reference.
*/
static inline Prolog_term_ref
Prolog_new_term_ref() {
  return SP_new_term_ref();
}

/*!
  Make \p t be a reference to the same term referenced by \p u,
  i.e., assign \p u to \p t.
*/
static inline bool
Prolog_put_term(Prolog_term_ref t, Prolog_term_ref u) {
  return SP_put_term(t, u) != 0;
}

/*!
  Assign to \p t a Prolog integer with value \p i.
*/
static inline bool
Prolog_put_long(Prolog_term_ref t, long i) {
  return SP_put_integer(t, i) != 0;
}

/*!
  Assign to \p t an atom whose name is given by the null-terminated string \s.
*/
static inline bool
Prolog_put_atom_chars(Prolog_term_ref t, const char* s) {
  return SP_put_string(t, s) != 0;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline bool
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  return SP_put_atom(t, a) != 0;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline bool
Prolog_put_address(Prolog_term_ref t, void* p) {
  return SP_put_address(t, p) != 0;
}

/*!
  Return an atom whose name is given by the null-terminated sring \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  return SP_atom_from_string(s);
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1) {
  return SP_cons_functor(t, f, 1, a1) != 0;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  return SP_cons_functor(t, f, 2, a1, a2) != 0;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  return SP_cons_functor(t, f, 3, a1, a2, a3) != 0;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  return SP_cons_functor(t, f, 4, a1, a2, a3, a4) != 0;
}


/*!
  Assign to \p l a Prolog list whose head is \p h and tail is \p t. 
*/
static inline bool
Prolog_construct_list(Prolog_term_ref l,
		      Prolog_term_ref h, Prolog_term_ref t) {
  return SP_cons_list(l, h, t) != 0;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  SP_raise_exception(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise. 
*/
static inline bool
Prolog_is_variable(Prolog_term_ref t) {
  return SP_is_variable(t) != 0;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise. 
*/
static inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return SP_is_integer(t) != 0;
}

/*!
  Return true if \p t is the representation of an address, false otherwise. 
*/
static inline bool
Prolog_is_address(Prolog_term_ref t) {
  return SP_is_integer(t) != 0;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise. 
*/
static inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return SP_is_compound(t) != 0;
}

/*!
  Return true if \p t is a Prolog list, false otherwise. 
*/
static inline bool
Prolog_is_list(Prolog_term_ref t) {
  return SP_is_list(t) != 0;
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
  return SP_get_integer(t, &v) != 0;
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into to \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
static inline bool
Prolog_get_address(Prolog_term_ref t, void** p) {
  assert(Prolog_is_address(t));
  return SP_get_address(t, p) != 0;
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline bool
Prolog_get_name_arity(Prolog_term_ref t, Prolog_atom& name, int& arity) {
  assert(Prolog_is_compound(t));
  return SP_get_functor(t, &name, &arity) != 0;
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
  return SP_get_arg(i, t, a) != 0;
}

/*!
  If \p l is a Prolog list, assign its head and tail to \p h and \p t,
  respectively.
  The behavior is undefined if \p l is not a Prolog list.
*/
static inline bool
Prolog_get_list(Prolog_term_ref l, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_list(t));
  return SP_get_list(l, h, t) != 0;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline bool
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return SP_unify(t, u) != 0;
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  assert(SP_is_integer(t));
  long v;
  if (SP_get_integer(t, &v) != 0)
    return PPL::Integer(v);
  else {
    char* s;
    if (SP_get_number_chars(t, &s) != 0)
      return PPL::Integer(s);
    else
      throw integer_out_of_range(t);
  }
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  Prolog_term_ref t = Prolog_new_term_ref();
  if (n.fits_slong_p())
    CHECK_STATUS(SP_put_integer(t, n.get_si()));
  else {
    std::string s = n.get_str();
    CHECK_STATUS(SP_put_number_chars(t, s.c_str()));
  }
  return t;
}

#include "../ppl_prolog.outlines.hh"

extern "C" void
ppl_init(int /* when */) {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i) {
    Prolog_atom a = SP_atom_from_string(prolog_atoms[i].name);
    if (SP_register_atom(a) == 0) {
      Prolog_term_ref et = Prolog_new_term_ref();
      Prolog_put_atom_chars(et, "Cannot initialize the PPL interface");
      Prolog_raise_exception(et);
      return;
    }
    *prolog_atoms[i].p_atom = a;
  }
}

extern "C" void
ppl_deinit(int /* when */) {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i)
    // SP_unregister_atom can fail.
    // We ignore such failures: what else can we do?
    (void) SP_unregister_atom(*prolog_atoms[i].p_atom);
}

extern "C" void
ppl_new_polyhedron(Prolog_term_ref t_ph, long nd) {
  try {
    if (!Prolog_is_variable(t_ph))
      throw not_a_variable(t_ph);
    PPL::Polyhedron* ph = new PPL::Polyhedron(get_unsigned_int(nd));
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp))
      REGISTER(ph);
    else {
      delete ph;
      throw unknown_interface_error("ppl_new_polyhedron/2");
    }
  }
  CATCH_ALL;
}

extern "C" void*
ppl_new_empty_polyhedron(long num_dimensions) {
  try {
    PPL::Polyhedron* ret
      = new PPL::Polyhedron(get_unsigned_int(num_dimensions),
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
  CATCH_ALL;
}

extern "C" long
ppl_space_dimension(const void* pp) {
  CHECK(pp);
  // Polyhedron::space_dimension() cannot throw.
  return static_cast<const PPL::Polyhedron*>(pp)->space_dimension();
}

extern "C" void
ppl_insert_constraint(void* pp, Prolog_term_ref t) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)->insert(build_constraint(t));
  }
  CATCH_ALL;
}

extern "C" long
ppl_add_constraints_and_minimize(void* pp, Prolog_term_ref t_clist) {
  try {
    CHECK(pp);
    PPL::ConSys cs;
    Prolog_term_ref c = Prolog_new_term_ref();
    while (Prolog_is_list(t_clist)) {
      Prolog_get_list(t_clist, c, t_clist);
      cs.insert(build_constraint(c));
    }
    PPL::Polyhedron& ph = *static_cast<PPL::Polyhedron*>(pp);
    return ph.add_constraints_and_minimize(cs) ? 1 : 0;
  }
  CATCH_ALL;
  return -1;
}

extern "C" void
ppl_insert_generator(void* pp, Prolog_term_ref t) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)->insert(build_generator(t));
  }
  CATCH_ALL;
}

extern "C" long
ppl_check_empty(const void* pp) {
  try {
    CHECK(pp);
    return static_cast<const PPL::Polyhedron*>(pp)->check_empty() ? 1 : 0;
  }
  CATCH_ALL;
  return -1;
}

extern "C" void
ppl_intersection_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(pp_lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(pp_rhs);
    x.intersection_assign(y);
  }
  CATCH_ALL;
}

extern "C" void
ppl_convex_hull_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(pp_lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(pp_rhs);
    x.convex_hull_assign_and_minimize(y);
  }
  CATCH_ALL;
}

extern "C" void
ppl_convex_difference_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(pp_lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(pp_rhs);
    x.convex_difference_assign_and_minimize(y);
  }
  CATCH_ALL;
}

extern "C" void
ppl_widening_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    static_cast<PPL::Polyhedron*>(pp_lhs)
      ->widening_assign(*static_cast<const PPL::Polyhedron*>(pp_rhs));
  }
  CATCH_ALL;
}

extern "C" void
ppl_get_constraints(const void* pp, Prolog_term_ref constraints_list) {
  try {
    CHECK(pp);
    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);

    const PPL::Polyhedron& ph = *static_cast<const PPL::Polyhedron*>(pp);
    const PPL::ConSys& cs = ph.constraints();

    for (PPL::ConSys::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      Prolog_term_ref new_tail = Prolog_new_term_ref();
      Prolog_construct_list(new_tail, constraint_term(*i), tail);
      tail = new_tail;
    }

    Prolog_put_term(constraints_list, tail);
  }
  CATCH_ALL;
}

extern "C" void
ppl_get_generators(const void* pp, Prolog_term_ref generators_list) {
  try {
    CHECK(pp);
    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);

    const PPL::Polyhedron& ph = *static_cast<const PPL::Polyhedron*>(pp);
    const PPL::GenSys& gs = ph.generators();

    for (PPL::GenSys::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i) {
      Prolog_term_ref new_tail = Prolog_new_term_ref();
      Prolog_construct_list(new_tail, generator_term(*i), tail);
      tail = new_tail;
    }

    Prolog_put_term(generators_list, tail);
  }
  CATCH_ALL;
}

extern "C" void
ppl_remove_dimensions(void* pp, Prolog_term_ref variables_list) {
  try {
    CHECK(pp);
    std::set<PPL::Variable> dead_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_list(variables_list)) {
      Prolog_get_list(variables_list, v, variables_list);
      dead_variables.insert(get_variable(v));
    }
    static_cast<PPL::Polyhedron*>(pp)->remove_dimensions(dead_variables);
  }
  CATCH_ALL;
}

extern "C" void
ppl_remove_higher_dimensions(void* pp, long new_dimension) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)
      ->remove_higher_dimensions(get_unsigned_int(new_dimension));
  }
  CATCH_ALL;
}

extern "C" void
ppl_add_dimensions_and_project(void* pp, long num_new_dimensions) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)
      ->add_dimensions_and_project(get_unsigned_int(num_new_dimensions));
  }
  CATCH_ALL;
}

extern "C" void
ppl_add_dimensions_and_embed(void* pp, long num_new_dimensions) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)
      ->add_dimensions_and_embed(get_unsigned_int(num_new_dimensions));
  }
  CATCH_ALL;
}
