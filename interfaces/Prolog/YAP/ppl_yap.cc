/* YAP Prolog interface: system-dependent part.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_install.hh"
// YAP 4.3.20 misses the `extern "C"' wrapper.
#define template templ
extern "C" {
#include <Yap/c_interface.h>
}
#undef template
#include <cassert>

#define GNU 1
#define PARANOID 1

typedef Term Prolog_term_ref;
typedef Atom Prolog_atom;
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
  return 0;
  //return Mk_Variable();
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
Prolog_put_long(Prolog_term_ref& t, long i) {
  t = MkIntTerm(i);
  return true;
}

/*!
  Assign to \p t an atom whose name is given by the null-terminated string \s.
*/
static inline bool
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  // FIXME: the following cast is really a bug in YAP.
  t = MkAtomTerm(FullLookupAtom(const_cast<char*>(s)));
  return true;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline bool
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  t = MkAtomTerm(a);
  return true;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline bool
Prolog_put_address(Prolog_term_ref& t, void* p) {
  t = MkIntTerm(reinterpret_cast<long>(p));
  return true;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  // FIXME: the following cast is really a bug in YAP.
  return FullLookupAtom(const_cast<char*>(s));
}

static Prolog_term_ref args[4];

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1) {
  args[0] = a1;
  t = MkApplTerm(f, 1, args);
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  args[0] = a1;
  args[1] = a2;
  t = MkApplTerm(f, 2, args);
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  args[0] = a1;
  args[1] = a2;
  args[2] = a3;
  t = MkApplTerm(f, 3, args);
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  args[0] = a1;
  args[1] = a2;
  args[2] = a3;
  args[3] = a4;
  t = MkApplTerm(f, 4, args);
  return true;
}

/*!
  Assign to \p l a Prolog list whose head is \p h and tail is \p t. 
*/
static inline bool
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  c = MkPairTerm(h, t);
  return true;
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
  return IsVarTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise. 
*/
static inline bool
Prolog_is_integer(Prolog_term_ref t) {
  return IsIntTerm(t) != FALSE;
}

/*!
  Return true if \p t is the representation of an address, false otherwise. 
*/
static inline bool
Prolog_is_address(Prolog_term_ref t) {
  return IsIntTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise. 
*/
static inline bool
Prolog_is_compound(Prolog_term_ref t) {
  return IsApplTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise. 
*/
static inline bool
Prolog_is_cons(Prolog_term_ref t) {
  return IsPairTerm(t) != FALSE;
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
  v = IntOfTerm(t);
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
  p = reinterpret_cast<void*>(IntOfTerm(t));
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
  Functor f = FunctorOfTerm(t);
  name = NameOfFunctor(f);
  arity = ArityOfFunctor(f);
  return true;
}

/*!
  If \p t is a Prolog compound term and \p i is a positive integer
  less than or equal to its arity, return true and assign to \p a the
  i-th (principal) argument of \p t.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline bool
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref& a) {
  assert(Prolog_is_compound(t));
  a = ArgOfTerm(i, t);
  return true;
}

/*!
  If \p l is a Prolog list, assign its head and tail to \p h and \p t,
  respectively.
  The behavior is undefined if \p l is not a Prolog list.
*/
static inline bool
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref& h, Prolog_term_ref& t) {
  assert(Prolog_is_cons(t));
  h = HeadOfTerm(c);
  t = TailOfTerm(c);
  return true;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline bool
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return unify(t, u) != 0;
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does YAP support unlimited precision integer?
  long v;
  Prolog_get_long(t, v);
  return PPL::Integer(v);
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  // FIXME: does YAP support unlimited precision integer?
  if (!n.fits_slong_p())
    throw_unknown_interface_error("Integer_to_integer_term()");
  return MkIntTerm(n.get_si());
}

static void
ppl_Prolog_sysdep_init() {
}

#include "../ppl_prolog.icc"

#define YAP_USER_C_PREDICATE(name, arity) \
 UserCPredicate(#name, reinterpret_cast<int(*)()>(name), arity)

extern "C" void
init() {
  ppl_init();
  YAP_USER_C_PREDICATE(ppl_initialize, 0);
  YAP_USER_C_PREDICATE(ppl_finalize, 0);
  YAP_USER_C_PREDICATE(ppl_new_C_Polyhedron_from_dimension, 2);
  YAP_USER_C_PREDICATE(ppl_new_NNC_Polyhedron_from_dimension, 2);
  YAP_USER_C_PREDICATE(ppl_new_C_Polyhedron_empty_from_dimension, 2);
  YAP_USER_C_PREDICATE(ppl_new_NNC_Polyhedron_empty_from_dimension, 2);
  YAP_USER_C_PREDICATE(ppl_new_C_Polyhedron_from_C_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_new_C_Polyhedron_from_NNC_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_new_NNC_Polyhedron_from_C_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_new_NNC_Polyhedron_from_NNC_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_new_C_Polyhedron_from_ConSys, 2);
  YAP_USER_C_PREDICATE(ppl_new_NNC_Polyhedron_from_ConSys, 2);
  YAP_USER_C_PREDICATE(ppl_new_C_Polyhedron_from_GenSys, 2);
  YAP_USER_C_PREDICATE(ppl_new_NNC_Polyhedron_from_GenSys, 2);
  YAP_USER_C_PREDICATE(ppl_delete_Polyhedron, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_space_dimension, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_intersection_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_intersection_assign_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_convex_hull_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_convex_hull_assign_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_convex_difference_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_convex_difference_assign_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_widening_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_limited_widening_assign, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraint, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generator, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraints, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraints_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generators, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generators_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_constraints, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_project, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_embed, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_remove_dimensions, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_remove_higher_dimensions, 2);
  YAP_USER_C_PREDICATE(ppl_check_empty, 1);
  YAP_USER_C_PREDICATE(ppl_check_universe, 1);
  YAP_USER_C_PREDICATE(ppl_is_bounded, 1);
  YAP_USER_C_PREDICATE(ppl_get_constraints, 2);
  YAP_USER_C_PREDICATE(ppl_get_generators, 2);
  YAP_USER_C_PREDICATE(ppl_polyhedron_included_or_equal, 2);
  YAP_USER_C_PREDICATE(ppl_polyhedron_equal, 2);
  YAP_USER_C_PREDICATE(ppl_polyhedron_strictly_included, 2);
  YAP_USER_C_PREDICATE(ppl_relation_with_constraint, 3);
  YAP_USER_C_PREDICATE(ppl_relation_with_generator, 3);
  YAP_USER_C_PREDICATE(ppl_affine_image, 4);
  YAP_USER_C_PREDICATE(ppl_affine_preimage, 4);
}
