/* YAP Prolog interface: system-dependent part.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <config.h>

#include "Integer.defs.hh"
#include <Yap/YapInterface.h>
#include <cassert>

typedef YAP_Term Prolog_term_ref;
typedef YAP_Atom Prolog_atom;
typedef YAP_Bool Prolog_foreign_return_type;
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
}

/*!
  Make \p t be a reference to the same term referenced by \p u,
  i.e., assign \p u to \p t.
*/
static inline int
Prolog_put_term(Prolog_term_ref& t, Prolog_term_ref u) {
  t = u;
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p i.
*/
static inline int
Prolog_put_long(Prolog_term_ref& t, long i) {
  t = YAP_MkIntTerm(i);
  return 1;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
static inline int
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  // FIXME: the following cast is really a bug in YAP.
  t = YAP_MkAtomTerm(YAP_FullLookupAtom(const_cast<char*>(s)));
  return 1;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline int
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  t = YAP_MkAtomTerm(a);
  return 1;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline int
Prolog_put_address(Prolog_term_ref& t, void* p) {
  t = YAP_MkIntTerm(reinterpret_cast<long>(p));
  return 1;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  // FIXME: the following cast is really a bug in YAP.
  return YAP_FullLookupAtom(const_cast<char*>(s));
}

static Prolog_term_ref args[4];

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
static inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1) {
  args[0] = a1;
  t = YAP_MkApplTerm(YAP_MkFunctor(f, 1), 1, args);
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
static inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  args[0] = a1;
  args[1] = a2;
  t = YAP_MkApplTerm(YAP_MkFunctor(f, 2), 2, args);
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
static inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  args[0] = a1;
  args[1] = a2;
  args[2] = a3;
  t = YAP_MkApplTerm(YAP_MkFunctor(f, 3), 3, args);
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
static inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  args[0] = a1;
  args[1] = a2;
  args[2] = a3;
  args[3] = a4;
  t = YAP_MkApplTerm(YAP_MkFunctor(f, 4), 4, args);
  return 1;
}

/*!
  Assign to \p c a Prolog list whose head is \p h and tail is \p t.
*/
static inline int
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  c = YAP_MkPairTerm(h, t);
  return 1;
}

static Prolog_atom a_throw;

static void
ppl_Prolog_sysdep_init() {
  a_throw = YAP_LookupAtom("throw");
}

static void
ppl_Prolog_sysdep_deinit() {
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  YAP_Throw(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise.
*/
static inline int
Prolog_is_variable(Prolog_term_ref t) {
  return YAP_IsVarTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog atom, false otherwise.
*/
static inline int
Prolog_is_atom(Prolog_term_ref t) {
  return YAP_IsAtomTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise.
*/
static inline int
Prolog_is_integer(Prolog_term_ref t) {
  return YAP_IsIntTerm(t) != FALSE;
}

/*!
  Return true if \p t is the representation of an address, false otherwise.
*/
static inline int
Prolog_is_address(Prolog_term_ref t) {
  return YAP_IsIntTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise.
*/
static inline int
Prolog_is_compound(Prolog_term_ref t) {
  return YAP_IsApplTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise.
*/
static inline int
Prolog_is_cons(Prolog_term_ref t) {
  return YAP_IsPairTerm(t) != FALSE;
}

/*!
  Assuming \p t is a Prolog integer, return true if its value fits
  in a long, in which case the value is assigned to \p v,
  return false otherwise.  The behavior is undefined if \p t is
  not a Prolog integer.
*/
static inline int
Prolog_get_long(Prolog_term_ref t, long* lp) {
  assert(Prolog_is_integer(t));
  *lp = YAP_IntOfTerm(t);
  return 1;
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
static inline int
Prolog_get_address(Prolog_term_ref t, void** vpp) {
  assert(Prolog_is_address(t));
  *vpp = reinterpret_cast<void*>(YAP_IntOfTerm(t));
  return 1;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
static inline int
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom* ap) {
  assert(Prolog_is_atom(t));
  *ap = YAP_AtomOfTerm(t);
  return 1;
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline int
Prolog_get_compound_name_arity(Prolog_term_ref t, Prolog_atom* ap, int* ip) {
  assert(Prolog_is_compound(t));
  YAP_Functor f = YAP_FunctorOfTerm(t);
  *ap = YAP_NameOfFunctor(f);
  *ip = YAP_ArityOfFunctor(f);
  return 1;
}

/*!
  If \p t is a Prolog compound term and \p i is a positive integer
  less than or equal to its arity, return true and assign to \p a the
  i-th (principal) argument of \p t.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline int
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref& a) {
  assert(Prolog_is_compound(t));
  a = YAP_ArgOfTerm(i, t);
  return 1;
}

/*!
  If \p c is a Prolog cons (list constructor), assign its head and
  tail to \p h and \p t, respectively.
  The behavior is undefined if \p c is not a Prolog cons.
*/
static inline int
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref& h, Prolog_term_ref& t) {
  assert(Prolog_is_cons(c));
  h = YAP_HeadOfTerm(c);
  t = YAP_TailOfTerm(c);
  return 1;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline int
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return YAP_Unify(t, u) != FALSE;
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does YAP support unlimited precision integer?
  long v;
  Prolog_get_long(t, &v);
  return PPL::Integer(v);
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  // FIXME: does YAP support unlimited precision integer?
  if (!n.fits_slong_p())
    throw_unknown_interface_error("Integer_to_integer_term()");
  return YAP_MkIntTerm(n.get_si());
}

#include "../ppl_prolog.icc"

#define YAP_STUB_0(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  return name(); \
}

#define YAP_STUB_1(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  Prolog_term_ref arg1 = YAP_ARG1; \
  return name(arg1); \
}

#define YAP_STUB_2(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  Prolog_term_ref arg1 = YAP_ARG1; \
  Prolog_term_ref arg2 = YAP_ARG2; \
  return name(arg1, arg2); \
}

#define YAP_STUB_3(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  Prolog_term_ref arg1 = YAP_ARG1; \
  Prolog_term_ref arg2 = YAP_ARG2; \
  Prolog_term_ref arg3 = YAP_ARG3; \
  return name(arg1, arg2, arg3); \
}

#define YAP_STUB_4(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  Prolog_term_ref arg1 = YAP_ARG1; \
  Prolog_term_ref arg2 = YAP_ARG2; \
  Prolog_term_ref arg3 = YAP_ARG3; \
  Prolog_term_ref arg4 = YAP_ARG4; \
  return name(arg1, arg2, arg3, arg4); \
}

#define YAP_STUB_5(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  Prolog_term_ref arg1 = YAP_ARG1; \
  Prolog_term_ref arg2 = YAP_ARG2; \
  Prolog_term_ref arg3 = YAP_ARG3; \
  Prolog_term_ref arg4 = YAP_ARG4; \
  Prolog_term_ref arg5 = YAP_ARG5; \
  return name(arg1, arg2, arg3, arg4, arg5); \
}

YAP_STUB_1(ppl_version_major)
YAP_STUB_1(ppl_version_minor)
YAP_STUB_1(ppl_version_revision)
YAP_STUB_1(ppl_version_beta)
YAP_STUB_1(ppl_version)
YAP_STUB_1(ppl_max_space_dimension)
YAP_STUB_0(ppl_initialize)
YAP_STUB_0(ppl_finalize)
YAP_STUB_1(ppl_set_timeout_exception_atom)
YAP_STUB_1(ppl_timeout_exception_atom)
YAP_STUB_1(ppl_set_timeout)
YAP_STUB_0(ppl_reset_timeout)
YAP_STUB_3(ppl_new_Polyhedron_from_dimension)
YAP_STUB_3(ppl_new_Polyhedron_empty_from_dimension)
YAP_STUB_4(ppl_new_Polyhedron_from_Polyhedron)
YAP_STUB_3(ppl_new_Polyhedron_from_constraints)
YAP_STUB_3(ppl_new_Polyhedron_from_generators)
YAP_STUB_3(ppl_new_Polyhedron_from_bounding_box)
YAP_STUB_2(ppl_Polyhedron_swap)
YAP_STUB_1(ppl_delete_Polyhedron)
YAP_STUB_2(ppl_Polyhedron_space_dimension)
YAP_STUB_2(ppl_Polyhedron_get_constraints)
YAP_STUB_2(ppl_Polyhedron_get_minimized_constraints)
YAP_STUB_2(ppl_Polyhedron_get_generators)
YAP_STUB_2(ppl_Polyhedron_get_minimized_generators)
YAP_STUB_3(ppl_Polyhedron_relation_with_constraint)
YAP_STUB_3(ppl_Polyhedron_relation_with_generator)
YAP_STUB_3(ppl_Polyhedron_get_bounding_box)
YAP_STUB_1(ppl_Polyhedron_is_empty)
YAP_STUB_1(ppl_Polyhedron_is_universe)
YAP_STUB_1(ppl_Polyhedron_is_bounded)
YAP_STUB_2(ppl_Polyhedron_bounds_from_above)
YAP_STUB_2(ppl_Polyhedron_bounds_from_below)
YAP_STUB_5(ppl_Polyhedron_maximize)
YAP_STUB_5(ppl_Polyhedron_minimize)
YAP_STUB_1(ppl_Polyhedron_is_topologically_closed)
YAP_STUB_2(ppl_Polyhedron_contains_Polyhedron)
YAP_STUB_2(ppl_Polyhedron_strictly_contains_Polyhedron)
YAP_STUB_2(ppl_Polyhedron_is_disjoint_from_Polyhedron)
YAP_STUB_2(ppl_Polyhedron_equals_Polyhedron)
YAP_STUB_1(ppl_Polyhedron_OK)
YAP_STUB_2(ppl_Polyhedron_add_constraint)
YAP_STUB_2(ppl_Polyhedron_add_constraint_and_minimize)
YAP_STUB_2(ppl_Polyhedron_add_generator)
YAP_STUB_2(ppl_Polyhedron_add_generator_and_minimize)
YAP_STUB_2(ppl_Polyhedron_add_constraints)
YAP_STUB_2(ppl_Polyhedron_add_constraints_and_minimize)
YAP_STUB_2(ppl_Polyhedron_add_generators)
YAP_STUB_2(ppl_Polyhedron_add_generators_and_minimize)
YAP_STUB_2(ppl_Polyhedron_intersection_assign)
YAP_STUB_2(ppl_Polyhedron_intersection_assign_and_minimize)
YAP_STUB_2(ppl_Polyhedron_poly_hull_assign)
YAP_STUB_2(ppl_Polyhedron_poly_hull_assign_and_minimize)
YAP_STUB_2(ppl_Polyhedron_poly_difference_assign)
YAP_STUB_4(ppl_Polyhedron_affine_image)
YAP_STUB_4(ppl_Polyhedron_affine_preimage)
YAP_STUB_5(ppl_Polyhedron_generalized_affine_image)
YAP_STUB_4(ppl_Polyhedron_generalized_affine_image_lhs_rhs)
YAP_STUB_2(ppl_Polyhedron_time_elapse_assign)
YAP_STUB_1(ppl_Polyhedron_topological_closure_assign)
YAP_STUB_3(ppl_Polyhedron_BHRZ03_widening_assign_with_token)
YAP_STUB_2(ppl_Polyhedron_BHRZ03_widening_assign)
YAP_STUB_4(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token)
YAP_STUB_3(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign)
YAP_STUB_4(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token)
YAP_STUB_3(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign)
YAP_STUB_3(ppl_Polyhedron_H79_widening_assign_with_token)
YAP_STUB_2(ppl_Polyhedron_H79_widening_assign)
YAP_STUB_4(ppl_Polyhedron_limited_H79_extrapolation_assign_with_token)
YAP_STUB_3(ppl_Polyhedron_limited_H79_extrapolation_assign)
YAP_STUB_4(ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token)
YAP_STUB_3(ppl_Polyhedron_bounded_H79_extrapolation_assign)
YAP_STUB_2(ppl_Polyhedron_add_dimensions_and_project)
YAP_STUB_2(ppl_Polyhedron_add_dimensions_and_embed)
YAP_STUB_2(ppl_Polyhedron_concatenate_assign)
YAP_STUB_2(ppl_Polyhedron_remove_dimensions)
YAP_STUB_2(ppl_Polyhedron_remove_higher_dimensions)
YAP_STUB_2(ppl_Polyhedron_map_dimensions)

#define YAP_USER_C_PREDICATE(name, arity) \
 YAP_UserCPredicate(#name, reinterpret_cast<int(*)()>(yap_stub_##name), arity)

extern "C" void
init() {
  ppl_initialize();
  YAP_USER_C_PREDICATE(ppl_version_major, 1);
  YAP_USER_C_PREDICATE(ppl_version_minor, 1);
  YAP_USER_C_PREDICATE(ppl_version_revision, 1);
  YAP_USER_C_PREDICATE(ppl_version_beta, 1);
  YAP_USER_C_PREDICATE(ppl_version, 1);
  YAP_USER_C_PREDICATE(ppl_max_space_dimension, 1);
  YAP_USER_C_PREDICATE(ppl_initialize, 0);
  YAP_USER_C_PREDICATE(ppl_finalize, 0);
  YAP_USER_C_PREDICATE(ppl_set_timeout_exception_atom, 1);
  YAP_USER_C_PREDICATE(ppl_timeout_exception_atom, 1);
  YAP_USER_C_PREDICATE(ppl_set_timeout, 1);
  YAP_USER_C_PREDICATE(ppl_reset_timeout, 0);
  YAP_USER_C_PREDICATE(ppl_new_Polyhedron_from_dimension, 3);
  YAP_USER_C_PREDICATE(ppl_new_Polyhedron_empty_from_dimension, 3);
  YAP_USER_C_PREDICATE(ppl_new_Polyhedron_from_Polyhedron, 4);
  YAP_USER_C_PREDICATE(ppl_new_Polyhedron_from_constraints, 3);
  YAP_USER_C_PREDICATE(ppl_new_Polyhedron_from_generators, 3);
  YAP_USER_C_PREDICATE(ppl_new_Polyhedron_from_bounding_box, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_swap, 2);
  YAP_USER_C_PREDICATE(ppl_delete_Polyhedron, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_space_dimension, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_get_constraints, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_get_minimized_constraints, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_get_generators, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_get_minimized_generators, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_relation_with_constraint, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_relation_with_generator, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_get_bounding_box, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_is_empty, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_is_universe, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_is_bounded, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_bounds_from_above, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_bounds_from_below, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_maximize, 5);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_minimize, 5);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_is_topologically_closed, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_contains_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_strictly_contains_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_is_disjoint_from_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_equals_Polyhedron, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_OK, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraint, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraint_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generator, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generator_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraints, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_constraints_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generators, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_generators_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_intersection_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_intersection_assign_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_poly_hull_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_poly_hull_assign_and_minimize, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_poly_difference_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_affine_image, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_affine_preimage, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_generalized_affine_image, 5);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_generalized_affine_image_lhs_rhs, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_time_elapse_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_topological_closure_assign, 1);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_BHRZ03_widening_assign_with_token, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_BHRZ03_widening_assign, 2);
  YAP_USER_C_PREDICATE(
          ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign, 3);
  YAP_USER_C_PREDICATE(
          ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_H79_widening_assign_with_token, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_H79_widening_assign, 2);
  YAP_USER_C_PREDICATE(
          ppl_Polyhedron_limited_H79_extrapolation_assign_with_token, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_limited_H79_extrapolation_assign, 3);
  YAP_USER_C_PREDICATE(
          ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token, 4);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_bounded_H79_extrapolation_assign, 3);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_project, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_embed, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_concatenate_assign, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_remove_dimensions, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_remove_higher_dimensions, 2);
  YAP_USER_C_PREDICATE(ppl_Polyhedron_map_dimensions, 2);
}
