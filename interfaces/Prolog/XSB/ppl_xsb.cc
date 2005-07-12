/* XSB Prolog interface: system-dependent part.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "Coefficient.defs.hh"
#include "checked.defs.hh"
#include "checked_int.inlines.hh"
#include "checked_mpz.inlines.hh"
#include <cinterf.h>

// In XSB 2.6, <error_xsb.h> does not come with the extern "C" wrapper.
extern "C" {
#include <error_xsb.h>
}

// In XSB 2.6, <cinterf.h> pollutes the namespace with `min' and `max'.
#undef min
#undef max

#include <cassert>

typedef prolog_term Prolog_term_ref;
typedef char* Prolog_atom;
typedef xsbBool Prolog_foreign_return_type;

namespace {

const Prolog_foreign_return_type PROLOG_SUCCESS = TRUE;
const Prolog_foreign_return_type PROLOG_FAILURE = FALSE;

} // namespace

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

namespace {

/*!
  True if and only if the Prolog engine supports unbounded integers.
*/
bool Prolog_has_unbounded_integers;

/*!
  If \p Prolog_has_unbounded_integers is false, holds the minimum
  integer value representable by a Prolog integer.
  Holds zero otherwise.
*/
long Prolog_min_integer;

/*!
  If \p Prolog_has_unbounded_integers is false, holds the maximum
  integer value representable by a Prolog integer.
  Holds zero otherwise.
*/
long Prolog_max_integer;

/*!
  Performs system-dependent initialization.
*/
void
ppl_Prolog_sysdep_init() {
  Prolog_has_unbounded_integers = false;
  // FIXME: these seem to be the values on IA32 but, who knows
  //        on other architectures?
  Prolog_min_integer = -268435456;
  Prolog_max_integer = 268435455;
}

void
ppl_Prolog_sysdep_deinit() {
}

/*!
  Return a new term reference.
*/
inline Prolog_term_ref
Prolog_new_term_ref() {
  return p2p_new();
}

/*!
  Make \p t be a reference to the same term referenced by \p u,
  i.e., assign \p u to \p t.
*/
inline int
Prolog_put_term(Prolog_term_ref& t, Prolog_term_ref u) {
  t = u;
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p l.
*/
inline int
Prolog_put_long(Prolog_term_ref& t, long l) {
  assert(is_var(t) == TRUE);
  if (l < Prolog_min_integer || l > Prolog_max_integer)
    throw PPL_integer_out_of_range(l);
  return c2p_int(l, t) != FALSE;
}

/*!
  Assign to \p t a Prolog integer with value \p ul.
*/
inline int
Prolog_put_ulong(Prolog_term_ref& t, unsigned long ul) {
  assert(is_var(t) == TRUE);
  if (ul > static_cast<unsigned long>(Prolog_max_integer))
    throw PPL_integer_out_of_range(ul);
  return c2p_int(ul, t) != FALSE;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
inline int
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  assert(is_var(t) == TRUE);
  // FIXME: the following cast is really a bug in XSB.
  return c2p_string(string_find(const_cast<char*>(s), 1), t) != FALSE;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
inline int
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  assert(is_var(t) == TRUE);
  return c2p_string(a, t) != FALSE;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
inline int
Prolog_put_address(Prolog_term_ref& t, void* p) {
  assert(is_var(t) == TRUE);
  return c2p_int(reinterpret_cast<long>(p), t) != FALSE;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  // FIXME: the following cast is really a bug in XSB.
  return string_find(const_cast<char*>(s), 1);
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1) {
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 1, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  t = new_compound;
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 2, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  p2p_unify(p2p_arg(new_compound, 2), a2);
  t = new_compound;
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 3, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  p2p_unify(p2p_arg(new_compound, 2), a2);
  p2p_unify(p2p_arg(new_compound, 3), a3);
  t = new_compound;
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 4, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  p2p_unify(p2p_arg(new_compound, 2), a2);
  p2p_unify(p2p_arg(new_compound, 3), a3);
  p2p_unify(p2p_arg(new_compound, 4), a4);
  t = new_compound;
  return 1;
}

/*!
  Assign to \p c a Prolog list whose head is \p h and tail is \p t.
*/
inline int
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  prolog_term new_cons = p2p_new();
  c2p_list(new_cons);
  p2p_unify(p2p_car(new_cons), h);
  p2p_unify(p2p_cdr(new_cons), t);
  c = new_cons;
  return 1;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
inline void
Prolog_raise_exception(Prolog_term_ref t) {
  xsb_throw(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise.
*/
inline int
Prolog_is_variable(Prolog_term_ref t) {
  return is_var(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog atom, false otherwise.
*/
inline int
Prolog_is_atom(Prolog_term_ref t) {
  return is_string(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise.
*/
inline int
Prolog_is_integer(Prolog_term_ref t) {
  return is_int(t) != FALSE;
}

/*!
  Return true if \p t is the representation of an address, false otherwise.
*/
inline int
Prolog_is_address(Prolog_term_ref t) {
  return is_int(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise.
*/
inline int
Prolog_is_compound(Prolog_term_ref t) {
  return is_functor(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise.
*/
inline int
Prolog_is_cons(Prolog_term_ref t) {
  return is_list(t) != FALSE;
}

/*!
  Assuming \p t is a Prolog integer, return true if its value fits
  in a long, in which case the value is assigned to \p v,
  return false otherwise.  The behavior is undefined if \p t is
  not a Prolog integer.
*/
inline int
Prolog_get_long(Prolog_term_ref t, long* lp) {
  assert(Prolog_is_integer(t));
  *lp = p2c_int(t);
  return 1;
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
inline int
Prolog_get_address(Prolog_term_ref t, void** vpp) {
  assert(Prolog_is_address(t));
  *vpp = reinterpret_cast<void*>(p2c_int(t));
  return 1;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
inline int
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom* ap) {
  assert(Prolog_is_atom(t));
  *ap = p2c_string(t);
  return 1;
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
inline int
Prolog_get_compound_name_arity(Prolog_term_ref t, Prolog_atom* ap, int* ip) {
  assert(Prolog_is_compound(t));
  *ap = p2c_functor(t);
  *ip = p2c_arity(t);
  return 1;
}

/*!
  If \p t is a Prolog compound term and \p i is a positive integer
  less than or equal to its arity, return true and assign to \p a the
  i-th (principal) argument of \p t.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
inline int
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref& a) {
  assert(Prolog_is_compound(t));
  a = p2p_arg(t, i);
  return 1;
}

/*!
  If \p c is a Prolog cons (list constructor), assign its head and
  tail to \p h and \p t, respectively.
  The behavior is undefined if \p c is not a Prolog cons.
*/
inline int
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref& h, Prolog_term_ref& t) {
  assert(Prolog_is_cons(c));
  h = p2p_car(c);
  t = p2p_cdr(c);
  return 1;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
inline int
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return p2p_unify(t, u) != FALSE;
 }

PPL::Coefficient
integer_term_to_Coefficient(Prolog_term_ref t) {
  // FIXME: does XSB support unlimited precision integers?
  long v;
  Prolog_get_long(t, &v);
  return PPL::Coefficient(v);
}

Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  long v;
  if (PPL::Checked::assign<PPL::Check_Overflow_Policy>(v, PPL::raw_value(n), PPL::ROUND_IGNORE)
      != PPL::V_EQ)
    throw PPL_integer_out_of_range(n);
  Prolog_term_ref t = p2p_new();
  Prolog_put_long(t, v);
  return t;
}

} // namespace

#define ppl_version_major xsb_stub_ppl_version_major
#define ppl_version_minor xsb_stub_ppl_version_minor
#define ppl_version_revision xsb_stub_ppl_version_revision
#define ppl_version_beta xsb_stub_ppl_version_beta
#define ppl_version xsb_stub_ppl_version
#define ppl_banner xsb_stub_ppl_banner
#define ppl_max_space_dimension xsb_stub_ppl_max_space_dimension
#define ppl_initialize xsb_stub_ppl_initialize
#define ppl_finalize xsb_stub_ppl_finalize
#define ppl_set_timeout_exception_atom xsb_stub_ppl_set_timeout_exception_atom
#define ppl_timeout_exception_atom xsb_stub_ppl_timeout_exception_atom
#define ppl_set_timeout xsb_stub_ppl_set_timeout
#define ppl_reset_timeout xsb_stub_ppl_reset_timeout
#define ppl_new_Polyhedron_from_space_dimension xsb_stub_ppl_new_Polyhedron_from_space_dimension
#define ppl_new_Polyhedron_from_Polyhedron xsb_stub_ppl_new_Polyhedron_from_Polyhedron
#define ppl_new_Polyhedron_from_constraints xsb_stub_ppl_new_Polyhedron_from_constraints
#define ppl_new_Polyhedron_from_generators xsb_stub_ppl_new_Polyhedron_from_generators
#define ppl_new_Polyhedron_from_bounding_box xsb_stub_ppl_new_Polyhedron_from_bounding_box
#define ppl_Polyhedron_swap xsb_stub_ppl_Polyhedron_swap
#define ppl_delete_Polyhedron xsb_stub_ppl_delete_Polyhedron
#define ppl_Polyhedron_space_dimension xsb_stub_ppl_Polyhedron_space_dimension
#define ppl_Polyhedron_affine_dimension xsb_stub_ppl_Polyhedron_affine_dimension
#define ppl_Polyhedron_get_constraints xsb_stub_ppl_Polyhedron_get_constraints
#define ppl_Polyhedron_get_minimized_constraints xsb_stub_ppl_Polyhedron_get_minimized_constraints
#define ppl_Polyhedron_get_generators xsb_stub_ppl_Polyhedron_get_generators
#define ppl_Polyhedron_get_minimized_generators xsb_stub_ppl_Polyhedron_get_minimized_generators
#define ppl_Polyhedron_relation_with_constraint xsb_stub_ppl_Polyhedron_relation_with_constraint
#define ppl_Polyhedron_relation_with_generator xsb_stub_ppl_Polyhedron_relation_with_generator
#define ppl_Polyhedron_get_bounding_box xsb_stub_ppl_Polyhedron_get_bounding_box
#define ppl_Polyhedron_is_empty xsb_stub_ppl_Polyhedron_is_empty
#define ppl_Polyhedron_is_universe xsb_stub_ppl_Polyhedron_is_universe
#define ppl_Polyhedron_is_bounded xsb_stub_ppl_Polyhedron_is_bounded
#define ppl_Polyhedron_bounds_from_above xsb_stub_ppl_Polyhedron_bounds_from_above
#define ppl_Polyhedron_bounds_from_below xsb_stub_ppl_Polyhedron_bounds_from_below
#define ppl_Polyhedron_maximize xsb_stub_ppl_Polyhedron_maximize
#define ppl_Polyhedron_maximize_with_point xsb_stub_ppl_Polyhedron_maximize_with_point
#define ppl_Polyhedron_minimize xsb_stub_ppl_Polyhedron_minimize
#define ppl_Polyhedron_minimize_with_point xsb_stub_ppl_Polyhedron_minimize_with_point
#define ppl_Polyhedron_is_topologically_closed xsb_stub_ppl_Polyhedron_is_topologically_closed
#define ppl_Polyhedron_contains_Polyhedron xsb_stub_ppl_Polyhedron_contains_Polyhedron
#define ppl_Polyhedron_strictly_contains_Polyhedron xsb_stub_ppl_Polyhedron_strictly_contains_Polyhedron
#define ppl_Polyhedron_is_disjoint_from_Polyhedron xsb_stub_ppl_Polyhedron_is_disjoint_from_Polyhedron
#define ppl_Polyhedron_equals_Polyhedron xsb_stub_ppl_Polyhedron_equals_Polyhedron
#define ppl_Polyhedron_OK xsb_stub_ppl_Polyhedron_OK
#define ppl_Polyhedron_add_constraint xsb_stub_ppl_Polyhedron_add_constraint
#define ppl_Polyhedron_add_constraint_and_minimize xsb_stub_ppl_Polyhedron_add_constraint_and_minimize
#define ppl_Polyhedron_add_generator xsb_stub_ppl_Polyhedron_add_generator
#define ppl_Polyhedron_add_generator_and_minimize xsb_stub_ppl_Polyhedron_add_generator_and_minimize
#define ppl_Polyhedron_add_constraints xsb_stub_ppl_Polyhedron_add_constraints
#define ppl_Polyhedron_add_constraints_and_minimize xsb_stub_ppl_Polyhedron_add_constraints_and_minimize
#define ppl_Polyhedron_add_generators xsb_stub_ppl_Polyhedron_add_generators
#define ppl_Polyhedron_add_generators_and_minimize xsb_stub_ppl_Polyhedron_add_generators_and_minimize
#define ppl_Polyhedron_intersection_assign xsb_stub_ppl_Polyhedron_intersection_assign
#define ppl_Polyhedron_intersection_assign_and_minimize xsb_stub_ppl_Polyhedron_intersection_assign_and_minimize
#define ppl_Polyhedron_poly_hull_assign xsb_stub_ppl_Polyhedron_poly_hull_assign
#define ppl_Polyhedron_poly_hull_assign_and_minimize xsb_stub_ppl_Polyhedron_poly_hull_assign_and_minimize
#define ppl_Polyhedron_poly_difference_assign xsb_stub_ppl_Polyhedron_poly_difference_assign
#define ppl_Polyhedron_affine_image xsb_stub_ppl_Polyhedron_affine_image
#define ppl_Polyhedron_affine_preimage xsb_stub_ppl_Polyhedron_affine_preimage
#define ppl_Polyhedron_bounded_affine_image xsb_stub_ppl_Polyhedron_bounded_affine_image
#define ppl_Polyhedron_generalized_affine_image xsb_stub_ppl_Polyhedron_generalized_affine_image
#define ppl_Polyhedron_generalized_affine_image_lhs_rhs xsb_stub_ppl_Polyhedron_generalized_affine_image_lhs_rhs
#define ppl_Polyhedron_time_elapse_assign xsb_stub_ppl_Polyhedron_time_elapse_assign
#define ppl_Polyhedron_topological_closure_assign xsb_stub_ppl_Polyhedron_topological_closure_assign
#define ppl_Polyhedron_BHRZ03_widening_assign_with_token xsb_stub_ppl_Polyhedron_BHRZ03_widening_assign_with_token
#define ppl_Polyhedron_BHRZ03_widening_assign xsb_stub_ppl_Polyhedron_BHRZ03_widening_assign
#define ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token xsb_stub_ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token
#define ppl_Polyhedron_limited_BHRZ03_extrapolation_assign xsb_stub_ppl_Polyhedron_limited_BHRZ03_extrapolation_assign
#define ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token xsb_stub_ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token
#define ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign xsb_stub_ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign
#define ppl_Polyhedron_H79_widening_assign_with_token xsb_stub_ppl_Polyhedron_H79_widening_assign_with_token
#define ppl_Polyhedron_H79_widening_assign xsb_stub_ppl_Polyhedron_H79_widening_assign
#define ppl_Polyhedron_limited_H79_extrapolation_assign_with_token xsb_stub_ppl_Polyhedron_limited_H79_extrapolation_assign_with_token
#define ppl_Polyhedron_limited_H79_extrapolation_assign xsb_stub_ppl_Polyhedron_limited_H79_extrapolation_assign
#define ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token xsb_stub_ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token
#define ppl_Polyhedron_bounded_H79_extrapolation_assign xsb_stub_ppl_Polyhedron_bounded_H79_extrapolation_assign
#define ppl_Polyhedron_add_space_dimensions_and_project xsb_stub_ppl_Polyhedron_add_space_dimensions_and_project
#define ppl_Polyhedron_add_space_dimensions_and_embed xsb_stub_ppl_Polyhedron_add_space_dimensions_and_embed
#define ppl_Polyhedron_concatenate_assign xsb_stub_ppl_Polyhedron_concatenate_assign
#define ppl_Polyhedron_remove_space_dimensions xsb_stub_ppl_Polyhedron_remove_space_dimensions
#define ppl_Polyhedron_remove_higher_space_dimensions xsb_stub_ppl_Polyhedron_remove_higher_space_dimensions
#define ppl_Polyhedron_expand_space_dimension xsb_stub_ppl_Polyhedron_expand_space_dimension
#define ppl_Polyhedron_fold_space_dimensions xsb_stub_ppl_Polyhedron_fold_space_dimensions
#define ppl_Polyhedron_map_space_dimensions xsb_stub_ppl_Polyhedron_map_space_dimensions

#include "../ppl_prolog.icc"

#undef ppl_version_major
#undef ppl_version_minor
#undef ppl_version_revision
#undef ppl_version_beta
#undef ppl_version
#undef ppl_banner
#undef ppl_max_space_dimension
#undef ppl_initialize
#undef ppl_finalize
#undef ppl_set_timeout_exception_atom
#undef ppl_timeout_exception_atom
#undef ppl_set_timeout
#undef ppl_reset_timeout
#undef ppl_new_Polyhedron_from_space_dimension
#undef ppl_new_Polyhedron_from_Polyhedron
#undef ppl_new_Polyhedron_from_constraints
#undef ppl_new_Polyhedron_from_generators
#undef ppl_new_Polyhedron_from_bounding_box
#undef ppl_Polyhedron_swap
#undef ppl_delete_Polyhedron
#undef ppl_Polyhedron_space_dimension
#undef ppl_Polyhedron_affine_dimension
#undef ppl_Polyhedron_get_constraints
#undef ppl_Polyhedron_get_minimized_constraints
#undef ppl_Polyhedron_get_generators
#undef ppl_Polyhedron_get_minimized_generators
#undef ppl_Polyhedron_relation_with_constraint
#undef ppl_Polyhedron_relation_with_generator
#undef ppl_Polyhedron_get_bounding_box
#undef ppl_Polyhedron_is_empty
#undef ppl_Polyhedron_is_universe
#undef ppl_Polyhedron_is_bounded
#undef ppl_Polyhedron_bounds_from_above
#undef ppl_Polyhedron_bounds_from_below
#undef ppl_Polyhedron_maximize
#undef ppl_Polyhedron_maximize_with_point
#undef ppl_Polyhedron_minimize
#undef ppl_Polyhedron_minimize_with_point
#undef ppl_Polyhedron_is_topologically_closed
#undef ppl_Polyhedron_contains_Polyhedron
#undef ppl_Polyhedron_strictly_contains_Polyhedron
#undef ppl_Polyhedron_is_disjoint_from_Polyhedron
#undef ppl_Polyhedron_equals_Polyhedron
#undef ppl_Polyhedron_OK
#undef ppl_Polyhedron_add_constraint
#undef ppl_Polyhedron_add_constraint_and_minimize
#undef ppl_Polyhedron_add_generator
#undef ppl_Polyhedron_add_generator_and_minimize
#undef ppl_Polyhedron_add_constraints
#undef ppl_Polyhedron_add_constraints_and_minimize
#undef ppl_Polyhedron_add_generators
#undef ppl_Polyhedron_add_generators_and_minimize
#undef ppl_Polyhedron_intersection_assign
#undef ppl_Polyhedron_intersection_assign_and_minimize
#undef ppl_Polyhedron_poly_hull_assign
#undef ppl_Polyhedron_poly_hull_assign_and_minimize
#undef ppl_Polyhedron_poly_difference_assign
#undef ppl_Polyhedron_affine_image
#undef ppl_Polyhedron_affine_preimage
#undef ppl_Polyhedron_bounded_affine_image
#undef ppl_Polyhedron_generalized_affine_image
#undef ppl_Polyhedron_generalized_affine_image_lhs_rhs
#undef ppl_Polyhedron_time_elapse_assign
#undef ppl_Polyhedron_topological_closure_assign
#undef ppl_Polyhedron_BHRZ03_widening_assign_with_token
#undef ppl_Polyhedron_BHRZ03_widening_assign
#undef ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token
#undef ppl_Polyhedron_limited_BHRZ03_extrapolation_assign
#undef ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token
#undef ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign
#undef ppl_Polyhedron_H79_widening_assign_with_token
#undef ppl_Polyhedron_H79_widening_assign
#undef ppl_Polyhedron_limited_H79_extrapolation_assign_with_token
#undef ppl_Polyhedron_limited_H79_extrapolation_assign
#undef ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token
#undef ppl_Polyhedron_bounded_H79_extrapolation_assign
#undef ppl_Polyhedron_add_space_dimensions_and_project
#undef ppl_Polyhedron_add_space_dimensions_and_embed
#undef ppl_Polyhedron_concatenate_assign
#undef ppl_Polyhedron_remove_space_dimensions
#undef ppl_Polyhedron_remove_higher_space_dimensions
#undef ppl_Polyhedron_expand_space_dimension
#undef ppl_Polyhedron_fold_space_dimensions
#undef ppl_Polyhedron_map_space_dimensions

#define XSB_ENTRY_0(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  return xsb_stub_##name(); \
}

#define XSB_ENTRY_1(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  Prolog_term_ref arg1 = reg_term(1); \
  return xsb_stub_##name(arg1); \
}

#define XSB_ENTRY_2(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  Prolog_term_ref arg1 = reg_term(1); \
  Prolog_term_ref arg2 = reg_term(2); \
  return xsb_stub_##name(arg1, arg2); \
}

#define XSB_ENTRY_3(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  Prolog_term_ref arg1 = reg_term(1); \
  Prolog_term_ref arg2 = reg_term(2); \
  Prolog_term_ref arg3 = reg_term(3); \
  return xsb_stub_##name(arg1, arg2, arg3); \
}

#define XSB_ENTRY_4(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  Prolog_term_ref arg1 = reg_term(1); \
  Prolog_term_ref arg2 = reg_term(2); \
  Prolog_term_ref arg3 = reg_term(3); \
  Prolog_term_ref arg4 = reg_term(4); \
  return xsb_stub_##name(arg1, arg2, arg3, arg4); \
}

#define XSB_ENTRY_5(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  Prolog_term_ref arg1 = reg_term(1); \
  Prolog_term_ref arg2 = reg_term(2); \
  Prolog_term_ref arg3 = reg_term(3); \
  Prolog_term_ref arg4 = reg_term(4); \
  Prolog_term_ref arg5 = reg_term(5); \
  return xsb_stub_##name(arg1, arg2, arg3, arg4, arg5); \
}

#define XSB_ENTRY_6(name) \
extern "C" Prolog_foreign_return_type \
name() { \
  Prolog_term_ref arg1 = reg_term(1); \
  Prolog_term_ref arg2 = reg_term(2); \
  Prolog_term_ref arg3 = reg_term(3); \
  Prolog_term_ref arg4 = reg_term(4); \
  Prolog_term_ref arg5 = reg_term(5); \
  Prolog_term_ref arg6 = reg_term(6); \
  return xsb_stub_##name(arg1, arg2, arg3, arg4, arg5, arg6); \
}

XSB_ENTRY_1(ppl_version_major)
XSB_ENTRY_1(ppl_version_minor)
XSB_ENTRY_1(ppl_version_revision)
XSB_ENTRY_1(ppl_version_beta)
XSB_ENTRY_1(ppl_version)
XSB_ENTRY_1(ppl_banner)
XSB_ENTRY_1(ppl_max_space_dimension)
XSB_ENTRY_0(ppl_initialize)
XSB_ENTRY_0(ppl_finalize)
XSB_ENTRY_1(ppl_set_timeout_exception_atom)
XSB_ENTRY_1(ppl_timeout_exception_atom)
XSB_ENTRY_1(ppl_set_timeout)
XSB_ENTRY_0(ppl_reset_timeout)
XSB_ENTRY_4(ppl_new_Polyhedron_from_space_dimension)
XSB_ENTRY_4(ppl_new_Polyhedron_from_Polyhedron)
XSB_ENTRY_3(ppl_new_Polyhedron_from_constraints)
XSB_ENTRY_3(ppl_new_Polyhedron_from_generators)
XSB_ENTRY_3(ppl_new_Polyhedron_from_bounding_box)
XSB_ENTRY_2(ppl_Polyhedron_swap)
XSB_ENTRY_1(ppl_delete_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_space_dimension)
XSB_ENTRY_2(ppl_Polyhedron_affine_dimension)
XSB_ENTRY_2(ppl_Polyhedron_get_constraints)
XSB_ENTRY_2(ppl_Polyhedron_get_minimized_constraints)
XSB_ENTRY_2(ppl_Polyhedron_get_generators)
XSB_ENTRY_2(ppl_Polyhedron_get_minimized_generators)
XSB_ENTRY_3(ppl_Polyhedron_relation_with_constraint)
XSB_ENTRY_3(ppl_Polyhedron_relation_with_generator)
XSB_ENTRY_3(ppl_Polyhedron_get_bounding_box)
XSB_ENTRY_1(ppl_Polyhedron_is_empty)
XSB_ENTRY_1(ppl_Polyhedron_is_universe)
XSB_ENTRY_1(ppl_Polyhedron_is_bounded)
XSB_ENTRY_2(ppl_Polyhedron_bounds_from_above)
XSB_ENTRY_2(ppl_Polyhedron_bounds_from_below)
XSB_ENTRY_5(ppl_Polyhedron_maximize)
XSB_ENTRY_6(ppl_Polyhedron_maximize_with_point)
XSB_ENTRY_5(ppl_Polyhedron_minimize)
XSB_ENTRY_6(ppl_Polyhedron_minimize_with_point)
XSB_ENTRY_1(ppl_Polyhedron_is_topologically_closed)
XSB_ENTRY_2(ppl_Polyhedron_contains_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_strictly_contains_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_is_disjoint_from_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_equals_Polyhedron)
XSB_ENTRY_1(ppl_Polyhedron_OK)
XSB_ENTRY_2(ppl_Polyhedron_add_constraint)
XSB_ENTRY_2(ppl_Polyhedron_add_constraint_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_add_generator)
XSB_ENTRY_2(ppl_Polyhedron_add_generator_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_add_constraints)
XSB_ENTRY_2(ppl_Polyhedron_add_constraints_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_add_generators)
XSB_ENTRY_2(ppl_Polyhedron_add_generators_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_intersection_assign)
XSB_ENTRY_2(ppl_Polyhedron_intersection_assign_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_poly_hull_assign)
XSB_ENTRY_2(ppl_Polyhedron_poly_hull_assign_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_poly_difference_assign)
XSB_ENTRY_4(ppl_Polyhedron_affine_image)
XSB_ENTRY_4(ppl_Polyhedron_affine_preimage)
XSB_ENTRY_5(ppl_Polyhedron_bounded_affine_image)
XSB_ENTRY_5(ppl_Polyhedron_generalized_affine_image)
XSB_ENTRY_4(ppl_Polyhedron_generalized_affine_image_lhs_rhs)
XSB_ENTRY_2(ppl_Polyhedron_time_elapse_assign)
XSB_ENTRY_1(ppl_Polyhedron_topological_closure_assign)
XSB_ENTRY_3(ppl_Polyhedron_BHRZ03_widening_assign_with_token)
XSB_ENTRY_2(ppl_Polyhedron_BHRZ03_widening_assign)
XSB_ENTRY_4(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token)
XSB_ENTRY_3(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign)
XSB_ENTRY_4(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token)
XSB_ENTRY_3(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign)
XSB_ENTRY_3(ppl_Polyhedron_H79_widening_assign_with_token)
XSB_ENTRY_2(ppl_Polyhedron_H79_widening_assign)
XSB_ENTRY_4(ppl_Polyhedron_limited_H79_extrapolation_assign_with_token)
XSB_ENTRY_3(ppl_Polyhedron_limited_H79_extrapolation_assign)
XSB_ENTRY_4(ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token)
XSB_ENTRY_3(ppl_Polyhedron_bounded_H79_extrapolation_assign)
XSB_ENTRY_2(ppl_Polyhedron_add_space_dimensions_and_project)
XSB_ENTRY_2(ppl_Polyhedron_add_space_dimensions_and_embed)
XSB_ENTRY_2(ppl_Polyhedron_concatenate_assign)
XSB_ENTRY_2(ppl_Polyhedron_remove_space_dimensions)
XSB_ENTRY_2(ppl_Polyhedron_remove_higher_space_dimensions)
XSB_ENTRY_3(ppl_Polyhedron_expand_space_dimension)
XSB_ENTRY_3(ppl_Polyhedron_fold_space_dimensions)
XSB_ENTRY_2(ppl_Polyhedron_map_space_dimensions)

extern "C" void
init() {
  ppl_initialize();
}
