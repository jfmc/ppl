/* SWI Prolog interface.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>

#include "Coefficient.defs.hh"
#include "checked.defs.hh"
#include "checked_int.inlines.hh"
#include "checked_mpz.inlines.hh"
#include <SWI-Prolog.h>
#include <cassert>

typedef term_t Prolog_term_ref;
typedef atom_t Prolog_atom;
typedef foreign_t Prolog_foreign_return_type;

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
  Prolog_min_integer = PL_query(PL_QUERY_MIN_INTEGER);
  Prolog_max_integer = PL_query(PL_QUERY_MAX_INTEGER);
}

/*!
  Perform system-dependent de-itialization.
*/
void
ppl_Prolog_sysdep_deinit() {
}

/*!
  Return a new term reference.
*/
inline Prolog_term_ref
Prolog_new_term_ref() {
  return PL_new_term_ref();
}

/*!
  Make \p t be a reference to the same term referenced by \p u,
  i.e., assign \p u to \p t.
*/
inline int
Prolog_put_term(Prolog_term_ref t, Prolog_term_ref u) {
  PL_put_term(t, u);
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p l.
*/
inline int
Prolog_put_long(Prolog_term_ref t, long l) {
  if (l < Prolog_min_integer || l > Prolog_max_integer)
    throw PPL_integer_out_of_range(l);
  PL_put_integer(t, l);
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p ul.
*/
inline int
Prolog_put_ulong(Prolog_term_ref t, unsigned long ul) {
  if (ul > static_cast<unsigned long>(Prolog_max_integer))
    throw PPL_integer_out_of_range(ul);
  PL_put_integer(t, ul);
  return 1;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
inline int
Prolog_put_atom_chars(Prolog_term_ref t, const char* s) {
  PL_put_atom_chars(t, s);
  return 1;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
inline int
Prolog_put_atom(Prolog_term_ref t, Prolog_atom a) {
  PL_put_atom(t, a);
  return 1;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
inline int
Prolog_put_address(Prolog_term_ref t, void* p) {
  PL_put_pointer(t, p);
  return 1;
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
inline int
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1) {
  PL_cons_functor(t, PL_new_functor(f, 1), a1);
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
inline int
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  PL_cons_functor(t, PL_new_functor(f, 2), a1, a2);
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 3 with arguments \p a1, \p a2 and \p a3.
*/
inline int
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3) {
  PL_cons_functor(t, PL_new_functor(f, 3), a1, a2, a3);
  return 1;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
*/
inline int
Prolog_construct_compound(Prolog_term_ref t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2,
			  Prolog_term_ref a3, Prolog_term_ref a4) {
  PL_cons_functor(t, PL_new_functor(f, 4), a1, a2, a3, a4);
  return 1;
}

/*!
  Assign to \p c a Prolog list whose head is \p h and tail is \p t.
*/
inline int
Prolog_construct_cons(Prolog_term_ref c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  PL_cons_list(c, h, t);
  return 1;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
inline void
Prolog_raise_exception(Prolog_term_ref t) {
  (void) PL_raise_exception(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise.
*/
inline int
Prolog_is_variable(Prolog_term_ref t) {
  return PL_is_variable(t);
}

/*!
  Return true if \p t is a Prolog atom, false otherwise.
*/
inline int
Prolog_is_atom(Prolog_term_ref t) {
  return PL_is_atom(t);
}

/*!
  Return true if \p t is a Prolog integer, false otherwise.
*/
inline int
Prolog_is_integer(Prolog_term_ref t) {
  return PL_is_integer(t);
}

/*!
  Return true if \p t is the representation of an address, false otherwise.
*/
inline int
Prolog_is_address(Prolog_term_ref t) {
  return PL_is_integer(t);
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise.
*/
inline int
Prolog_is_compound(Prolog_term_ref t) {
  return PL_is_compound(t);
}

/*!
  Return true if \p t is a Prolog list, false otherwise.
*/
inline int
Prolog_is_cons(Prolog_term_ref t) {
  return !PL_is_atom(t) && PL_is_list(t);
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
  return PL_get_long(t, lp);
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
inline int
Prolog_get_address(Prolog_term_ref t, void** vpp) {
  assert(Prolog_is_address(t));
  return PL_get_pointer(t, vpp);
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
inline int
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom* ap) {
  assert(Prolog_is_atom(t));
  return PL_get_atom(t, ap);
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
inline int
Prolog_get_compound_name_arity(Prolog_term_ref t, Prolog_atom* ap, int* ip) {
  assert(Prolog_is_compound(t));
  return PL_get_name_arity(t, ap, ip);
}

/*!
  If \p t is a Prolog compound term and \p i is a positive integer
  less than or equal to its arity, return true and assign to \p a the
  i-th (principal) argument of \p t.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
inline int
Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref a) {
  assert(Prolog_is_compound(t));
  return PL_get_arg(i, t, a);
}

#include <iostream>
using namespace std;
/*!
  If \p c is a Prolog cons (list constructor), assign its head and
  tail to \p h and \p t, respectively.
  The behavior is undefined if \p c is not a Prolog cons.
*/
inline int
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_cons(c));
  return PL_get_list(c, h, t);
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
inline int
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return PL_unify(t, u);
}

PPL::Coefficient
integer_term_to_Coefficient(Prolog_term_ref t) {
  assert(Prolog_is_integer(t));
  long v;
  Prolog_get_long(t, &v);
  return PPL::Coefficient(v);
}

Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  long v;
  if (PPL::assign(v, PPL::raw_value(n), PPL::ROUND_IGNORE) != PPL::V_EQ)
    throw PPL_integer_out_of_range(n);
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_put_long(t, v);
  return t;
}

} // namespace

#include "../ppl_prolog.icc"

#define PL_EXTENSION_ENTRY(name, arity) { #name, arity, (void*) name, 0 },

namespace {

PL_extension predicates[] = {
  PL_EXTENSION_ENTRY(ppl_version_major, 1)
  PL_EXTENSION_ENTRY(ppl_version_minor, 1)
  PL_EXTENSION_ENTRY(ppl_version_revision, 1)
  PL_EXTENSION_ENTRY(ppl_version_beta, 1)
  PL_EXTENSION_ENTRY(ppl_version, 1)
  PL_EXTENSION_ENTRY(ppl_banner, 1)
  PL_EXTENSION_ENTRY(ppl_max_space_dimension, 1)
  PL_EXTENSION_ENTRY(ppl_initialize, 0)
  PL_EXTENSION_ENTRY(ppl_finalize, 0)
  PL_EXTENSION_ENTRY(ppl_set_timeout_exception_atom, 1)
  PL_EXTENSION_ENTRY(ppl_timeout_exception_atom, 1)
  PL_EXTENSION_ENTRY(ppl_set_timeout, 1)
  PL_EXTENSION_ENTRY(ppl_reset_timeout, 0)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_space_dimension, 4)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_Polyhedron, 4)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_constraints, 3)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_generators, 3)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_bounding_box, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_swap, 2)
  PL_EXTENSION_ENTRY(ppl_delete_Polyhedron, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_space_dimension, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_affine_dimension, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_minimized_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_generators, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_minimized_generators, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_relation_with_constraint, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_relation_with_generator, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_bounding_box, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_is_empty, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_is_universe, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_is_bounded, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_bounds_from_above, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_bounds_from_below, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_maximize, 5)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_maximize_with_point, 6)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_minimize, 5)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_minimize_with_point, 6)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_is_topologically_closed, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_topological_closure_assign, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_contains_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_strictly_contains_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_is_disjoint_from_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_equals_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_OK, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraint, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraint_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generator, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generator_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraints_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generators, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generators_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_intersection_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_intersection_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_hull_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_hull_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_difference_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_affine_image, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_affine_preimage, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_bounded_affine_image, 5)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_bounded_affine_preimage, 5)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_generalized_affine_image, 5)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_generalized_affine_preimage, 5)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_generalized_affine_image_lhs_rhs, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_generalized_affine_preimage_lhs_rhs, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_time_elapse_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_BHRZ03_widening_assign_with_token, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_BHRZ03_widening_assign, 2)
  PL_EXTENSION_ENTRY(
           ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign, 3)
  PL_EXTENSION_ENTRY(
           ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_H79_widening_assign_with_token, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_H79_widening_assign, 2)
  PL_EXTENSION_ENTRY(
           ppl_Polyhedron_limited_H79_extrapolation_assign_with_token, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_limited_H79_extrapolation_assign, 3)
  PL_EXTENSION_ENTRY(
           ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_bounded_H79_extrapolation_assign, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_space_dimensions_and_project, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_space_dimensions_and_embed, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_concatenate_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_remove_space_dimensions, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_remove_higher_space_dimensions, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_expand_space_dimension, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_fold_space_dimensions, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_map_space_dimensions, 2)
  { NULL, 0, NULL, 0 }
};

} // namespace

extern "C" install_t
install() {
  ppl_initialize();
  PL_register_extensions(predicates);
}

extern "C" install_t
uninstall() {
  ppl_finalize();
}
