/* XSB Prolog interface: system-dependent part.  -*- C++ -*-
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

#include <config.h>

#include "Integer.defs.hh"

// XSB 2.4 and 2.5 miss the `extern "C"' wrapper.
extern "C" {

#include <cinterf.h>
#undef min
#undef max

} // extern "C"

#include <cassert>

typedef prolog_term Prolog_term_ref;
typedef char* Prolog_atom;
typedef xsbBool Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = TRUE;
static const Prolog_foreign_return_type PROLOG_FAILURE = FALSE;

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

/*!
  Return a new term reference.
*/
static inline Prolog_term_ref
Prolog_new_term_ref() {
  return p2p_new();
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
  return c2p_int(i, t) != FALSE;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
static inline bool
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  // FIXME: the following cast is really a bug in XSB.
  return c2p_string(string_find(const_cast<char*>(s), 1), t) != FALSE;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline bool
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  return c2p_string(a, t) != FALSE;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline bool
Prolog_put_address(Prolog_term_ref& t, void* p) {
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
static inline bool
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1) {
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 1, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  t = new_compound;
  return true;
}

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 2 with arguments \p a1 and \p a2.
*/
static inline bool
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1, Prolog_term_ref a2) {
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 2, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  p2p_unify(p2p_arg(new_compound, 2), a2);
  t = new_compound;
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
  prolog_term new_compound;
  c2p_functor(f, 3, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  p2p_unify(p2p_arg(new_compound, 2), a2);
  p2p_unify(p2p_arg(new_compound, 3), a3);
  t = new_compound;
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
  prolog_term new_compound = p2p_new();
  c2p_functor(f, 4, new_compound);
  p2p_unify(p2p_arg(new_compound, 1), a1);
  p2p_unify(p2p_arg(new_compound, 2), a2);
  p2p_unify(p2p_arg(new_compound, 3), a3);
  p2p_unify(p2p_arg(new_compound, 4), a4);
  t = new_compound;
  return true;
}

/*!
  Assign to \p c a Prolog list whose head is \p h and tail is \p t. 
*/
static inline bool
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  prolog_term new_cons = p2p_new();
  c2p_list(new_cons);
  p2p_unify(p2p_car(new_cons), h);
  p2p_unify(p2p_cdr(new_cons), t);
  c = new_cons;
  return true;
}

static Prolog_atom a_throw;

static void
ppl_Prolog_sysdep_init() {
  a_throw = string_find("throw", 1);
}

static void
ppl_Prolog_sysdep_deinit() {
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
}

/*!
  Return true if \p t is a Prolog variable, false otherwise. 
*/
static inline bool
Prolog_is_variable(Prolog_term_ref t) {
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  return is_var(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog atom, false otherwise. 
*/
static inline bool
Prolog_is_atom(Prolog_term_ref t) {
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  return is_string(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise. 
*/
static inline bool
Prolog_is_integer(Prolog_term_ref t) {
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  return is_int(t) != FALSE;
}

/*!
  Return true if \p t is the representation of an address, false otherwise. 
*/
static inline bool
Prolog_is_address(Prolog_term_ref t) {
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  return is_int(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise. 
*/
static inline bool
Prolog_is_compound(Prolog_term_ref t) {
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  return is_functor(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise. 
*/
static inline bool
Prolog_is_cons(Prolog_term_ref t) {
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  return is_list(t) != FALSE;
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
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  v = p2c_int(t);
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
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  p = reinterpret_cast<void*>(p2c_int(t));
  return true;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
static inline bool
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom& name) {
  assert(Prolog_is_atom(t));
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  name = p2c_string(t);
  return true;
}

/*!
  If \p t is a Prolog compound term, return true and store its name
  and arity into \p name and \p arity, respectively.
  The behavior is undefined if \p t is not a Prolog compound term.
*/
static inline bool
Prolog_get_compound_name_arity(Prolog_term_ref t,
			       Prolog_atom& name, int& arity) {
  assert(Prolog_is_compound(t));
  // The following statement is to get around a bug in XSB 2.5.
  t = p2p_deref(t);
  name = p2c_functor(t);
  arity = p2c_arity(t);
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
  a = p2p_arg(t, i);
  return true;
}

/*!
  If \p c is a Prolog cons (list constructor), assign its head and
  tail to \p h and \p t, respectively.
  The behavior is undefined if \p c is not a Prolog cons.
*/
static inline bool
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref& h, Prolog_term_ref& t) {
  assert(Prolog_is_cons(c));
  h = p2p_car(c);
  t = p2p_cdr(c);
  return true;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline bool
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return p2p_unify(t, u) != FALSE;
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does XSB support unlimited precision integers?
  long v;
  Prolog_get_long(t, v);
  return PPL::Integer(v);
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  // FIXME: does XSB support unlimited precision integer?
  if (!n.fits_slong_p())
    throw_unknown_interface_error("Integer_to_integer_term()");
  Prolog_term_ref t = p2p_new();
  c2p_int(n.get_si(), t);
  return t;
}

#define ppl_initialize xsb_stub_ppl_initialize
#define ppl_finalize xsb_stub_ppl_finalize
#define ppl_new_Polyhedron_from_dimension xsb_stub_ppl_new_Polyhedron_from_dimension
#define ppl_new_Polyhedron_empty_from_dimension xsb_stub_ppl_new_Polyhedron_empty_from_dimension
#define ppl_new_Polyhedron_from_Polyhedron xsb_stub_ppl_new_Polyhedron_from_Polyhedron
#define ppl_new_Polyhedron_from_constraints xsb_stub_ppl_new_Polyhedron_from_constraints
#define ppl_new_Polyhedron_from_generators xsb_stub_ppl_new_Polyhedron_from_generators
#define ppl_new_Polyhedron_from_bounding_box xsb_stub_ppl_new_Polyhedron_from_bounding_box
#define ppl_delete_Polyhedron xsb_stub_ppl_delete_Polyhedron
#define ppl_Polyhedron_space_dimension xsb_stub_ppl_Polyhedron_space_dimension
#define ppl_Polyhedron_intersection_assign xsb_stub_ppl_Polyhedron_intersection_assign
#define ppl_Polyhedron_intersection_assign_and_minimize xsb_stub_ppl_Polyhedron_intersection_assign_and_minimize
#define ppl_Polyhedron_poly_hull_assign xsb_stub_ppl_Polyhedron_poly_hull_assign
#define ppl_Polyhedron_poly_hull_assign_and_minimize xsb_stub_ppl_Polyhedron_poly_hull_assign_and_minimize
#define ppl_Polyhedron_poly_difference_assign xsb_stub_ppl_Polyhedron_poly_difference_assign
#define ppl_Polyhedron_poly_difference_assign_and_minimize xsb_stub_ppl_Polyhedron_poly_difference_assign_and_minimize
#define ppl_Polyhedron_H79_widening_assign xsb_stub_ppl_Polyhedron_H79_widening_assign
#define ppl_Polyhedron_limited_H79_widening_assign xsb_stub_ppl_Polyhedron_limited_H79_widening_assign
#define ppl_Polyhedron_topological_closure_assign xsb_stub_ppl_Polyhedron_topological_closure_assign
#define ppl_Polyhedron_get_constraints xsb_stub_ppl_Polyhedron_get_constraints
#define ppl_Polyhedron_get_minimized_constraints xsb_stub_ppl_Polyhedron_get_minimized_constraints
#define ppl_Polyhedron_get_generators xsb_stub_ppl_Polyhedron_get_generators
#define ppl_Polyhedron_get_minimized_generators xsb_stub_ppl_Polyhedron_get_minimized_generators
#define ppl_Polyhedron_add_constraint xsb_stub_ppl_Polyhedron_add_constraint
#define ppl_Polyhedron_add_generator xsb_stub_ppl_Polyhedron_add_generator
#define ppl_Polyhedron_add_constraints xsb_stub_ppl_Polyhedron_add_constraints
#define ppl_Polyhedron_add_constraints_and_minimize xsb_stub_ppl_Polyhedron_add_constraints_and_minimize
#define ppl_Polyhedron_add_generators xsb_stub_ppl_Polyhedron_add_generators
#define ppl_Polyhedron_add_generators_and_minimize xsb_stub_ppl_Polyhedron_add_generators_and_minimize
#define ppl_Polyhedron_add_dimensions_and_constraints xsb_stub_ppl_Polyhedron_add_dimensions_and_constraints
#define ppl_Polyhedron_add_dimensions_and_project xsb_stub_ppl_Polyhedron_add_dimensions_and_project
#define ppl_Polyhedron_add_dimensions_and_embed xsb_stub_ppl_Polyhedron_add_dimensions_and_embed
#define ppl_Polyhedron_remove_dimensions xsb_stub_ppl_Polyhedron_remove_dimensions
#define ppl_Polyhedron_remove_higher_dimensions xsb_stub_ppl_Polyhedron_remove_higher_dimensions
#define ppl_Polyhedron_affine_image xsb_stub_ppl_Polyhedron_affine_image
#define ppl_Polyhedron_affine_preimage xsb_stub_ppl_Polyhedron_affine_preimage
#define ppl_Polyhedron_relation_with_constraint xsb_stub_ppl_Polyhedron_relation_with_constraint
#define ppl_Polyhedron_relation_with_generator xsb_stub_ppl_Polyhedron_relation_with_generator
#define ppl_Polyhedron_check_empty xsb_stub_ppl_Polyhedron_check_empty
#define ppl_Polyhedron_check_universe xsb_stub_ppl_Polyhedron_check_universe
#define ppl_Polyhedron_is_bounded xsb_stub_ppl_Polyhedron_is_bounded
#define ppl_Polyhedron_bounds_from_above xsb_stub_ppl_Polyhedron_bounds_from_above
#define ppl_Polyhedron_bounds_from_below xsb_stub_ppl_Polyhedron_bounds_from_below
#define ppl_Polyhedron_is_topologically_closed xsb_stub_ppl_Polyhedron_is_topologically_closed
#define ppl_Polyhedron_contains_Polyhedron xsb_stub_ppl_Polyhedron_contains_Polyhedron
#define ppl_Polyhedron_strictly_contains_Polyhedron xsb_stub_ppl_Polyhedron_strictly_contains_Polyhedron
#define ppl_Polyhedron_equals_Polyhedron xsb_stub_ppl_Polyhedron_equals_Polyhedron
#define ppl_Polyhedron_get_bounding_box xsb_stub_ppl_Polyhedron_get_bounding_box

#include "../ppl_prolog.icc"

#undef ppl_initialize
#undef ppl_finalize
#undef ppl_new_Polyhedron_from_dimension
#undef ppl_new_Polyhedron_empty_from_dimension
#undef ppl_new_Polyhedron_from_Polyhedron
#undef ppl_new_Polyhedron_from_constraints
#undef ppl_new_Polyhedron_from_generators
#undef ppl_new_Polyhedron_from_bounding_box
#undef ppl_delete_Polyhedron
#undef ppl_Polyhedron_space_dimension
#undef ppl_Polyhedron_intersection_assign
#undef ppl_Polyhedron_intersection_assign_and_minimize
#undef ppl_Polyhedron_poly_hull_assign
#undef ppl_Polyhedron_poly_hull_assign_and_minimize
#undef ppl_Polyhedron_poly_difference_assign
#undef ppl_Polyhedron_poly_difference_assign_and_minimize
#undef ppl_Polyhedron_H79_widening_assign
#undef ppl_Polyhedron_limited_H79_widening_assign
#undef ppl_Polyhedron_topological_closure_assign
#undef ppl_Polyhedron_get_constraints
#undef ppl_Polyhedron_get_minimized_constraints
#undef ppl_Polyhedron_get_generators
#undef ppl_Polyhedron_get_minimized_generators
#undef ppl_Polyhedron_add_constraint
#undef ppl_Polyhedron_add_generator
#undef ppl_Polyhedron_add_constraints
#undef ppl_Polyhedron_add_constraints_and_minimize
#undef ppl_Polyhedron_add_generators
#undef ppl_Polyhedron_add_generators_and_minimize
#undef ppl_Polyhedron_add_dimensions_and_constraints
#undef ppl_Polyhedron_add_dimensions_and_project
#undef ppl_Polyhedron_add_dimensions_and_embed
#undef ppl_Polyhedron_remove_dimensions
#undef ppl_Polyhedron_remove_higher_dimensions
#undef ppl_Polyhedron_affine_image
#undef ppl_Polyhedron_affine_preimage
#undef ppl_Polyhedron_relation_with_constraint
#undef ppl_Polyhedron_relation_with_generator
#undef ppl_Polyhedron_check_empty
#undef ppl_Polyhedron_check_universe
#undef ppl_Polyhedron_is_bounded
#undef ppl_Polyhedron_bounds_from_above
#undef ppl_Polyhedron_bounds_from_below
#undef ppl_Polyhedron_is_topologically_closed
#undef ppl_Polyhedron_contains_Polyhedron
#undef ppl_Polyhedron_strictly_contains_Polyhedron
#undef ppl_Polyhedron_equals_Polyhedron
#undef ppl_Polyhedron_get_bounding_box

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

XSB_ENTRY_0(ppl_initialize)
XSB_ENTRY_0(ppl_finalize)
XSB_ENTRY_3(ppl_new_Polyhedron_from_dimension)
XSB_ENTRY_3(ppl_new_Polyhedron_empty_from_dimension)
XSB_ENTRY_4(ppl_new_Polyhedron_from_Polyhedron)
XSB_ENTRY_3(ppl_new_Polyhedron_from_constraints)
XSB_ENTRY_3(ppl_new_Polyhedron_from_generators)
XSB_ENTRY_3(ppl_new_Polyhedron_from_bounding_box)
XSB_ENTRY_1(ppl_delete_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_space_dimension)
XSB_ENTRY_2(ppl_Polyhedron_intersection_assign)
XSB_ENTRY_2(ppl_Polyhedron_intersection_assign_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_poly_hull_assign)
XSB_ENTRY_2(ppl_Polyhedron_poly_hull_assign_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_poly_difference_assign)
XSB_ENTRY_2(ppl_Polyhedron_poly_difference_assign_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_H79_widening_assign)
XSB_ENTRY_3(ppl_Polyhedron_limited_H79_widening_assign)
XSB_ENTRY_1(ppl_Polyhedron_topological_closure_assign)
XSB_ENTRY_2(ppl_Polyhedron_get_constraints)
XSB_ENTRY_2(ppl_Polyhedron_get_minimized_constraints)
XSB_ENTRY_2(ppl_Polyhedron_get_generators)
XSB_ENTRY_2(ppl_Polyhedron_get_minimized_generators)
XSB_ENTRY_2(ppl_Polyhedron_add_constraint)
XSB_ENTRY_2(ppl_Polyhedron_add_generator)
XSB_ENTRY_2(ppl_Polyhedron_add_constraints)
XSB_ENTRY_2(ppl_Polyhedron_add_constraints_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_add_generators)
XSB_ENTRY_2(ppl_Polyhedron_add_generators_and_minimize)
XSB_ENTRY_2(ppl_Polyhedron_add_dimensions_and_constraints)
XSB_ENTRY_2(ppl_Polyhedron_add_dimensions_and_project)
XSB_ENTRY_2(ppl_Polyhedron_add_dimensions_and_embed)
XSB_ENTRY_2(ppl_Polyhedron_remove_dimensions)
XSB_ENTRY_2(ppl_Polyhedron_remove_higher_dimensions)
XSB_ENTRY_4(ppl_Polyhedron_affine_image)
XSB_ENTRY_4(ppl_Polyhedron_affine_preimage)
XSB_ENTRY_3(ppl_Polyhedron_relation_with_constraint)
XSB_ENTRY_3(ppl_Polyhedron_relation_with_generator)
XSB_ENTRY_1(ppl_Polyhedron_check_empty)
XSB_ENTRY_1(ppl_Polyhedron_check_universe)
XSB_ENTRY_1(ppl_Polyhedron_is_bounded)
XSB_ENTRY_2(ppl_Polyhedron_bounds_from_above)
XSB_ENTRY_2(ppl_Polyhedron_bounds_from_below)
XSB_ENTRY_1(ppl_Polyhedron_is_topologically_closed)
XSB_ENTRY_2(ppl_Polyhedron_contains_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_strictly_contains_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_equals_Polyhedron)
XSB_ENTRY_2(ppl_Polyhedron_get_bounding_box)

extern "C" void
init() {
  ppl_initialize();
}
