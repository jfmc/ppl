/* SICStus Prolog interface: system-dependent part.
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
#include <sicstus/sicstus.h>
#include <cassert>

typedef SP_term_ref Prolog_term_ref;
typedef SP_atom Prolog_atom;
typedef int Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = SP_SUCCESS;
static const Prolog_foreign_return_type PROLOG_FAILURE = SP_FAILURE;

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

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
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
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
  Return an atom whose name is given by the null-terminated string \p s.
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
  Assign to \p c a Prolog list whose head is \p h and tail is \p t. 
*/
static inline bool
Prolog_construct_cons(Prolog_term_ref c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  return SP_cons_list(c, h, t) != 0;
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
  Return true if \p t is a Prolog atom, false otherwise. 
*/
static inline bool
Prolog_is_atom(Prolog_term_ref t) {
  return SP_is_atom(t) != 0;
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
Prolog_is_cons(Prolog_term_ref t) {
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
Prolog_get_address(Prolog_term_ref t, void*& p) {
  assert(Prolog_is_address(t));
  return SP_get_address(t, &p) != 0;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
static inline bool
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom& name) {
  assert(Prolog_is_atom(t));
  return SP_get_atom(t, &name);
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
  If \p c is a Prolog cons (list constructor), assign its head and
  tail to \p h and \p t, respectively.
  The behavior is undefined if \p c is not a Prolog cons.
*/
static inline bool
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_cons(c));
  return SP_get_list(c, h, t) != 0;
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
    if (SP_put_integer(t, n.get_si()) == 0)
      throw_unknown_interface_error("Integer_to_integer_term()");
  else {
    std::string s = n.get_str();
    if (SP_put_number_chars(t, s.c_str()) == 0)
      throw_unknown_interface_error("Integer_to_integer_term()");
  }
  return t;
}

static void
ppl_Prolog_sysdep_init() {
}

static void
ppl_Prolog_sysdep_deinit() {
}

#include "../ppl_prolog.icc"

#define SP_STUB_0(name) \
extern "C" Prolog_foreign_return_type \
sp_stub_##name(Prolog_term_ref /* goal */, void*) { \
  return name(); \
}

#define SP_STUB_1(name) \
extern "C" Prolog_foreign_return_type \
sp_stub_##name(Prolog_term_ref goal, void*) { \
  Prolog_term_ref arg1 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(1, goal, arg1)) \
    return PROLOG_FAILURE; \
  return name(arg1); \
}

#define SP_STUB_2(name) \
extern "C" Prolog_foreign_return_type \
sp_stub_##name(Prolog_term_ref goal, void*) { \
  Prolog_term_ref arg1 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(1, goal, arg1)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg2 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(2, goal, arg2)) \
    return PROLOG_FAILURE; \
  return name(arg1, arg2); \
}

#define SP_STUB_3(name) \
extern "C" Prolog_foreign_return_type \
sp_stub_##name(Prolog_term_ref goal, void*) { \
  Prolog_term_ref arg1 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(1, goal, arg1)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg2 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(2, goal, arg2)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg3 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(3, goal, arg3)) \
    return PROLOG_FAILURE; \
  return name(arg1, arg2, arg3); \
}

#define SP_STUB_4(name) \
extern "C" Prolog_foreign_return_type \
sp_stub_##name(Prolog_term_ref goal, void*) { \
  Prolog_term_ref arg1 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(1, goal, arg1)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg2 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(2, goal, arg2)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg3 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(3, goal, arg3)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg4 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(4, goal, arg4)) \
    return PROLOG_FAILURE; \
  return name(arg1, arg2, arg3, arg4); \
}


SP_STUB_0(ppl_initialize)
SP_STUB_0(ppl_finalize)
SP_STUB_3(ppl_new_Polyhedron_from_dimension)
SP_STUB_3(ppl_new_Polyhedron_empty_from_dimension)
SP_STUB_4(ppl_new_Polyhedron_from_Polyhedron)
SP_STUB_3(ppl_new_Polyhedron_from_constraints)
SP_STUB_3(ppl_new_Polyhedron_from_generators)
SP_STUB_3(ppl_new_Polyhedron_from_bounding_box)
SP_STUB_1(ppl_delete_Polyhedron)
SP_STUB_2(ppl_Polyhedron_space_dimension)
SP_STUB_2(ppl_Polyhedron_intersection_assign)
SP_STUB_2(ppl_Polyhedron_intersection_assign_and_minimize)
SP_STUB_2(ppl_Polyhedron_concatenate_assign)
SP_STUB_2(ppl_Polyhedron_poly_hull_assign)
SP_STUB_2(ppl_Polyhedron_poly_hull_assign_and_minimize)
SP_STUB_2(ppl_Polyhedron_poly_difference_assign)
SP_STUB_2(ppl_Polyhedron_poly_difference_assign_and_minimize)
SP_STUB_2(ppl_Polyhedron_H79_widening_assign)
SP_STUB_3(ppl_Polyhedron_limited_H79_widening_assign)
SP_STUB_1(ppl_Polyhedron_topological_closure_assign)
SP_STUB_2(ppl_Polyhedron_get_constraints)
SP_STUB_2(ppl_Polyhedron_get_minimized_constraints)
SP_STUB_2(ppl_Polyhedron_get_generators)
SP_STUB_2(ppl_Polyhedron_get_minimized_generators)
SP_STUB_2(ppl_Polyhedron_add_constraint)
SP_STUB_2(ppl_Polyhedron_add_generator)
SP_STUB_2(ppl_Polyhedron_add_constraints)
SP_STUB_2(ppl_Polyhedron_add_constraints_and_minimize)
SP_STUB_2(ppl_Polyhedron_add_generators)
SP_STUB_2(ppl_Polyhedron_add_generators_and_minimize)
SP_STUB_2(ppl_Polyhedron_add_dimensions_and_project)
SP_STUB_2(ppl_Polyhedron_add_dimensions_and_embed)
SP_STUB_2(ppl_Polyhedron_remove_dimensions)
SP_STUB_2(ppl_Polyhedron_remove_higher_dimensions)
SP_STUB_4(ppl_Polyhedron_affine_image)
SP_STUB_4(ppl_Polyhedron_affine_preimage)
SP_STUB_3(ppl_Polyhedron_relation_with_constraint)
SP_STUB_3(ppl_Polyhedron_relation_with_generator)
SP_STUB_1(ppl_Polyhedron_check_empty)
SP_STUB_1(ppl_Polyhedron_check_universe)
SP_STUB_1(ppl_Polyhedron_is_bounded)
SP_STUB_2(ppl_Polyhedron_bounds_from_above)
SP_STUB_2(ppl_Polyhedron_bounds_from_below)
SP_STUB_1(ppl_Polyhedron_is_topologically_closed)
SP_STUB_2(ppl_Polyhedron_contains_Polyhedron)
SP_STUB_2(ppl_Polyhedron_strictly_contains_Polyhedron)
SP_STUB_2(ppl_Polyhedron_is_disjoint_from_Polyhedron)
SP_STUB_2(ppl_Polyhedron_equals_Polyhedron)
SP_STUB_2(ppl_Polyhedron_get_bounding_box)

#define SP_DEFINE_C_PREDICATE(name, arity) \
  SP_define_c_predicate(#name, arity, "user", sp_stub_##name, NULL)

extern "C" void
ppl_sicstus_init(int /* when */) {
  ppl_initialize();
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i) {
    if (SP_register_atom(*prolog_atoms[i].p_atom) == 0) {
      Prolog_term_ref et = Prolog_new_term_ref();
      Prolog_put_atom_chars(et, "Cannot initialize the PPL interface");
      Prolog_raise_exception(et);
      return;
    }
  }
  SP_DEFINE_C_PREDICATE(ppl_initialize, 0);
  SP_DEFINE_C_PREDICATE(ppl_finalize, 0);
  SP_DEFINE_C_PREDICATE(ppl_new_Polyhedron_from_dimension, 3);
  SP_DEFINE_C_PREDICATE(ppl_new_Polyhedron_empty_from_dimension, 3);
  SP_DEFINE_C_PREDICATE(ppl_new_Polyhedron_from_Polyhedron, 4);
  SP_DEFINE_C_PREDICATE(ppl_new_Polyhedron_from_constraints, 3);
  SP_DEFINE_C_PREDICATE(ppl_new_Polyhedron_from_generators, 3);
  SP_DEFINE_C_PREDICATE(ppl_new_Polyhedron_from_bounding_box, 3);
  SP_DEFINE_C_PREDICATE(ppl_delete_Polyhedron, 1);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_space_dimension, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_intersection_assign, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_intersection_assign_and_minimize, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_concatenate_assign, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_poly_hull_assign, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_poly_hull_assign_and_minimize, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_poly_difference_assign, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_poly_difference_assign_and_minimize, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_H79_widening_assign, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_limited_H79_widening_assign, 3);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_topological_closure_assign, 1);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_get_constraints, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_get_minimized_constraints, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_get_generators, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_get_minimized_generators, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_constraint, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_generator, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_constraints, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_constraints_and_minimize, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_generators, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_generators_and_minimize, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_project, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_add_dimensions_and_embed, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_remove_dimensions, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_remove_higher_dimensions, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_affine_image, 4);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_affine_preimage, 4);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_relation_with_constraint, 3);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_relation_with_generator, 3);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_check_empty, 1);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_check_universe, 1);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_is_bounded, 1);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_bounds_from_above, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_bounds_from_below, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_is_topologically_closed, 1);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_contains_Polyhedron, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_strictly_contains_Polyhedron, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_is_disjoint_from_Polyhedron, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_equals_Polyhedron, 2);
  SP_DEFINE_C_PREDICATE(ppl_Polyhedron_get_bounding_box, 2);
}

extern "C" void
ppl_sicstus_deinit(int /* when */) {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i)
    // SP_unregister_atom can fail.
    // We ignore such failures: what else can we do?
    (void) SP_unregister_atom(*prolog_atoms[i].p_atom);
  ppl_finalize();
}
