/* SWI Prolog interface.
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
#include <SWI-Prolog.h>
#include <cassert>

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
Prolog_construct_cons(Prolog_term_ref c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  PL_cons_list(c, h, t);
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
  Return true if \p t is a Prolog atom, false otherwise. 
*/
static inline bool
Prolog_is_atom(Prolog_term_ref t) {
  return PL_is_atom(t) != 0;
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
Prolog_is_cons(Prolog_term_ref t) {
  return (PL_is_atom(t) == 0) && (PL_is_list(t) != 0);
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
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
static inline bool
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom& name) {
  assert(Prolog_is_atom(t));
  return PL_get_atom(t, &name) != 0;
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

#include <iostream>
using namespace std;
/*!
  If \p l is a Prolog list, assign its head and tail to \p h and \p t,
  respectively.
  The behavior is undefined if \p l is not a Prolog list.
*/
static inline bool
Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref h, Prolog_term_ref t) {
  assert(Prolog_is_cons(c));
  return PL_get_list(c, h, t) != 0;
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

static void
ppl_Prolog_sysdep_init() {
}

#include "../ppl_prolog.icc"

#define PL_EXTENSION_ENTRY(name, arity) { #name, arity, (void*) name, 0 },

static PL_extension predicates[] = {
  PL_EXTENSION_ENTRY(ppl_initialize, 0)
  PL_EXTENSION_ENTRY(ppl_finalize, 0)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_dimension, 3)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_empty_from_dimension, 3)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_Polyhedron, 4)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_ConSys, 3)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_GenSys, 3)
  PL_EXTENSION_ENTRY(ppl_new_Polyhedron_from_bounding_box, 3)
  PL_EXTENSION_ENTRY(ppl_delete_Polyhedron, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_space_dimension, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_intersection_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_intersection_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_hull_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_hull_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_difference_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_poly_difference_assign_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_widening_CC92_assign, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_limited_widening_CC92_assign, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_minimized_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_generators, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_minimized_generators, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraint, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generator, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_constraints_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generators, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_generators_and_minimize, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_dimensions_and_constraints, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_dimensions_and_project, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_add_dimensions_and_embed, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_remove_dimensions, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_remove_higher_dimensions, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_affine_image, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_affine_preimage, 4)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_relation_with_constraint, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_relation_with_generator, 3)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_check_empty, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_check_universe, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_is_bounded, 1)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_contains_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_strictly_contains_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_equals_Polyhedron, 2)
  PL_EXTENSION_ENTRY(ppl_Polyhedron_get_bounding_box, 2)
  { NULL, 0, NULL, 0 }
};

extern "C" install_t
install() {
  ppl_initialize();
  PL_register_extensions(predicates);
}

extern "C" install_t
uninstall() {
  ppl_finalize();
}
