/* Ciao Prolog interface: system-dependent part.
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
#include <sstream>

#include "Coefficient.defs.hh"
#include "checked.defs.hh"
#include "checked_int.inlines.hh"
#include "checked_mpz.inlines.hh"
#include <ciao_prolog.h>
#include <cassert>

typedef ciao_term Prolog_term_ref;
typedef const char* Prolog_atom;
typedef ciao_bool Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = 1;
static const Prolog_foreign_return_type PROLOG_FAILURE = 0;

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

/*!
  True if and only if the Prolog engine supports unbounded integers.
*/
static bool Prolog_has_unbounded_integers;

/*!
  If \p Prolog_has_unbounded_integers is false, holds the maximum
  integer value representable by a Prolog integer.
  Holds zero otherwise.
*/
static long Prolog_max_integer;

/*!
  Performs system-dependent initialization.
*/
static void
ppl_Prolog_sysdep_init() {
  Prolog_has_unbounded_integers = true;
  Prolog_max_integer = 0;
}

/*!
  Perform system-dependent de-itialization.
*/
static void
ppl_Prolog_sysdep_deinit() {
}

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
  Assign to \p t a Prolog integer with value \p l.
*/
static inline int
Prolog_put_long(Prolog_term_ref& t, long l) {
  t = ciao_integer(l);
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p ul.
*/
static inline int
Prolog_put_ulong(Prolog_term_ref& t, unsigned long ul) {
  // FIXME!
  t = ciao_integer(ul);
  return 1;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
static inline int
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  t = ciao_atom(s);
  return 1;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline int
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  t = ciao_atom(a);
  return 1;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline int
Prolog_put_address(Prolog_term_ref& t, void* p) {
  t = ciao_pointer_to_address(ciao_implicit_state, p);
  return 1;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  return ciao_atom_name(ciao_atom(s));
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
  t = ciao_structure_a(f, 1, args);
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
  t = ciao_structure_a(f, 2, args);
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
  t = ciao_structure_a(f, 3, args);
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
  t = ciao_structure_a(f, 4, args);
  return 1;
}

/*!
  Assign to \p c a Prolog list whose head is \p h and tail is \p t.
*/
static inline int
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  c = ciao_list(h, t);
  return 1;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  ciao_raise_exception(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise.
*/
static inline int
Prolog_is_variable(Prolog_term_ref t) {
  return ciao_is_variable(t);
}

/*!
  Return true if \p t is a Prolog atom, false otherwise.
*/
static inline int
Prolog_is_atom(Prolog_term_ref t) {
  return ciao_is_atom(t);
}

/*!
  Return true if \p t is a Prolog integer, false otherwise.
*/
static inline int
Prolog_is_integer(Prolog_term_ref t) {
  return ciao_is_integer(t);
}

/*!
  Return true if \p t is the representation of an address, false otherwise.
*/
static inline int
Prolog_is_address(Prolog_term_ref t) {
  return ciao_is_address(ciao_implicit_state, t);
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise.
*/
static inline int
Prolog_is_compound(Prolog_term_ref t) {
  return ciao_is_structure(t);
}

/*!
  Return true if \p t is a Prolog list, false otherwise.
*/
static inline int
Prolog_is_cons(Prolog_term_ref t) {
  return ciao_is_list(t);
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
  if (ciao_fits_in_int(t)) {
    *lp = ciao_to_integer(t);
    return 1;
  }
  else
    // FIXME: what if the value does not fit in an int but fits in a long?
    return 0;
}

/*!
  If \p t is the Prolog representation for a memory address, return
  true and store that address into \p v; return false otherwise.
  The behavior is undefined if \p t is not an address.
*/
static inline int
Prolog_get_address(Prolog_term_ref t, void** vpp) {
  assert(Prolog_is_address(t));
  *vpp = ciao_address_to_pointer(ciao_implicit_state, t);
  return 1;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
static inline int
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom* ap) {
  assert(Prolog_is_atom(t));
  *ap = ciao_atom_name(t);
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
  *ap = ciao_structure_name(t);
  *ip = ciao_structure_arity(t);
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
  a = ciao_structure_arg(t, i);
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
  h = ciao_list_head(c);
  t = ciao_list_tail(c);
  return 1;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline int
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return ciao_unify(t, u);
}

static PPL::Coefficient
integer_term_to_Coefficient(Prolog_term_ref t) {
  assert(ciao_is_integer(t));
  long v;
  if (Prolog_get_long(t, &v))
    return PPL::Coefficient(v);
  else {
    char* s;
    s = ciao_get_number_chars(t);
    PPL::Coefficient n(s);
    ciao_free(s);
    return n;
  }
}

static Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  long v;
  if (PPL::Checked::assign<PPL::Check_Overflow_Policy>(v, PPL::raw_value(n))
      == PPL::Checked::V_EQ)
    return ciao_integer(v);
  else {
    std::ostringstream s;
    s << n;
    // FIXME: the following cast is really a bug in Ciao Prolog.
    return ciao_put_number_chars(const_cast<char*>(s.str().c_str()));
  }
}

#include "../ppl_prolog.icc"

extern "C" void
init() {
  ppl_initialize();
}
