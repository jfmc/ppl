/* GNU Prolog interface: system-dependent part.
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
#include <gprolog.h>
#include <cassert>

typedef PlTerm Prolog_term_ref;
typedef int Prolog_atom;
typedef Bool Prolog_foreign_return_type;
static const Prolog_foreign_return_type PROLOG_SUCCESS = TRUE;
static const Prolog_foreign_return_type PROLOG_FAILURE = FALSE;

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

static Prolog_atom a_dollar_address;
static Prolog_atom a_throw;

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

#include <iostream>
using namespace std;

/*!
  Performs system-dependent initialization.
*/
static void
ppl_Prolog_sysdep_init() {
  a_dollar_address = Create_Allocate_Atom("$address");
  a_throw = Find_Atom("throw");
  Prolog_has_unbounded_integers = false;
  Prolog_max_integer = INT_GREATEST_VALUE;
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
  Assign to \p t a Prolog integer with value \p i.
*/
static inline int
Prolog_put_long(Prolog_term_ref& t, long i) {
  t = Mk_Integer(i);
  return 1;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
static inline int
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  // FIXME: the following cast is really a bug in GNU Prolog.
  t = Mk_Atom(Create_Allocate_Atom(const_cast<char*>(s)));
  return 1;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
static inline int
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  t = Mk_Atom(a);
  return 1;
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
static inline int
Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			  Prolog_term_ref a1) {
  args[0] = a1;
  t = Mk_Compound(f, 1, args);
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
  t = Mk_Compound(f, 2, args);
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
  t = Mk_Compound(f, 3, args);
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
  t = Mk_Compound(f, 4, args);
  return 1;
}

/*!
  Assign to \p c a Prolog list whose head is \p h and tail is \p t.
*/
static inline int
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  args[0] = h;
  args[1] = t;
  c = Mk_List(args);
  return 1;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
static inline int
Prolog_put_address(Prolog_term_ref& t, void* p) {
  union {
    unsigned long l;
    unsigned short s[2];
  } u;
  u.l = reinterpret_cast<unsigned long>(p);
  return Prolog_construct_compound(t, a_dollar_address,
				   Mk_Positive(u.s[0]), Mk_Positive(u.s[1]));
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
static inline void
Prolog_raise_exception(Prolog_term_ref t) {
  Pl_Exec_Continuation(a_throw, 1, &t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise.
*/
static inline int
Prolog_is_variable(Prolog_term_ref t) {
  return Blt_Var(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog atom, false otherwise.
*/
static inline int
Prolog_is_atom(Prolog_term_ref t) {
  return Blt_Atom(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise.
*/
static inline int
Prolog_is_integer(Prolog_term_ref t) {
  return Blt_Integer(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise.
*/
static inline int
Prolog_is_compound(Prolog_term_ref t) {
  return Blt_Compound(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise.
*/
static inline int
Prolog_is_cons(Prolog_term_ref t) {
  if (Blt_Compound(t) == FALSE)
    return 0;
  Prolog_atom name;
  int arity;
  Rd_Compound(t, &name, &arity);
  return name == ATOM_CHAR('.') && arity == 2;
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
  *lp = Rd_Integer_Check(t);
  return 1;
}

/*!
  Return true if \p t is the representation of an address, false otherwise.
*/
static inline int
Prolog_is_address(Prolog_term_ref t) {
  if (!Prolog_is_compound(t))
    return 0;
  Prolog_atom name;
  int arity;
  Prolog_term_ref* a = Rd_Compound_Check(t, &name, &arity);
  if (name != a_dollar_address || arity != 2)
    return 0;
  for (int i = 0; i <= 1; ++i) {
    if (!Prolog_is_integer(a[i]))
      return 0;
    long l;
    if (!Prolog_get_long(a[i], &l))
      return 0;
    if (l < 0 || l > USHRT_MAX)
      return 0;
  }
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
  static Prolog_atom dummy_name;
  static int dummy_arity;
  Prolog_term_ref* a = Rd_Compound_Check(t, &dummy_name, &dummy_arity);
  union {
    unsigned long l;
    unsigned short s[2];
  } u;
  u.s[0] = Rd_Integer_Check(a[0]);
  u.s[1] = Rd_Integer_Check(a[1]);
  *vpp = reinterpret_cast<void*>(u.l);
  return 1;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
static inline int
Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom* ap) {
  assert(Prolog_is_atom(t));
  *ap = Rd_Atom_Check(t);
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
  Rd_Compound_Check(t, ap, ip);
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
  static Prolog_atom dummy_name;
  static int dummy_arity;
  a = Rd_Compound_Check(t, &dummy_name, &dummy_arity)[i-1];
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
  Prolog_term_ref* ht = Rd_List_Check(c);
  h = ht[0];
  t = ht[1];
  return 1;
}

/*!
  Unify the terms referenced by \p t and \p u and return true
  if the unification is successful; return false otherwise.
*/
static inline int
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return Unify(t, u) != FALSE;
}

static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: does GNU Prolog support unlimited precision integer?
  long v;
  Prolog_get_long(t, &v);
  return PPL::Integer(v);
}

static Prolog_term_ref
Integer_to_integer_term(const PPL::Integer& n) {
  // FIXME: does GNU Prolog support unlimited precision integer?
  if (!n.fits_slong_p())
    throw_unknown_interface_error("Integer_to_integer_term()");
  return Mk_Integer(n.get_si());
}

#include "../ppl_prolog.icc"
