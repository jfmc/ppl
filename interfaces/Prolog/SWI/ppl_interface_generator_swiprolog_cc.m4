dnl This file generates ppl_swiprolog.cc.
/* SWI Prolog interface.
include(`ppl_interface_generator_copyright')

*/

#include "ppl.hh"
#include "pwl.hh"

// Include gmp.h before SWI-Prolog.h.  This is required in order
// to get access to interface functions dealing with GMP numbers
// and SWI-Prolog terms.
#include <gmp.h>
#include <SWI-Prolog.h>
#include <cassert>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

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
  Temporary used to communicate big integers between C++ and Prolog.
*/
mpz_class tmp_mpz_class;

/*!
  Performs system-dependent initialization.
*/
void
ppl_Prolog_sysdep_init() {
  Prolog_has_unbounded_integers = true;
  Prolog_min_integer = 0;
  Prolog_max_integer = 0;
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
  PL_put_integer(t, l);
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p ul.
*/
inline int
Prolog_put_ulong(Prolog_term_ref t, unsigned long ul) {
  if (ul <= LONG_MAX)
    PL_put_integer(t, ul);
  else if (ul <= static_cast<uint64_t>(std::numeric_limits<int64_t>::max()))
    PL_put_int64(t, static_cast<int64_t>(ul));
  else {
    PPL::assign_r(tmp_mpz_class, ul, PPL::ROUND_NOT_NEEDED);
    PL_unify_mpz(t, tmp_mpz_class.get_mpz_t());
  }
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
  PL_get_mpz(t, tmp_mpz_class.get_mpz_t());
  return PPL::Coefficient(tmp_mpz_class);
}

Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  PPL::assign_r(tmp_mpz_class, n, PPL::ROUND_NOT_NEEDED);
  Prolog_term_ref t = Prolog_new_term_ref();
  PL_unify_mpz(t, tmp_mpz_class.get_mpz_t());
  return t;
}

} // namespace

#include "../ppl_prolog.icc"

#define PL_EXTENSION_ENTRY(name, arity) { #name, arity, (void*) name, 0 },

namespace {

PL_extension predicates[] = {
divert(1)dnl
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
divert`'dnl
include(`ppl_interface_generator_prolog_systems.m4')dnl
define(`m4_extension', `SPACES  PL_EXTENSION_ENTRY($1, $2)
')dnl
patsubst(ppl_prolog_sys_code, SPACES, `')dnl
