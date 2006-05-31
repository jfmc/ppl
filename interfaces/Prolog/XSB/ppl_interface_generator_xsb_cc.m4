dnl This file generates ppl_xsb.cc.
/* XSB Prolog interface: system-dependent part.
include(`ppl_interface_generator_copyright')

*/

#include "ppl.hh"
#include "pwl.hh"
#include <cinterf.h>

// In XSB versions up to and including 2.7.1, <error_xsb.h> does not
// come with the extern "C" wrapper.
extern "C" {
#include <error_xsb.h>
}

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
  // XSB people claim XSB supports 32-bit integers.  However, experiments
  // suggest this is not the case.
  // See http://sourceforge.net/tracker/index.php?func=detail&aid=1400271&group_id=1176&atid=101176
#if 0
  Prolog_min_integer = -2147483647-1;
  Prolog_max_integer = 2147483647;
#else
  Prolog_min_integer = -268435456;
  Prolog_max_integer = 268435455;
#endif
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
  // TODO: remove the const_cast when the XSB people fix cinterf.h.
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
  // TODO: remove the const_cast when the XSB people fix cinterf.h.
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
  // XSB supports only 32-bit integers.
  long v;
  Prolog_get_long(t, &v);
  return PPL::Coefficient(v);
}

Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  long l = 0;
  if (PPL::assign_r(l, n, PPL::ROUND_NOT_NEEDED) != PPL::V_EQ)
    throw PPL_integer_out_of_range(n);
  Prolog_term_ref t = p2p_new();
  Prolog_put_long(t, l);
  return t;
}

} // namespace

divert(1)dnl

#include "../ppl_prolog.icc"

divert(2)dnl

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

divert(3)dnl

extern "C" void
init() {
  ppl_initialize();
}
divert`'dnl
include(`ppl_interface_generator_prolog_systems.m4')dnl
define(`extension', `#define $1 xsb_stub_$1
')dnl
ppl_prolog_sys_code`'dnl
undivert(1)`'dnl
divert`'dnl
define(`extension', `#undef $1
')dnl
ppl_prolog_sys_code`'dnl
undivert(2)`'dnl
divert`'dnl
define(`extension', `XSB_ENTRY_$2($1)
')dnl
ppl_prolog_sys_code`'dnl
