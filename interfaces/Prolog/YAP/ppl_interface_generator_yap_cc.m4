dnl This file generates ppl_yap.cc.
/* YAP Prolog interface: system-dependent part.
include(`ppl_interface_generator_copyright')`'dnl
*/

#include "ppl.hh"
#include "pwl.hh"
#include <Yap/YapInterface.h>
#include <cassert>
#include <cassert>
#include <climits>

typedef YAP_Term Prolog_term_ref;
typedef YAP_Atom Prolog_atom;
typedef YAP_Bool Prolog_foreign_return_type;

namespace {

const Prolog_foreign_return_type PROLOG_SUCCESS = TRUE;
const Prolog_foreign_return_type PROLOG_FAILURE = FALSE;

} // namespace

#include "../exceptions.hh"

namespace PPL = Parma_Polyhedra_Library;

namespace {

Prolog_atom a_throw;

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

  a_throw = YAP_LookupAtom("throw");
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
  return 0;
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
  t = YAP_MkIntTerm(l);
  return 1;
}

/*!
  Assign to \p t a Prolog integer with value \p ul.
*/
inline int
Prolog_put_ulong(Prolog_term_ref& t, unsigned long ul) {
  if (ul <= LONG_MAX)
    t = YAP_MkIntTerm(ul);
  else {
    tmp_mpz_class = ul;
    t = YAP_MkBigNumTerm(tmp_mpz_class.get_mpz_t());
  }
  return 1;
}

/*!
  Assign to \p t an atom whose name is given
  by the null-terminated string \p s.
*/
inline int
Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
  t = YAP_MkAtomTerm(YAP_FullLookupAtom(s));
  return 1;
}

/*!
  Assign to \p t the Prolog atom \p a.
*/
inline int
Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
  t = YAP_MkAtomTerm(a);
  return 1;
}

/*!
  Assign to \p t a term representing the address contained in \p p.
*/
inline int
Prolog_put_address(Prolog_term_ref& t, void* p) {
  t = YAP_MkIntTerm(reinterpret_cast<long>(p));
  return 1;
}

/*!
  Return an atom whose name is given by the null-terminated string \p s.
*/
Prolog_atom
Prolog_atom_from_string(const char* s) {
  return YAP_FullLookupAtom(s);
}

Prolog_term_ref args[4];

/*!
  Assign to \p t a compound term whose principal functor is \p f
  of arity 1 with argument \p a1.
*/
inline int
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
inline int
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
inline int
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
inline int
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
inline int
Prolog_construct_cons(Prolog_term_ref& c,
		      Prolog_term_ref h, Prolog_term_ref t) {
  c = YAP_MkPairTerm(h, t);
  return 1;
}

/*!
  Raise a Prolog exception with \p t as the exception term.
*/
inline void
Prolog_raise_exception(Prolog_term_ref t) {
  YAP_Throw(t);
}

/*!
  Return true if \p t is a Prolog variable, false otherwise.
*/
inline int
Prolog_is_variable(Prolog_term_ref t) {
  return YAP_IsVarTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog atom, false otherwise.
*/
inline int
Prolog_is_atom(Prolog_term_ref t) {
  return YAP_IsAtomTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog integer, false otherwise.
*/
inline int
Prolog_is_integer(Prolog_term_ref t) {
  return YAP_IsIntTerm(t) != FALSE || YAP_IsBigNumTerm(t) != FALSE;
}

/*!
  Return true if \p t is the representation of an address, false otherwise.
*/
inline int
Prolog_is_address(Prolog_term_ref t) {
  return YAP_IsIntTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog compound term, false otherwise.
*/
inline int
Prolog_is_compound(Prolog_term_ref t) {
  return YAP_IsApplTerm(t) != FALSE;
}

/*!
  Return true if \p t is a Prolog list, false otherwise.
*/
inline int
Prolog_is_cons(Prolog_term_ref t) {
  return YAP_IsPairTerm(t) != FALSE;
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
  if (YAP_IsBigNumTerm(t) != FALSE) {
    YAP_BigNumOfTerm(t, tmp_mpz_class.get_mpz_t());
    if (tmp_mpz_class >= LONG_MIN && tmp_mpz_class <= LONG_MAX)
      PPL::assign_r(*lp, tmp_mpz_class, PPL::ROUND_NOT_NEEDED);
    else
      return 0;
  }
  else
    *lp = YAP_IntOfTerm(t);
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
  *vpp = reinterpret_cast<void*>(YAP_IntOfTerm(t));
  return 1;
}

/*!
  If \p t is a Prolog atom, return true and store its name into \p name.
  The behavior is undefined if \p t is not a Prolog atom.
*/
inline int
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
inline int
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
inline int
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
inline int
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
inline int
Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
  return YAP_Unify(t, u) != FALSE;
}

PPL::Coefficient
integer_term_to_Coefficient(Prolog_term_ref t) {
  PPL::Coefficient n;
  if (YAP_IsBigNumTerm(t) != FALSE) {
    YAP_BigNumOfTerm(t, tmp_mpz_class.get_mpz_t());
    n = tmp_mpz_class;
  }
  else
    n = YAP_IntOfTerm(t);
  return n;
}

Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  if (n >= LONG_MIN && n <= LONG_MAX) {
    long l = 0;
    PPL::assign_r(l, n, PPL::ROUND_NOT_NEEDED);
    return YAP_MkIntTerm(l);
  }
  else {
    PPL::assign_r(tmp_mpz_class, n, PPL::ROUND_NOT_NEEDED);
    return YAP_MkBigNumTerm(tmp_mpz_class.get_mpz_t());
  }
}

} // namespace

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

#define YAP_STUB_6(name) \
extern "C" Prolog_foreign_return_type \
yap_stub_##name() { \
  Prolog_term_ref arg1 = YAP_ARG1; \
  Prolog_term_ref arg2 = YAP_ARG2; \
  Prolog_term_ref arg3 = YAP_ARG3; \
  Prolog_term_ref arg4 = YAP_ARG4; \
  Prolog_term_ref arg5 = YAP_ARG5; \
  Prolog_term_ref arg6 = YAP_ARG6; \
  return name(arg1, arg2, arg3, arg4, arg5, arg6); \
}

dnl
dnl Place here YAP_STUB macros.
dnl
divert(1)dnl

#define YAP_USER_C_PREDICATE(name, arity) \
 YAP_UserCPredicate(#name, reinterpret_cast<int(*)()>(yap_stub_##name), arity)

extern "C" void
init() {
  ppl_initialize();

dnl
dnl Place here YAP_USER_C_PREDICATE macros.
dnl
divert(2)dnl
}
dnl
divert`'dnl
dnl
dnl Include common macros for generating system dependent code.
include(`ppl_interface_generator_prolog_systems.m4')dnl
dnl
dnl Redefine m4_extension to generate YAP stubs.
dnl m4_extension(Predicate_Name, Arity)
define(`m4_extension', `dnl
YAP_STUB_$2($1)
')dnl
dnl Generate stubs.
ppl_prolog_sys_code`'dnl
undivert(1)`'dnl
dnl
divert`'dnl
dnl
dnl Redefine m4_extension to generate YAP user predicates.
dnl m4_extension(Predicate_Name, Arity)
define(`m4_extension', `dnl
  YAP_USER_C_PREDICATE($1, $2);
')dnl
dnl Generate user predicates.
ppl_prolog_sys_code`'dnl
dnl
dnl End of file generation.
