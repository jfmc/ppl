dnl This file generates ppl_yap.cc.
/* YAP Prolog interface: system-dependent part.
include(`ppl_interface_generator_copyright')`'dnl
*/

#include "ppl.hh"
#include "pwl.hh"
#include "yap_cfli.h"
#include "../exceptions.hh"
#include <cassert>
#include <climits>

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
