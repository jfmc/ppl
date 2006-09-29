dnl This file generates ppl_sicstus_sd.cc.
/* SICStus Prolog interface: system-dependent part.
include(`ppl_interface_generator_copyright')`'dnl
*/

#include "ppl.hh"
#include "pwl.hh"
#include "sicstus_cfli.h"
#include "../exceptions.hh"
#include <cassert>
#include <sstream>

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

PPL::Coefficient
integer_term_to_Coefficient(Prolog_term_ref t) {
  assert(SP_is_integer(t));
  long v;
  if (SP_get_integer(t, &v) != 0)
    return PPL::Coefficient(v);
  else {
    char* s;
    if (SP_get_number_chars(t, &s) != 0)
      return PPL::Coefficient(s);
    else
      throw unknown_interface_error("integer_term_to_Coefficient");
  }
}

Prolog_term_ref
Coefficient_to_integer_term(const PPL::Coefficient& n) {
  Prolog_term_ref t = Prolog_new_term_ref();
  long l = 0;
  if (PPL::assign_r(l, n, PPL::ROUND_NOT_NEEDED) == PPL::V_EQ) {
    if (SP_put_integer(t, l) == 0)
      throw unknown_interface_error("Coefficient_to_integer_term()");
  } else {
    std::ostringstream s;
    s << n;
    if (SP_put_number_chars(t, s.str().c_str()) == 0)
      throw unknown_interface_error("Coefficient_to_integer_term()");
  }
  return t;
}

} // namespace

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

#define SP_STUB_5(name) \
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
  Prolog_term_ref arg5 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(5, goal, arg5)) \
    return PROLOG_FAILURE; \
  return name(arg1, arg2, arg3, arg4, arg5); \
}

#define SP_STUB_6(name) \
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
  Prolog_term_ref arg5 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(5, goal, arg5)) \
    return PROLOG_FAILURE; \
  Prolog_term_ref arg6 = Prolog_new_term_ref(); \
  if (!Prolog_get_arg(6, goal, arg6)) \
    return PROLOG_FAILURE; \
  return name(arg1, arg2, arg3, arg4, arg5, arg6); \
}

divert(1)
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
divert(2)dnl
}

extern "C" void
ppl_sicstus_deinit(int /* when */) {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i)
    // SP_unregister_atom can fail.
    // We ignore such failures: what else can we do?
    (void) SP_unregister_atom(*prolog_atoms[i].p_atom);
  ppl_finalize();
}
dnl
divert`'dnl
dnl
dnl Include common macros for generating system dependent code.
include(`ppl_interface_generator_prolog_systems.m4')dnl
dnl
dnl Redefine m4_extension to generate SICStus stubs.
dnl m4_extension(Predicate_Name, Arity)
define(`m4_extension', `dnl
SP_STUB_$2($1)
')dnl
dnl Generate stubs.
ppl_prolog_sys_code`'dnl
undivert(1)`'dnl
dnl
divert`'dnl
dnl
dnl Redefine m4_extension to generate SICStus user predicates.
dnl m4_extension(Predicate_Name, Arity)
define(`m4_extension', `dnl
  SP_DEFINE_C_PREDICATE($1, $2);
')dnl
dnl Generate user predicates.
ppl_prolog_sys_code`'dnl
dnl
dnl End of file generation.

