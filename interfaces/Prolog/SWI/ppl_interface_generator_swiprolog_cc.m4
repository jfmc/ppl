dnl This file generates ppl_swiprolog.cc.
/* SWI Prolog interface.
include(`ppl_interface_generator_copyright')`'dnl
*/

#define __STDC_LIMIT_MACROS
#include "ppl.hh"
#include "pwl.hh"
#include "swi_cfli.h"
#include "../exceptions.hh"
#include <cassert>

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
dnl
divert`'dnl
dnl
dnl Include common macros for generating system dependent code.
include(`ppl_interface_generator_prolog_systems.m4')dnl
dnl
dnl Redefine m4_extension as useful for SWI Prolog.
dnl m4_extension(Predicate_Name, Arity)
dnl Note: SPACES is just a marker to generated the two spaces of
dnl indentation following it.
define(`m4_extension', `dnl
SPACES  PL_EXTENSION_ENTRY($1, $2)
')dnl
dnl Now remove the marker SPACES.
patsubst(ppl_prolog_sys_code, SPACES, `')dnl
dnl
dnl End of file generation.
