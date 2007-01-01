dnl Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_swiprolog.cc.
/* SWI Prolog interface.
m4_include(`ppl_interface_generator_copyright')`'dnl
*/

#define __STDC_LIMIT_MACROS
#include "ppl.hh"
#include "pwl.hh"
#include "swi_cfli.hh"
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

int
Prolog_get_Coefficient(Prolog_term_ref t, PPL::Coefficient& n) {
  assert(Prolog_is_integer(t));
  PL_get_mpz(t, tmp_mpz_class.get_mpz_t());
  n = tmp_mpz_class;
  return 1;
}

int
Prolog_unify_Coefficient(Prolog_term_ref t, const PPL::Coefficient& n) {
  PPL::assign_r(tmp_mpz_class, n, PPL::ROUND_NOT_NEEDED);
  return PL_unify_mpz(t, tmp_mpz_class.get_mpz_t());
}

int
Prolog_put_Coefficient(Prolog_term_ref t, const PPL::Coefficient& n) {
  PL_put_variable(t);
  return Prolog_unify_Coefficient(t, n);
}

} // namespace

#include "../ppl_prolog.icc"

#define PL_EXTENSION_ENTRY(name, arity) { #name, arity, (void*) name, 0 },

namespace {

PL_extension predicates[] = {
m4_divert(1)dnl
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
m4_divert`'dnl
dnl
dnl Include common macros for generating system dependent code.
m4_include(`ppl_interface_generator_prolog_systems.m4')dnl
dnl
dnl Redefine m4_extension as useful for SWI Prolog.
dnl m4_extension(Predicate_Name, Arity)
dnl Note: SPACES is just a marker to generated the two spaces of
dnl indentation following it.
m4_define(`m4_extension', `dnl
SPACES  PL_EXTENSION_ENTRY($1, $2)
')dnl
dnl Now remove the marker SPACES.
m4_patsubst(ppl_prolog_sys_code, SPACES, `')dnl
dnl
dnl End of file generation.
