m4_define(`dnl', `m4_dnl')`'dnl

dnl This m4 file generates the file ppl_xsb.cc.

m4_define(`dnl', `m4_dnl')
dnl Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
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

m4_include(`ppl_interface_generator_prolog_systems.m4')

m4_divert`'dnl
/* XSB Prolog interface: system-dependent part.
m4_include(`ppl_interface_generator_copyright')
*/

#include "ppl.hh"
#include "xsb_cfli.hh"
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
  Prolog_min_integer = XSB_min_integer;
  Prolog_max_integer = XSB_max_integer;
}

void
ppl_Prolog_sysdep_deinit() {
}

inline int
Prolog_get_Coefficient(Prolog_term_ref t, PPL::Coefficient& n) {
  // XSB supports only 32-bit integers.
  long l;
  Prolog_get_long(t, &l);
  n = l;
  return 1;
}

int
Prolog_put_Coefficient(Prolog_term_ref& t, const PPL::Coefficient& n) {
  long l = 0;
  if (PPL::assign_r(l, n, PPL::ROUND_NOT_NEEDED) != PPL::V_EQ)
    throw PPL_integer_out_of_range(n);
  return Prolog_put_long(t, l);
}

int
Prolog_unify_Coefficient(Prolog_term_ref t, const PPL::Coefficient& n) {
  Prolog_term_ref u = Prolog_new_term_ref();
  return Prolog_put_Coefficient(u, n) && Prolog_unify(t, u);
}

} // namespace

m4_divert(1)dnl

#include "../ppl_prolog_main.icc"

m4_divert(2)dnl

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

m4_divert(3)dnl

extern "C" void
init() {
  ppl_initialize();
}
m4_divert`'dnl
m4_define(`m4_extension', `#define $1 xsb_stub_$1
')dnl
ppl_prolog_sys_code`'dnl
m4_undivert(1)`'dnl
m4_divert`'dnl
m4_define(`m4_extension', `#undef $1
')dnl
ppl_prolog_sys_code`'dnl
m4_undivert(2)

m4_define(`m4_extension', `XSB_ENTRY_$2($1)
')

m4_divert`'dnl
ppl_prolog_sys_code`'dnl
dnl
dnl End of file generation.
