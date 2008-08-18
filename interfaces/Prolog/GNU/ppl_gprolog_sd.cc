/* GNU Prolog interface: system-dependent part.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl.hh"
#include "gprolog_cfli.hh"

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

#include <iostream>
using namespace std;

/*!
  Performs system-dependent initialization.
*/
void
ppl_Prolog_sysdep_init() {
  Prolog_has_unbounded_integers = false;
  Prolog_min_integer = INT_LOWEST_VALUE;
  Prolog_max_integer = INT_GREATEST_VALUE;
}

/*!
  Perform system-dependent de-initialization.
*/
void
ppl_Prolog_sysdep_deinit() {
}

inline int
Prolog_get_Coefficient(Prolog_term_ref t, PPL::Coefficient& n) {
  long v;
  Prolog_get_long(t, &v);
  n = v;
  return 1;
}

int
Prolog_put_Coefficient(Prolog_term_ref& t, const PPL::Coefficient& n) {
  long l = 0;
  if (PPL::assign_r(l, n, PPL::ROUND_NOT_NEEDED) != PPL::V_EQ
      || !Prolog_put_long(t, l))
    throw PPL_integer_out_of_range(n);
  return 1;
}

int
Prolog_unify_Coefficient(Prolog_term_ref t, const PPL::Coefficient& n) {
  Prolog_term_ref u = Prolog_new_term_ref();
  Prolog_put_Coefficient(u, n);
  return Prolog_unify(t, u);
}

} // namespace

#undef CS

#include "../ppl_prolog_main.icc"
