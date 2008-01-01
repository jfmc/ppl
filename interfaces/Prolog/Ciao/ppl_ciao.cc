/* Ciao Prolog interface: system-dependent part.
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
#include <ciao_prolog.h>
#include <cassert>
#include <sstream>
#include "ciao_cfli.hh"
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
  Prolog_has_unbounded_integers = true;
  Prolog_min_integer = 0;
  Prolog_max_integer = 0;
}

/*!
  Perform system-dependent de-initialization.
*/
void
ppl_Prolog_sysdep_deinit() {
}

inline int
Prolog_get_Coefficient(Prolog_term_ref t, PPL::Coefficient& n) {
  assert(Prolog_is_integer(t));
  if (ciao_fits_in_int(t))
    n = ciao_to_integer(t);
  else {
    const char* s = ciao_get_number_chars(t);
    n = PPL::Coefficient(s);
    // TODO: remove the const_cast when the Ciao people fix ciao_prolog.h.
    ciao_free(const_cast<char*>(s));
  }
  return 1;
}

inline int
Prolog_put_Coefficient(Prolog_term_ref& t, const PPL::Coefficient& n) {
  int i;
  if (PPL::assign_r(i, n, PPL::ROUND_NOT_NEEDED) == PPL::V_EQ)
    t = ciao_integer(i);
  else {
    std::ostringstream s;
    s << n;
    // TODO: remove the const_cast when the Ciao people fix ciao_prolog.h.
    t = ciao_put_number_chars(const_cast<char*>(s.str().c_str()));
  }
  return 1;
}

inline int
Prolog_unify_Coefficient(Prolog_term_ref t, const PPL::Coefficient& n) {
  Prolog_term_ref u = Prolog_new_term_ref();
  Prolog_put_Coefficient(u, n);
  return ciao_unify(t, u);
}

} // namespace

#include "../ppl_prolog.icc"

extern "C" void
init() {
  ppl_initialize();
}
