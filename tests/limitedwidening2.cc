/* Testing Polyhedron::limited_widening().
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"
#include <iostream>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  Variable x(0);
  Variable y(1);

  ConSys cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 1);
  cs1.insert(y == 0);

  Polyhedron ph1(cs1);

#if NOISY
  print_constraints(ph1, "*** ph1 ****");
#endif

  ConSys cs2;
  cs2.insert(x <= 2);
  cs2.insert(y >= 0);
  cs2.insert(y <= x);

  Polyhedron ph2(cs2);

#if NOISY
  print_constraints(ph2, "*** ph2 ****");
#endif

  // Note: this is inconsistent with both `ph1' and `ph2'.
  ConSys cs(y <= -1);

#if NOISY
  print_constraints(cs, "*** cs ****");
#endif

  Polyhedron computed_result = ph2;
  computed_result.limited_widening_assign(ph1, cs);

#if NOISY
  print_constraints(computed_result, "*** After limited_widening_assign ****");
#endif

  // The result must be empty.
  return computed_result.check_empty() ? 0 : 1;
}
