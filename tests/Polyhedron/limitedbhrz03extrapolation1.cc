/* Test Polyhedron::limited_BHRZ03_extrapolation_assign().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable A(0);
  Variable B(1);

  Generator_System gs1;
  gs1.insert(point());
  gs1.insert(point(A + B));
  gs1.insert(point(A));
  C_Polyhedron ph1(gs1);

  Generator_System gs2;
  gs2.insert(point());
  gs2.insert(point(2*A));
  gs2.insert(point(2*A + 2*B));
  C_Polyhedron ph2(gs2);

  Constraint_System cs;
  cs.insert(A <= 5);
  cs.insert(B <= 4);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
  print_constraints(cs, "*** cs ***");
#endif

  ph2.limited_BHRZ03_extrapolation_assign(ph1, cs);

  C_Polyhedron known_result(2);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(A - B >= 0);
  known_result.add_constraint(B <= 4);
  known_result.add_constraint(A <= 5);

  int retval = (ph2 == known_result) ? 0 : 1;

#if NOISY
  print_constraints(ph2, "*** After ph2.limited_BHRZ03_widening(ph1, cs)***");
#endif

  return retval;
}
CATCH
