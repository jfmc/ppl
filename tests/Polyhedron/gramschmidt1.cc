/* Test Constraint_System::gram_schmidt().
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
#define NOISY 1
#endif

int
main() TRY {
  set_handlers();

  Variable A(0);
  Variable B(1);
  Variable C(2);

  C_Polyhedron ph1(3);
  ph1.add_constraint(A == 0);
  ph1.add_constraint(B == C);

  C_Polyhedron ph2(3);
  ph2.add_constraint(A == 1);
  ph2.add_constraint(B == C-1);

  ph1.poly_hull_assign_and_minimize(ph2);

  Constraint_System cs1 = ph1.constraints();

#if NOISY
  print_constraints(cs1, "*** cs1 ***");
#endif

  cs1.gram_schmidt();

#if NOISY
  print_constraints(cs1, "*** after cs1.gram_schmidt() ***");
#endif

  return 0; //retval;
}
CATCH
