/* Test BD_Shape::CC76_extrapolation_assign().
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
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);


  TBD_Shape bd1(4);
  TBD_Shape bd2(4);

  bd1.add_constraint(A >= 0);
  bd1.add_constraint(B >= 0);
  bd1.add_constraint(B <= 25);
  bd1.add_constraint(C >= 0);
  bd1.add_constraint(C <= 29);
  bd1.add_constraint(D >= 0);
  bd1.add_constraint(D <= 27);
  bd1.add_constraint(B - A <= 25);
  bd1.add_constraint(C - A <= 29);
  bd1.add_constraint(D - A <= 27);
  bd1.add_constraint(B - C <= 2);
  bd1.add_constraint(C - B <= 6);
  bd1.add_constraint(B - D <= 2);
  bd1.add_constraint(D - B <= 4);
  bd1.add_constraint(C - D <= 4);
  bd1.add_constraint(D - C <= 4);

  bd2.add_constraint(A >= 0);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 26);
  bd2.add_constraint(C >= 3);
  bd2.add_constraint(C <= 29);
  bd2.add_constraint(D >= 2);
  bd2.add_constraint(D <= 28);
  bd2.add_constraint(B - A <= 26);
  bd2.add_constraint(C - A <= 29);
  bd2.add_constraint(D - A <= 28);
  bd2.add_constraint(B - C <= 0);
  bd2.add_constraint(C - B <= 6);
  bd2.add_constraint(B - D == 2);
  bd2.add_constraint(C - D <= 4);
  bd2.add_constraint(D - C <= 2);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B >= 0);
  cs.insert(C >= 0);
  cs.insert(D >= 0);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");
#endif

  bd1.poly_hull_assign(bd2);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);


#if NOISY
  print_constraints(bd1,
		    "bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");
#endif

 TBD_Shape known_result(4);
 known_result.add_constraint(A >= 0);
 known_result.add_constraint(B >= 0);
 known_result.add_constraint(B <= 26);
 known_result.add_constraint(C >= 0);
 known_result.add_constraint(D >= 0);
 known_result.add_constraint(B - A <= 26);
 known_result.add_constraint(B - C <= 2);
 known_result.add_constraint(B - D <= 2);
 known_result.add_constraint(C - D <= 4);

  return bd1 == known_result ? 0 : 1;

}
CATCH
