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
  bd1.add_constraint(A >= 0);
  bd1.add_constraint(B >= 0);
  bd1.add_constraint(B <= 39);
  bd1.add_constraint(C >= 0);
  bd1.add_constraint(C <= 40);
  bd1.add_constraint(D >= 0);
  bd1.add_constraint(D <= 40);
  bd1.add_constraint(B - A <= 39);
  bd1.add_constraint(C - A <= 40);
  bd1.add_constraint(D - A <= 40);
  bd1.add_constraint(B - C <= 0);
  bd1.add_constraint(C - B <= 1);
  bd1.add_constraint(B - D <= 0);
  bd1.add_constraint(D - B <= 2);
  bd1.add_constraint(C - D <= 0);
  bd1.add_constraint(D - C <= 1);

  TBD_Shape bd2(4);
  bd2.add_constraint(A >= 0);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 38);
  bd2.add_constraint(C >= 0);
  bd2.add_constraint(C <= 39);
  bd2.add_constraint(D >= 0);
  bd2.add_constraint(D <= 40);
  bd2.add_constraint(B - A <= 38);
  bd2.add_constraint(C - A <= 39);
  bd2.add_constraint(D - A <= 40);
  bd2.add_constraint(B - C <= 0);
  bd2.add_constraint(C - B <= 1);
  bd2.add_constraint(B - D <= 0);
  bd2.add_constraint(D - B <= 2);
  bd2.add_constraint(C - D <= 0);
  bd2.add_constraint(D - C <= 1);

  // Force the closure of `bd1'.
  bd1 == bd2;
  
  TBD_Shape known_widening(4);
  known_widening.add_constraint(A >= 0);
  known_widening.add_constraint(B >= 0);
  known_widening.add_constraint(C >= 0);
  known_widening.add_constraint(D >= 0);
  known_widening.add_constraint(D <= 40);
  known_widening.add_constraint(D - A <= 40);
  known_widening.add_constraint(B - C <= 0);
  known_widening.add_constraint(C - B <= 1);
  known_widening.add_constraint(B - D <= 0);
  known_widening.add_constraint(D - B <= 2);
  known_widening.add_constraint(C - D <= 0);
  known_widening.add_constraint(D - C <= 1);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  bd1.CC76_extrapolation_assign(bd2);

#if NOISY
  print_constraints(bd1, "*** bd1.CC76_extrapolation_assign(bd2) ***");
#endif

  // Force the closure of `bd1'.
  bd1 == bd2;

#if NOISY
  print_constraints(bd1, "*** bd1.closure_assign() ***");
#endif

  return bd1 == known_widening ? 0 : 1;

}
CATCH
