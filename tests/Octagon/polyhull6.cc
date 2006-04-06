/* Test Octagon::poly_hull_assign().
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

  Constraint_System cs1;
  cs1.insert(A == 1);
  cs1.insert(C - A <= 2);
  cs1.insert(C - B <= 7);
  cs1.insert(B - A <= 3);


  TOctagon oct1(cs1);
  TOctagon oct2(3);

  Constraint_System cs2;
  cs2.insert(A == 1);
  cs2.insert(C - A <= 3);
  cs2.insert(C - A >= 7);
  cs2.insert(B - A <= 2);

  oct2.add_constraints(cs2);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct1.poly_hull_assign(oct2);

  TOctagon known_result(oct1);

#if NOISY
  print_constraints(oct1, "*** oct1.poly_hull_assign(oct2) ***");
#endif


  int retval = (oct1 == known_result) ? 0 : 1;

  return retval;
}
CATCH
