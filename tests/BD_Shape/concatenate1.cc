/* Test BD_Shape::concatenate_assign().
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
  // Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  // Variable x4(3);
  Variable x5(4);
  Variable x6(5);
  // Variable x7(6);
  Variable x8(7);
  Variable x9(8);


  TBD_Shape bd1(6);
  TBD_Shape bd2(3);


  bd1.add_constraint(x2 - x3 <= 0);
  bd1.add_constraint(x3 <= 2);
  bd1.add_constraint(x6 - x5 <= 2);
  bd1.add_constraint(x5 <= 3);

  bd2.add_constraint(x2 - x3 <= 2);
  bd2.add_constraint(x3 <= 7);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  bd1.concatenate_assign(bd2);

  TBD_Shape known_result(9);

  known_result.add_constraint(x2 - x3 <= 0);
  known_result.add_constraint(x3 <= 2);
  known_result.add_constraint(x6 - x5 <= 2);
  known_result.add_constraint(x5 <= 3);
  known_result.add_constraint(x8 - x9 <= 2);
  known_result.add_constraint(x9 <= 7);


#if NOISY
  print_constraints(bd1, "*** bd1.concatenate_assign(bd2) ***");
#endif

  int retval = (bd1 == known_result) ? 0 : 1;

  return retval;
}
CATCH
