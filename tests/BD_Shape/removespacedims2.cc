/* Remove some variables from the space.
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

static void
test1() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);
  Variable x7(6);
  Variable x8(7);

  TBD_Shape bd1(8);

  bd1.add_constraint(x7 - x3 <= 0);
  bd1.add_constraint(x1 <= 2);
  bd1.add_constraint(x4 - x8 <= 2);
  bd1.add_constraint(x5 <= 7);
  bd1.add_constraint(x2 <= 10);
  bd1.add_constraint(x6 - x8 <= 4);


#if NOISY
  print_constraints(bd1, "*** bd1 ***");
#endif


  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x1);
  to_be_removed.insert(x2);
  to_be_removed.insert(x3);
  to_be_removed.insert(x4);
  to_be_removed.insert(x5);
  to_be_removed.insert(x6);
  to_be_removed.insert(x7);
  to_be_removed.insert(x8);
  bd1.remove_space_dimensions(to_be_removed);


  TBD_Shape known_result(0);

#if NOISY
  print_constraints(bd1, "*** bd1.remove_space_dimensions({x1,x2,x3,x4,x5,x6,x7,x8}) ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);

  TBD_Shape bd1(4);

  bd1.add_constraint(x1 - x2 <=1);
  bd1.add_constraint(x2 - x3 <= -2);
  bd1.add_constraint(x3 - x1 <= 0);
  bd1.add_constraint(x2 >= 5);
  bd1.add_constraint(x4 >= 3);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
#endif

 Variables_Set to_be_removed;
 to_be_removed.insert(x1);
 to_be_removed.insert(x3);
 to_be_removed.insert(x4);

 bd1.remove_space_dimensions(to_be_removed);

#if NOISY
  print_constraints(bd1, "*** bd1.remove_space_dimensions({x1,x3,x4}) ***");
#endif

 TBD_Shape known_result(1, EMPTY);

#if NOISY
  print_constraints(known_result, "*** known_result ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}


int
main() TRY {
  test1();
  test2();

  return 0;

}
CATCH
