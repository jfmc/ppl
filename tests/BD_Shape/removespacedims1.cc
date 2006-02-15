/* Remove some variables from the space.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

namespace {

bool
test1() {
  // Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);

  TBD_Shape bd1(6);
  bd1.add_constraint(x2 - x3 <= 0);
  bd1.add_constraint(x3 <= 2);
  bd1.add_constraint(x6 - x5 <= 2);
  bd1.add_constraint(x5 <= 3);

  print_constraints(bd1, "*** bd1 ***");

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x3);
  to_be_removed.insert(x5);

  bd1.remove_space_dimensions(to_be_removed);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(x2 <= 2);
  known_result.add_constraint(x4 <= 5);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** bd1.remove_space_dimensions({x3,x5}) ***");

  return ok;
}

bool
test2() {
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

  print_constraints(bd1, "*** bd1 ***");

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

  BD_Shape<mpq_class> known_result(0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.remove_space_dimensions"
		    "({x1,x2,x3,x4,x5,x6,x7,x8}) ***");

  return ok;
}

bool
test3() {
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

  print_constraints(bd1, "*** bd1 ***");

  Variables_Set to_be_removed;
  to_be_removed.insert(x1);
  to_be_removed.insert(x3);
  to_be_removed.insert(x4);

  bd1.remove_space_dimensions(to_be_removed);

  BD_Shape<mpq_class> known_result(1, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.remove_space_dimensions({x1,x3,x4}) ***");

  return ok;
}

bool
test4() {
  Variable x1(0);
  Variable x2(1);

  TBD_Shape bd(2);
  bd.add_constraint(x1 <= 2);
  bd.add_constraint(x2 <= 10);

  print_constraints(bd, "*** bd ***");

  bd.remove_higher_space_dimensions(0);

  BD_Shape<mpq_class> known_result(0, UNIVERSE);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(0) ***");

  return ok;
}

bool
test5() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);

  TBD_Shape bd(4);
  bd.add_constraint(x1 - x2 <=1);
  bd.add_constraint(x2 - x3 <= -2);
  bd.add_constraint(x3 - x1 <= 0);
  bd.add_constraint(x2 >= 5);
  bd.add_constraint(x4 >= 3);

  print_constraints(bd, "*** bd ***");

  bd.remove_higher_space_dimensions(1);

  BD_Shape<mpq_class> known_result(1, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(1) ***");

  return ok;
}

bool
test6() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);

  TBD_Shape bd(5);
  bd.add_constraint(x1 - x2 <=1);
  bd.add_constraint(x2 - x3 <= 2);
  bd.add_constraint(x3 - x1 <= 0);
  bd.add_constraint(x2 >= 5);
  bd.add_constraint(x4 >= 3);
  bd.add_constraint(x5 - x3 == 2);

  print_constraints(bd, "*** bd ***");

  bd.remove_higher_space_dimensions(3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x1 - x2 <=1);
  known_result.add_constraint(x2 - x3 <= 2);
  known_result.add_constraint(x3 - x1 <= 0);
  known_result.add_constraint(x2 >= 5);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(3) ***");

  return ok;
}

bool
test7() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  TBD_Shape bd(3);
  bd.add_constraint(x1 - x2 <=1);
  bd.add_constraint(x2 - x3 <= 2);
  bd.add_constraint(x3 - x1 <= 0);
  bd.add_constraint(x2 >= 5);

  print_constraints(bd, "*** bd ***");

  BD_Shape<mpq_class> known_result(bd);

  bd.remove_higher_space_dimensions(3);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(3) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test1);
  NEW_TEST(test2);
  NEW_TEST(test3);
  NEW_TEST(test4);
  NEW_TEST(test5);
  NEW_TEST(test6);
  NEW_TEST(test7);
END_MAIN
