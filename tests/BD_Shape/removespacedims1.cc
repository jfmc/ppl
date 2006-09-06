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
test01() {
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
test02() {
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
test03() {
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
test04() {
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
test05() {
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
test06() {
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
test07() {
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

bool
test08() {
  TBD_Shape bd(5);

  try {
    // This is an invalid use of the function
    // BD_Shape::remove_higher_dimensions(n): it is illegal to erase
    // a variable that is not in the space of the polyhedron.
    bd.remove_higher_space_dimensions(7);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test09() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs;
  cs.insert(x <= 3);
  cs.insert(y - z <= 2);
  TBD_Shape bd(cs);

  Variables_Set to_be_removed;
  to_be_removed.insert(z);

  bd.remove_space_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // This is an incorrect use use of function
    // BD_Shape::remove_dimensions(to_be_remove).
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    bd.remove_space_dimensions(to_be_removed);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
END_MAIN
