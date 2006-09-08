/* Test BD_Shape::intersection_assign().
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
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(3);
  bd1.add_constraint(x <= 3);
  bd1.add_constraint(x - y <= 4);

  TBD_Shape bd2(3);
  bd2.add_constraint(-y <= -2);
  bd2.add_constraint(x - y <= 5);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.intersection_assign(bd2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x <= 3);
  known_result.add_constraint(-y <= -2);
  known_result.add_constraint(x - y <= 4);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(4);
  bd1.add_constraint(x - y <= 4);
  bd1.add_constraint(x <= 3);

  TBD_Shape bd2(4);
  bd2.add_constraint(x - y <= 5);
  bd2.add_constraint(-y <= -2);
  bd2.add_constraint(z - x <= 0);
  bd2.add_constraint(y - z <= -1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.intersection_assign(bd2);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(x - y <= 4);
  known_result.add_constraint(x <= 3);
  known_result.add_constraint(-y <= -2);
  known_result.add_constraint(z - x <= 0);
  known_result.add_constraint(y - z <= -1);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(x <= 4);
  bd1.add_constraint(-x <= -1);
  bd1.add_constraint(y <= 3);
  bd1.add_constraint(-y <= -1);
  bd1.add_constraint(x - y <= 1);

  TBD_Shape bd2(3);
  bd2.add_constraint(y - x <= -1);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(-y <= 5);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.intersection_assign(bd2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x <= 3);
  known_result.add_constraint(-x <= -1);
  known_result.add_constraint(y <= 3);
  known_result.add_constraint(-y <= -1);
  known_result.add_constraint(y - x <= -1);
  known_result.add_constraint(x - y <= 1);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(x <= 4);
  bd1.add_constraint(x >= 5);
  bd1.add_constraint(y <= 3);
  bd1.add_constraint(y >= 1);
  bd1.add_constraint(x - y <= 1);

  TBD_Shape bd2(3);
  bd2.add_constraint(y - x <= -1);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(y >= -5);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.intersection_assign(bd2);

  BD_Shape<mpq_class> known_result(3, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(x <= 4);
  bd1.add_constraint(x >= 5);
  bd1.add_constraint(y <= 3);
  bd1.add_constraint(y >= 1);
  bd1.add_constraint(x - y <= 1);

  TBD_Shape bd2(3);
  bd2.add_constraint(y - x <= -1);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(y >= -5);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd2.intersection_assign(bd1);

  BD_Shape<mpq_class> known_result(3, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd2) == known_result);

  print_constraints(bd2, "*** bd2.intersection_assign(bd1) ***");

  return ok;
}

bool
test06() {
  TBD_Shape bd1;
  TBD_Shape bd2;

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.intersection_assign(bd2);

  BD_Shape<mpq_class> known_result;

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test07() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);

  TBD_Shape bd2(3);
  bd2.add_constraint(y - x <= -1);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(y >= -5);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  BD_Shape<mpq_class> known_result(bd2);

  bd1.intersection_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test08() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);
  TBD_Shape bd2(3);
  bd2.add_constraint(y - x <= -1);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(y >= -5);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  BD_Shape<mpq_class> known_result(bd2);

  bd1.intersection_assign_and_minimize(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.intersection_assign(bd2) ***");

  return ok;
}

bool
test09() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(x >= y);

  TBD_Shape bd2(3);

  try {
    // This is an invalid use of function
    // BD_Shape::intersection_assign_and_minimize(bd2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    bd1.intersection_assign_and_minimize(bd2);
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
test10() {
  TBD_Shape bd1(7);
  TBD_Shape bd2(15);

  try {
    // This is an invalid use of function
    // BD_Shape::intersection_assign(bd2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    bd1.intersection_assign(bd2);
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
  DO_TEST(test10);
END_MAIN
