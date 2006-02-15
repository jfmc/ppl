/* Test BD_Shape::bds_difference_assign().
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
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A >= 0);
  bd1.add_constraint(A <= -2);
  bd1.add_constraint(B == 0);

  TBD_Shape bd2(2);
  bd2.add_constraint(A >= 0);
  bd2.add_constraint(A <= 2);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** ph2 ***");

  bd1.bds_difference_assign(bd2);

  BD_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(ph2) ***");

  return ok;
}

bool
test02() {
  TBD_Shape bd1;
  TBD_Shape bd2;

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.bds_difference_assign(bd2);

  Constraint_System cs;
  cs.insert(Linear_Expression(-4) >= 0);
  BD_Shape<mpq_class> known_result(cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(bd2) ***");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(x <= 2);
  bd1.add_constraint(x >= 0);
  bd1.add_constraint(y <= 5);
  bd1.add_constraint(y >= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(x >= 1);
  bd2.add_constraint(y <= 4);
  bd2.add_constraint(y >= 1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.bds_difference_assign(bd2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(y <= 5);
  known_result.add_constraint(y >= 2);
  known_result.add_constraint(y - x >= 1);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(bd2) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(x <= 8);
  bd1.add_constraint(x >= 0);
  bd1.add_constraint(y <= 7);
  bd1.add_constraint(y >= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(x <= 3);
  bd2.add_constraint(x >= 1);
  bd2.add_constraint(y <= 0);
  bd2.add_constraint(y >= 1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.bds_difference_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(bd2) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(x <= 8);
  bd1.add_constraint(x >= 0);
  bd1.add_constraint(y <= 7);
  bd1.add_constraint(y >= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(x <= 9);
  bd2.add_constraint(x >= 0);
  bd2.add_constraint(y <= 8);
  bd2.add_constraint(y >= 1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.bds_difference_assign(bd2);

  BD_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(bd2) ***");

  return ok;
}

bool
test06() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(x <= 8);
  bd1.add_constraint(y <= 7);
  bd1.add_constraint(y >= 1);
  bd1.add_constraint(z <= 2);

  TBD_Shape bd2(3);
  bd2.add_constraint(x == 8);
  bd2.add_constraint(y <= 2);
  bd2.add_constraint(y >= 1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.bds_difference_assign(bd2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x <= 8);
  known_result.add_constraint(y <= 7);
  known_result.add_constraint(y >= 1);
  known_result.add_constraint(z <= 2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(bd2) ***");

  return ok;
}

bool
test07() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A >= 0);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(B >= 0);
  bd1.add_constraint(B <= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(A >= 2);
  bd2.add_constraint(A <= 4);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 2);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(B <= 2);

  bd1.bds_difference_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1, "*** After bd1.bds_difference_assign(bd2) ***");
  print_constraints(known_result, "*** known_result ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
END_MAIN
