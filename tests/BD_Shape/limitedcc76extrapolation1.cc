/* Test BD_Shape::limited_CC76_extrapolation_assign().
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
  TBD_Shape bd1(0);
  TBD_Shape bd2(0);
  Constraint_System cs;

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test2() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A == -2);

  TBD_Shape bd2(2);
  bd2.add_constraint(A == -2);
  bd2.add_constraint(B == 3);

  Constraint_System cs;
  cs.insert(A <= 0);
  cs.insert(A - B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test3() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 4);

  TBD_Shape bd2(2);
  bd2.add_constraint(A == -2);

  Constraint_System cs;
  cs.insert(A <= 0);
  cs.insert(A - B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(B >= 1);

  TBD_Shape bd2(3);
  bd2.add_constraint(A <= -2);
  bd2.add_constraint(B >= 4);

  Constraint_System cs;
  cs.insert(A <= 5);
  cs.insert(A - B + C <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A <= 5);
  known_result.add_constraint(B >= 1);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test5() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(B >= 1);

  TBD_Shape bd2(2);
  bd2.add_constraint(A <= -2);
  bd2.add_constraint(B >= 4);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A + B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(B >= 1);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test6() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(B >= 1);

  TBD_Shape bd2(2);
  bd2.add_constraint(A <= -2);
  bd2.add_constraint(B >= 4);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A - B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(B >= 1);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test7() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(A >= 5);
  bd1.add_constraint(B >= 1);

  TBD_Shape bd2(2);
  bd2.add_constraint(A <= -2);
  bd2.add_constraint(A >= 3);
  bd2.add_constraint(B >= 4);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A - B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test8() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(B >= 1);

  TBD_Shape bd2(2);
  bd2.add_constraint(A <= -2);
  bd2.add_constraint(A >= 3);
  bd2.add_constraint(B >= 4);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A - B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test9() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(4);
  bd1.add_constraint(A <= 4);
  bd1.add_constraint(B <= 6);
  bd1.add_constraint(C - D == 5);

  TBD_Shape bd2(4);
  bd2.add_constraint(A <= 4);
  bd2.add_constraint(C - D == 5);
  bd2.add_constraint(B <= 5);

  Constraint_System cs;
  cs.insert(A == 4);
  cs.insert(C - D == 5);
  cs.insert(A - B <= 6);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A <= 4);
  known_result.add_constraint(C - D == 5);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test10() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x <= 1);
  cs1.insert(y >= 4);

  Constraint_System cs2;
  cs2.insert(x == 0);
  cs2.insert(y >= 5);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 20);
  cs.insert(y >= 3);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ****");

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(y >= 3);
  known_result.add_constraint(y - x >= 2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test11() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(3, EMPTY);
  TBD_Shape bd2(3, EMPTY);

  Constraint_System cs;
  cs.insert(x <= 1);
  cs.insert(y >= 4);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test12() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x <= 1);
  cs1.insert(y >= 4);
  cs1.insert(x - y >= 2);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(2, EMPTY);

  Constraint_System cs2;
  cs2.insert(x <= 0);
  cs2.insert(y >= 3);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs2, "*** cs2 ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs2) ***");

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
  NEW_TEST(test8);
  NEW_TEST(test9);
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
END_MAIN
