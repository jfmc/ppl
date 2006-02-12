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

void
test1() {
  TBD_Shape bd1(0);

  TBD_Shape bd2(0);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  TBD_Shape known_result(bd1);

  Constraint_System cs;

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(2);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(3);
  known_result.add_constraint(A <= 5);
  known_result.add_constraint(B >= 1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(2);
  known_result.add_constraint(B >= 1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(2);
  known_result.add_constraint(B >= 1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(2, EMPTY);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

void
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

  TBD_Shape known_result(4);
  known_result.add_constraint(A <= 4);
  known_result.add_constraint(C - D == 5);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {
  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);

  return 0;
}
CATCH
