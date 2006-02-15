/* Test BD_Shape::limited_BHMZ05_extrapolation_assign().
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

  Constraint_System cs1;
  cs1.insert(x <= 1);
  cs1.insert(y >= 4);

  Constraint_System cs2;
  cs2.insert(x <= 0);
  cs2.insert(y >= 5);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 20);
  cs.insert(y >= 3);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ***");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(y >= 3);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ****");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 2);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);

  Constraint_System cs2;
  cs2.insert(x >= 0);
  cs2.insert(x <= 1);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(y >= 0);
  cs.insert(x <= 5);
  cs.insert(y <= 5);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(x <= 5);
  known_result.add_constraint(y - x <= 0);
  known_result.add_constraint(x >= 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x >= 3);
  cs1.insert(x <= 2);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);

  Constraint_System cs2;
  cs2.insert(x >= 2);
  cs2.insert(x <= 1);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(y >= 0);
  cs.insert(x <= 5);
  cs.insert(y <= 5);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 1);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);

  Constraint_System cs2;
  cs2.insert(x >= 3);
  cs2.insert(x <= 2);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(y >= 0);
  cs.insert(x + y <= 0);
  cs.insert(x - y >= 0);
  cs.insert(x <= 5);
  cs.insert(y <= 5);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);
  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test05() {
  TBD_Shape bd1;
  TBD_Shape bd2;
  Constraint_System cs;

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result;

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test06() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 2);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);
  cs1.insert(z <= 0);

  Constraint_System cs2;
  cs2.insert(x >= 0);
  cs2.insert(x <= 1);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);
  cs2.insert(z <= 0);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(y >= 0);
  cs.insert(x <= 5);
  cs.insert(y <= 5);
  cs.insert(x - y + z <= 5);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(x <= 5);
  known_result.add_constraint(y - x <= 0);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(z <= 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test07() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs1;
  cs1.insert(x == 0);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);
  cs1.insert(z <= 0);

  Constraint_System cs2;
  cs2.insert(x == 0);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);
  cs2.insert(z <= -1);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x == 0);
  cs.insert(y >= 0);
  cs.insert(y <= 5);
  cs.insert(x - y + z <= 5);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(y - x <= 0);
  known_result.add_constraint(x == 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test08() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 2);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);
  cs1.insert(z <= 0);

  Constraint_System cs2;
  cs2.insert(x >= 0);
  cs2.insert(x <= 1);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);
  cs2.insert(z <= 0);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(y >= 0);
  cs.insert(x <= 5);
  cs.insert(y <= 5);
  cs.insert(2*x - 3*y <= 5);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(x <= 5);
  known_result.add_constraint(y - x <= 0);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(z <= 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test09() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 2);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);
  cs1.insert(z <= 0);

  Constraint_System cs2;
  cs2.insert(x >= 0);
  cs2.insert(x <= 1);
  cs2.insert(y >= 0);
  cs2.insert(x - y >= 0);
  cs2.insert(z <= 0);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(cs2);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(y >= 0);
  cs.insert(x <= 5);
  cs.insert(y <= 5);
  cs.insert(x - y >= 0);

  print_constraints(bd1, "*** bd1 ****");
  print_constraints(bd2, "*** bd2 ****");
  print_constraints(cs, "*** cs ****");

  bd1.limited_BHMZ05_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(x <= 5);
  known_result.add_constraint(y - x <= 0);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(z <= 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result) ;

  print_constraints(bd1,
		    "*** bd1.limited_BHMZ05_extrapolation_assign(bd2) ***");

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
  NEW_TEST(test08);
  NEW_TEST(test09);
END_MAIN
