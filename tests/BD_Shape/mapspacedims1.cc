/* Test BD_Shape::map_space_dimensions().
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
  Partial_Function function;

  TBD_Shape bd1(3);

  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");

  bd1.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result;

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");

  return ok;
}

bool
test02() {
  Partial_Function function;

  TBD_Shape bd1(3, EMPTY);

  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");

  bd1.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(0, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");

  return ok;
}

bool
test03() {
  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);

  TBD_Shape bd1(3, EMPTY);

  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");

  bd1.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Partial_Function function;
  function.insert(0, 0);
  function.insert(2, 1);

  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 3);

  TBD_Shape bd1(cs);

  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");

  bd1.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x == 1);
  known_result.add_constraint(y - x <= 3);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Partial_Function function;
  function.insert(2, 0);

  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 3);
  cs.insert(z - y <= 7);
  cs.insert(y - x <= 2);

  TBD_Shape bd1(cs);

  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");

  bd1.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(1);
  known_result.add_constraint(x <= 4);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");

  return ok;
}

bool
test06() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Partial_Function function;
  function.insert(2, 1);
  function.insert(1, 0);

  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 1);
  cs.insert(z - y <= 7);
  cs.insert(y - x <= 1);

  TBD_Shape bd1(cs);

  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");

  bd1.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y - x <= 7);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");

  return ok;
}

bool
test07() {
  Variable x(0);
  Variable y(1);

  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);

  TBD_Shape bd(2);
  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= -1);
  bd.add_constraint(y - x <= 3);

  print_constraints(bd, "*** bd ***");
  print_function(function, "*** function ***");

  bd.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x <= -1);
  known_result.add_constraint(y <= 1);
  known_result.add_constraint(x - y <= 3);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result) ;

  print_constraints(bd, "*** bd.map_space_dimension(function) ***");

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B >= 0);
  cs.insert(C >= 0);
  cs.insert(D == 0);
  cs.insert(B - A == 0);
  TBD_Shape bd(cs);

  Partial_Function function;
  function.insert(0, 2);
  function.insert(1, 1);
  function.insert(3, 0);

  print_function(function, "*** function ***");
  print_constraints(bd, "*** bd ***");

  bd.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 0);
  known_result.add_constraint(B - C == 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.map_space_dimensions(function) ***");

  return ok;
}

bool
test09() {
  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);

  TBD_Shape bd(0);

  print_constraints(bd, "*** bd ***");
  print_function(function, "*** function ***");

  bd.map_space_dimensions(function);

  BD_Shape<mpq_class> known_result(0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result) ;

  print_constraints(bd, "*** bd.map_space_dimension(function) ***");

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
