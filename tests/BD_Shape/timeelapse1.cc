/* Test time_elapse_assign() for particular polyhedra.
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
  TBD_Shape oc1(2, EMPTY);
  TBD_Shape oc2(2);

  print_constraints(oc1, "**** oc1 ****");
  print_constraints(oc2, "**** oc2 ****");

  oc1.time_elapse_assign(oc2);

  TBD_Shape oc3(2);
  TBD_Shape oc4(2, EMPTY);

  print_constraints(oc3, "**** oc3 ****");
  print_constraints(oc4, "**** oc4 ****");

  oc3.time_elapse_assign(oc4);

  bool ok = (oc1.is_empty()
		&& oc3.is_empty()) ;

  print_constraints(oc1, "**** oc1_time_elapse_assign(oc2) ****");
  print_constraints(oc3, "**** oc3_time_elapse_assign(oc4) ****");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);

  TBD_Shape oc1(2);
  oc1.add_constraint(x >= 0);
  oc1.add_constraint(y >= 0);
  oc1.add_constraint(x + y - 2 <= 0);

  TBD_Shape oc2(2);
  oc2.add_constraint(x >= 2);
  oc2.add_constraint(x <= 4);
  oc2.add_constraint(y == 3);

  print_constraints(oc1, "**** oc1 ****");
  print_constraints(oc2, "**** oc2 ****");

  oc1.time_elapse_assign(oc2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y >= 0);

  bool ok = (BD_Shape<mpq_class>(oc1) == known_result) ;

  print_constraints(oc1, "**** oc1_time_elapse_assign(oc2) ****");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);

  TBD_Shape oc1(2);
  oc1.add_constraint(x >= 1);
  oc1.add_constraint(x <= 3);
  oc1.add_constraint(y >= 1);
  oc1.add_constraint(y <= 3);

  TBD_Shape oc2(2);
  oc2.add_constraint(y == 5);

  print_constraints(oc1, "**** oc1 ****");
  print_constraints(oc2, "**** oc2 ****");

  oc1.time_elapse_assign(oc2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(y >= 1);

  bool ok = (BD_Shape<mpq_class>(oc1) == known_result) ;

  print_constraints(oc1, "**** oc1_time_elapse_assign(oc2) ****");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);

  TBD_Shape oc1(3);
  oc1.add_constraint(x <= 3);
  oc1.add_constraint(y <= 5);

  TBD_Shape oc2(3);
  oc2.add_constraint(x <= 2);
  oc2.add_constraint(y <= 3);

  print_constraints(oc1, "**** oc1 ****");
  print_constraints(oc2, "**** oc2 ****");

  oc1.time_elapse_assign(oc2);

  BD_Shape<mpq_class> known_result(3);

  bool ok = (BD_Shape<mpq_class>(oc1) == known_result) ;

  print_constraints(oc1, "**** oc1_time_elapse_assign(oc2) ****");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape oc1(3);
  oc1.add_constraint(x <= 2);
  oc1.add_constraint(x >= 1);
  oc1.add_constraint(y <= 5);
  oc1.add_constraint(y >= 10);
  oc1.add_constraint(z >= 1);

  TBD_Shape oc2(3);
  oc2.add_constraint(x <= 9);
  oc2.add_constraint(x >= 0);
  oc2.add_constraint(y <= 3);
  oc2.add_constraint(y >= -1);
  oc2.add_constraint(z >= 2);

  print_constraints(oc1, "**** oc1 ****");
  print_constraints(oc2, "**** oc2 ****");

  oc1.time_elapse_assign(oc2);

  BD_Shape<mpq_class> known_result(3, EMPTY);

  bool ok = (BD_Shape<mpq_class>(oc1) == known_result) ;

  print_constraints(oc1, "**** oc1.time_elapse_assign(oc2) ****");

  return ok;
}

bool
test06() {
  Variable x(0);

  TBD_Shape bd1(1);
  bd1.add_constraint(x == 1);

  TBD_Shape bd2(2);

  try {
    // This is an invalid use of the function
    // BD_Shape::time_elapse_assign(bd2): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    bd1.time_elapse_assign(bd2);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
END_MAIN
