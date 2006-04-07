/* Test BD_Shape::add_constraints_and_minimize().
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

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B == 5);

  TBD_Shape bd1(2);
  bd1.add_constraints_and_minimize(cs);

  print_constraints(bd1, "*** bd1.add_constraints_and_minimize(cs) ***");

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B == 5);
  known_result.add_constraint(B - A <= 5);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(known_result, "*** known_result ***");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(2);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a strict inequality.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y < 0);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test03() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(2);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a constraint with bigger dimension.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y - x + z >= 0);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test04() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(3);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a constraint with three dimension.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y - x + z >= 0);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test05() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a constraint with two different coefficients.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(2*y - 3*x <= 0);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test06() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(1);

  try {
    // This is an invalid use of the function
    // BD_Shape::add_constraints_and_minimize(cs): it is illegal to
    // add a system of constraints that is not dimensional incompatible
    // with the polyhedron.
    Constraint_System cs;
    cs.insert(x - y >= 0);
    bd.add_constraints_and_minimize(cs);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test07() {
  Variable y(1);

  TBD_Shape bd(1);

  try {
    // This is an invalid use of the function
    // RBD_Shape::add_constraint(c): it is illegal to insert a
    // constraints that contains a variable that is not in the space
    // of the polyhedron.
    bd.add_constraint(y >= 0);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test08() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(1);

  try {
    // This is an invalid use of the function
    // BD_Shape::add_constraints(cs): it is illegal to add a system
    // of constraints that is dimensional incompatible with the
    // polyhedron.
    Constraint_System cs;
    cs.insert(x - y == 0);
    bd.add_constraints(cs);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
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
  DO_TEST(test07);
  DO_TEST(test08);
END_MAIN
