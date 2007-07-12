/* Test adding constraints to a grid.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

// add_congruences_and_minimize(cs)
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  gr.add_congruences_and_minimize(cs);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cs) ***");

  return ok;
}

// add_constraints
bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_constraints(cs);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_constraints(cs) ***");

  return ok;
}

// add_constraints, resulting grid empty.
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_constraints(cs);

  Grid known_gr(3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_constraints(cs) ***");

  return ok;
}

// add_congruences(cs)
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(B > 0);
  cs.insert(A == 0);
  cs.insert(C > 0);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_congruences(cs);

  Grid known_gr(3);
  known_gr.add_congruence(A == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences(cs) ***");

  return ok;
}

// add_recycled_congruences(cs)
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(2*B == 3);
  cs.insert(A == 0);
  cs.insert(C > 0);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_recycled_congruences(cs);

  Grid known_gr(3);
  known_gr.add_congruence(A == 0);
  known_gr.add_congruence(2*B == 3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_recycled_congruences(cs) ***");

  return ok;
}

// add_recycled_congruences_and_minimize(cs)
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(2*B >= 3);
  cs.insert(2*A == 7);
  cs.insert(C > 0);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_recycled_congruences_and_minimize(cs);

  Grid known_gr(3);
  known_gr.add_congruence(2*A == 7);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr.add_recycled_congruences_and_minimize(cs) ***");

  return ok;
}

// add_constraints_and_minimize(cs)

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Constraint_System cs;
  cs.insert(2*B >= 3);
  cs.insert(D == 0);
  cs.insert(2*A == C);

  Grid gr(4);

  print_congruences(gr, "*** gr ***");

  gr.add_constraints_and_minimize(cs);

  Grid known_gr(4);
  known_gr.add_congruence(2*A == C);
  known_gr.add_congruence(D == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_constraints_and_minimize(cs) ***");

  return ok;
}

// add_recycled_constraints
bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Constraint_System cs;
  cs.insert(2*B > 2);
  cs.insert(2*D == 0);

  Grid gr(4);

  print_congruences(gr, "*** gr ***");

  gr.add_recycled_constraints(cs);

  Grid known_gr(4);
  known_gr.add_congruence(D == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_recycled_constraints(cs) ***");

  return ok;
}

// add_recycled_constraints_and_minimize
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Constraint_System cs;
  cs.insert(2*B > 6);
  cs.insert(2*C == 6*D);

  Grid gr(4);

  print_congruences(gr, "*** gr ***");

  gr.add_recycled_constraints_and_minimize(cs);

  Grid known_gr(4);
  known_gr.add_congruence(C == 3*D);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr.add_recycled_constraints_and_minimize(cs) ***");

  return ok;
}

// add_recycled_congruences(cs) -- space dimension exception
bool
test10() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_congruences(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_congruences(cs) -- space dimension exception
bool
test11() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_congruences(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_recycled_congruences_and_minimize(cs) -- space dimension
// exception
bool
test12() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_congruences_and_minimize(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_congruences_and_minimize(cs) -- space dimension exception
bool
test13() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_congruences_and_minimize(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_constraints(cs) -- space dimension exception
bool
test14() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_constraints(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_constraints(cs) -- space dimension exception
bool
test15() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_constraints(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_recycled_constraints(cs) -- space dimension exception
bool
test16() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_constraints(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_recycled_constraints_and_minimize(cs) -- space dimension
// exception
bool
test17() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_constraints_and_minimize(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_constraints_and_minimize(cs) -- space dimension exception
bool
test18() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_constraints_and_minimize(cs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// Construct a congruence system from a constraint system
bool
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Congruence_System cgs(cs);

  Grid gr(3);
  Grid gr1(3);

  gr.add_constraints(cs);
  gr1.add_congruences(cgs);
  print_congruences(gr, "*** gr.add_constraints(cs) ***");
  print_congruences(gr1, "*** gr1.add_congruences(cgs) ***");
  bool ok = (gr1 == gr);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  ok &= (gr == known_gr);

  print_congruences(gr, "*** gr.add_constraints(cs) ***");

  return ok;
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
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST(test18);
  DO_TEST(test19);
END_MAIN
