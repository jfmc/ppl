/* Test Box::Box(const Polyhedron&, Complexity_Class).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

// A non-bounded closed polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
bool
test01() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(2, UNIVERSE);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// A non-bounded closed polyhedron  in 2D consisting of a wedge bounded
// by y >= 0 and x >= y.
// The resulting bounding box depends on the complexity class.
bool
test02() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_nbox(2);
  known_nbox.add_constraint(x >= 0);
  known_nbox.add_constraint(y >= 0);

  TBox known_pbox(2);
  known_pbox.add_constraint(y >= 0);

  bool ok = (nbox == known_nbox && pbox == known_pbox && pbox.contains(nbox));

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_nbox, "*** known_nbox ***");
  print_constraints(known_pbox, "*** known_pbox ***");

  return ok;
}

// A bounded non-rectangular closed polyhedron in 2D.
bool
test03() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(3*x + y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_nbox(2);
  known_nbox.add_constraint(3*x >= -2);
  known_nbox.add_constraint(x <= 4);
  known_nbox.add_constraint(y >= -10);
  known_nbox.add_constraint(y <= 4);

  TBox known_pbox(2);
  known_pbox.add_constraint(x <= 4);
  known_pbox.add_constraint(y <= 4);

  bool ok = (nbox == known_nbox && pbox == known_pbox && pbox.contains(nbox));

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_nbox, "*** known_nbox ***");
  print_constraints(known_pbox, "*** known_pbox ***");

  return ok;
}

// An unbounded closed polyhedron in 4D but bounded in 2D.
bool
test04() {
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph(4);
  ph.add_constraint(3*x + y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_nbox(4);
  known_nbox.add_constraint(3*x >= -2);
  known_nbox.add_constraint(x <= 4);
  known_nbox.add_constraint(y >= -10);
  known_nbox.add_constraint(3*y <= 12);
  known_nbox.add_constraint(3*z >= 15);

  TBox known_pbox(4);
  known_pbox.add_constraint(x <= 4);
  known_pbox.add_constraint(y <= 4);
  known_pbox.add_constraint(z >= 5);

  bool ok = (nbox == known_nbox && pbox == known_pbox && pbox.contains(nbox));

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_nbox, "*** known_nbox ***");
  print_constraints(known_pbox, "*** known_pbox ***");

  return ok;
}

// This is the universal, 2-dimensional closed polyhedron.
bool
test05() {
  C_Polyhedron ph(2);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(2, UNIVERSE);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// A zero-dimensional closed polyhedron.
bool
test06() {
  C_Polyhedron ph;

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(0);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// An empty closed polyhedron in 2D.
bool
test07() {
  C_Polyhedron ph(2, EMPTY);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(ph.space_dimension(), EMPTY);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// A bounded polyhedron that is a single point.
bool
test08() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(2);
  known_box.add_constraint(x == 2);
  known_box.add_constraint(y == 4);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// A unit square closed polyhedron.
bool
test09() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);

  C_Polyhedron ph(cs);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(2);
  known_box.add_constraint(x >= 0);
  known_box.add_constraint(x <= 1);
  known_box.add_constraint(y >= 0);
  known_box.add_constraint(y <= 1);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// A bounded rectangular closed polyhedron;
bool
test10() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(x >= 1);
  ph.add_constraint(x <= 3);
  ph.add_constraint(y <= 3);
  ph.add_constraint(y >= 1);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox nbox(ph);

  TBox known_box(2);
  known_box.add_constraint(x >= 1);
  known_box.add_constraint(x <= 3);
  known_box.add_constraint(y <= 3);
  known_box.add_constraint(y >= 1);

  bool ok = (nbox == known_box && pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(nbox, "*** nbox ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

// A bounded polyhedron having redundant constraints.
bool
test11() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 3);
  ph.add_constraint(y == 1);
  ph.add_constraint(x + y == 4);
  ph.add_constraint(x - y == 2);
  ph.add_constraint(3*x + y == 10);
  ph.add_constraint(x >= 0);
  ph.add_constraint(y <= 5);
  ph.add_constraint(x + 2*y >= 5);

  TBox pbox(ph, POLYNOMIAL_COMPLEXITY);

  TBox known_box(2);
  known_box.add_constraint(x == 3);
  known_box.add_constraint(y == 1);

  bool ok = (pbox == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(pbox, "*** pbox ***");
  print_constraints(known_box, "*** known_box ***");

  return ok;
}

bool
test12() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + 2*B <= 5);
  cs.insert(A + 2*B >= -10);
  cs.insert(A >= 0);
  cs.insert(B <= 7);
  cs.insert(3*A - 5*B <= 18);
  C_Polyhedron ph(cs);

  TBox box1(ph, SIMPLEX_COMPLEXITY);
  TBox box2(ph, ANY_COMPLEXITY);

  Rational_Box known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(11*A <= 61);
  known_result.add_constraint(2*B <= 5);
  known_result.add_constraint(5*B >= -18);

  bool ok = (box1 == box2
	     && check_result(box1, known_result,
			     "7.50e-7", "4.89e-7", "4.34e-7"));

  print_constraints(box1, "*** box1 ***");
  print_constraints(box2, "*** box2 ***");

  return ok;
}

bool
test13() {
  C_Polyhedron ph(1, EMPTY);

  TBox box(ph);

  TBox known_box(1, EMPTY);

  bool ok (box == known_box);

  print_constraints(ph, "*** ph ***");
  print_constraints(box, "*** box ***");
  print_constraints(known_box, "*** known_box ***");

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
  DO_TEST_F8(test12);
  DO_TEST(test13);
END_MAIN
