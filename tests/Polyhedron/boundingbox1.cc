/* Test Polyhedron::shrink_bounding_box().
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
#include "BBox.hh"

namespace {

// A non-bounded closed polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
bool
test01() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

  BBox pbox(2);
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(2);
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test01 ph ***");
  nbox.print(nout, "*** test01 nbox ***");
  pbox.print(nout, "*** test01 pbox ***");

  BBox known_box(2);

  known_box.print(nout, "*** test01 known_box ***");

  return nbox == known_box && pbox == known_box;
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

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test02 ph ***");
  nbox.print(nout, "*** test02 nbox ***");
  pbox.print(nout, "*** test02 pbox ***");

  BBox known_nbox(2);
  known_nbox.raise_lower_bound(0, true, 0, 1);
  known_nbox.raise_lower_bound(1, true, 0, 1);

  BBox known_pbox(2);
  known_pbox.raise_lower_bound(1, true, 0, 1);

  known_nbox.print(nout, "*** test02 known_nbox ***");
  known_pbox.print(nout, "*** test02 known_pbox ***");

  return nbox == known_nbox && pbox == known_pbox && nbox <= pbox;
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

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test03 ph ***");
  nbox.print(nout, "*** test03 nbox ***");
  pbox.print(nout, "*** test03 pbox ***");

  BBox known_nbox(2);
  known_nbox.raise_lower_bound(0, true, -2, 3);
  known_nbox.lower_upper_bound(0, true, 4, 1);
  known_nbox.raise_lower_bound(1, true, -10, 1);
  known_nbox.lower_upper_bound(1, true, 12, 3);

  BBox known_pbox(2);
  known_pbox.lower_upper_bound(0, true, 4, 1);
  known_pbox.lower_upper_bound(1, true, 4, 1);

  known_nbox.print(nout, "*** test03 known_nbox ***");
  known_pbox.print(nout, "*** test03 known_pbox ***");

  return nbox == known_nbox && pbox == known_pbox && nbox <= pbox;
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

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test04 ph ***");
  nbox.print(nout, "*** test04 nbox ***");
  pbox.print(nout, "*** test04 pbox ***");

  BBox known_nbox(4);
  known_nbox.raise_lower_bound(1, true, -2, 3);
  known_nbox.lower_upper_bound(1, true, 4, 1);
  known_nbox.raise_lower_bound(2, true, -10, 1);
  known_nbox.lower_upper_bound(2, true, 12, 3);
  known_nbox.raise_lower_bound(3, true, 15, 3);

  BBox known_pbox(4);
  known_pbox.lower_upper_bound(1, true, 4, 1);
  known_pbox.lower_upper_bound(2, true, 4, 1);
  known_pbox.raise_lower_bound(3, true, 5, 1);

  known_nbox.print(nout, "*** test04 known_nbox ***");
  known_pbox.print(nout, "*** test04 known_pbox ***");

  return nbox == known_nbox && pbox == known_pbox && nbox <= pbox;
}

// This is the universal, 2-dimensional closed polyhedron.
bool
test05() {
  C_Polyhedron ph(2);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test05 ph ***");
  nbox.print(nout, "*** test05 nbox ***");
  pbox.print(nout, "*** test05 pbox ***");

  BBox known_box(2);

  known_box.print(nout, "*** test05 known_box ***");

  return nbox == known_box && pbox == known_box;
}

// A zero-dimensional closed polyhedron.
bool
test06() {
  C_Polyhedron ph;

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test06 ph ***");
  nbox.print(nout, "*** test06 nbox ***");
  pbox.print(nout, "*** test06 pbox ***");

  BBox known_box(0);

  known_box.print(nout, "*** test06 known_box ***");

  return nbox == known_box && pbox == known_box;
}

// An empty closed polyhedron in 2D.
bool
test07() {
  C_Polyhedron ph(2, EMPTY);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test07 ph ***");
  nbox.print(nout, "*** test07 nbox ***");
  pbox.print(nout, "*** test07 pbox ***");

  BBox known_box(ph.space_dimension());
  known_box.set_empty();

  known_box.print(nout, "*** test07 known_box ***");

  return nbox == known_box && pbox == known_box;
}

// A bounded polyhedron that is a single point.
bool
test08() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test08 ph ***");
  nbox.print(nout, "*** test08 nbox ***");
  pbox.print(nout, "*** test08 pbox ***");

  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 2, 1);
  known_box.lower_upper_bound(0, true, 2, 1);
  known_box.raise_lower_bound(1, true, 4, 1);
  known_box.lower_upper_bound(1, true, 4, 1);

  known_box.print(nout, "*** test08 known_box ***");

  return nbox == known_box && pbox == known_box;
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

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test09 ph ***");
  nbox.print(nout, "*** test09 nbox ***");
  pbox.print(nout, "*** test09 pbox ***");

  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);

  known_box.print(nout, "*** test09 known_box ***");

  return nbox == known_box && pbox == known_box;
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

  print_constraints(ph, "*** test10 ph ***");

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  nbox.print(nout, "*** test10 nbox ***");
  pbox.print(nout, "*** test10 pbox ***");

  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 1, 1);
  known_box.lower_upper_bound(0, true, 3, 1);
  known_box.raise_lower_bound(1, true, 1, 1);
  known_box.lower_upper_bound(1, true, 3, 1);

  known_box.print(nout, "*** test10 known_box ***");

  return nbox == known_box && pbox == known_box;
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

  print_constraints(ph, "*** test11 ph ***");

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  pbox.print(nout, "*** test11 pbox ***");

  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 3, 1);
  known_box.lower_upper_bound(0, true, 3, 1);
  known_box.raise_lower_bound(1, true, 1, 1);
  known_box.lower_upper_bound(1, true, 1, 1);

  known_box.print(nout, "*** test11 known_box ***");

  return pbox == known_box;
}

// The box is the xy plane.
bool
test12() {
  Bounding_Box box(2);

  C_Polyhedron ph(box, From_Bounding_Box());

  C_Polyhedron known_ph(box.space_dimension());

  print_generators(ph, "*** test12 ph ***");
  print_generators(known_ph, "*** test12 known_ph ***");

  return ph == known_ph;
}

// This box is the closed +ve quadrant.
bool
test13() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 0, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(y >= 0);

  print_generators(ph, "*** test13 ph ***");
  print_generators(known_ph, "*** test13 known_ph ***");

  return ph == known_ph;
}

// A bounded box in 2D.
bool
test14() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, 4, 1);
  box.raise_lower_bound(1, true, -10, 1);
  box.lower_upper_bound(1, true, 12, 3);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y >= -10);

  print_generators(ph, "*** test14 ph ***");
  print_generators(known_ph, "*** test14 known_ph ***");

  return ph == known_ph;
}

// An unbounded closed box in 4D but bounded in 2D.
bool
test15() {
  Bounding_Box box(4);
  box.raise_lower_bound(1, true, -2, 3);
  box.lower_upper_bound(1, true, 4, 1);
  box.raise_lower_bound(2, true, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);
  box.raise_lower_bound(3, true, 15, 3);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y >= -10);
  known_ph.add_constraint(z >= 5);

  print_generators(ph, "*** test15 ph ***");
  print_generators(known_ph, "*** test15 known_ph ***");

  return ph == known_ph;
}

// A zero-dimensional box.
bool
test16() {
  Bounding_Box box(0);

  C_Polyhedron ph(box, From_Bounding_Box());

  C_Polyhedron known_ph;

  print_generators(ph, "*** test16 ph ***");
  print_generators(known_ph, "*** test16 known_ph ***");

  return ph == known_ph;
}

// An empty closed box in 2D.
bool
test17() {
  Bounding_Box box(2);
  box.set_empty();

  C_Polyhedron ph(box, From_Bounding_Box());

  print_constraints(ph, "*** test17 ph ***");

  C_Polyhedron known_ph(2, EMPTY);

  print_constraints(known_ph, "*** test17 known_ph ***");

  return ph == known_ph;
}

// A single point.
bool
test18() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 2, 1);
  box.lower_upper_bound(0, true, 2, 1);
  box.raise_lower_bound(1, true, 4, 1);
  box.lower_upper_bound(1, true, 4, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(x == 2);
  known_ph.add_constraint(y == 4);

  print_generators(ph, "*** test18 ph ***");
  print_generators(known_ph, "*** test18 known_ph ***");

  return ph == known_ph;
}

// A closed unit square.
bool
test19() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  Constraint_System known_cs;
  known_cs.insert(x >= 0);
  known_cs.insert(x <= 1);
  known_cs.insert(y >= 0);
  known_cs.insert(y <= 1);

  C_Polyhedron known_ph(known_cs);

  print_generators(ph, "*** test19 ph generators ***");
  print_generators(known_ph, "*** test19 known_ph ***");

  return ph == known_ph;
}

// Constructs the polyhedron { x >= 0, x <= 1/2, y >= 0 }
// from the corresponding box.
bool
test20() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 2);
  box.raise_lower_bound(1, true, 0, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  print_generators(ph, "*** test20 ph ***");

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(2*x <= 1);
  known_ph.add_constraint(y >= 0);

  print_generators(known_ph, "*** test20 known_ph ***");

  return ph == known_ph;
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
  DO_TEST(test20);
END_MAIN
