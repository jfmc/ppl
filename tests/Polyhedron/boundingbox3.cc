/* Test NNC_Polyhedron::shrink_bounding_box().
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
#include "BBox.hh"

namespace {

// An unbounded NNC polyhedron in 4D but bounded in 2D
// with strict inequality and closure points at the lower bound.
bool
test01() {
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x + y > 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test01 ph ***");
  nbox.print(nout, "*** test01 nbox ***");
  pbox.print(nout, "*** test01 pbox ***");

  BBox known_nbox(4);
  known_nbox.raise_lower_bound(1, false, -2, 3);
  known_nbox.lower_upper_bound(1, true, 4, 1);
  known_nbox.raise_lower_bound(2, false, -10, 1);
  known_nbox.lower_upper_bound(2, true, 4, 1);
  known_nbox.raise_lower_bound(3, true, 5, 1);

  BBox known_pbox(4);
  known_pbox.lower_upper_bound(1, true, 4, 1);
  known_pbox.lower_upper_bound(2, true, 4, 1);
  known_pbox.raise_lower_bound(3, true, 5, 1);

  known_nbox.print(nout, "*** test9 known_nbox ***");
  known_pbox.print(nout, "*** test9 known_pbox ***");

  return nbox == known_nbox && pbox == known_pbox && nbox <= pbox;
}

// A bounded NNC polyhedron with strict inequalities
// causing upper and lower bounds of the box to be open.
bool
test02() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x < 4);
  ph.add_constraint(y <= 4);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  print_constraints(ph, "*** test02 ph ***");
  nbox.print(nout, "*** test02 nbox ***");
  pbox.print(nout, "*** test02 pbox ***");

  BBox known_nbox(2);
  known_nbox.raise_lower_bound(0, true, -2, 3);
  known_nbox.lower_upper_bound(0, false, 4, 1);
  known_nbox.raise_lower_bound(1, false, -10, 1);
  known_nbox.lower_upper_bound(1, true, 4, 1);

  BBox known_pbox(2);
  known_pbox.lower_upper_bound(0, false, 4, 1);
  known_pbox.lower_upper_bound(1, true, 4, 1);

  known_nbox.print(nout, "*** test02 known_nbox ***");
  known_pbox.print(nout, "*** test02 known_pbox ***");

  return nbox == known_nbox && pbox == known_pbox && nbox <= pbox;
}

// An empty polyhedron in 2D defined using strict constraints.
bool
test03() {
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x > 0);
  ph.add_constraint(x < 0);
  ph.add_constraint(y > 0);
  ph.add_constraint(y < 0);

  Rational_Box pbox(2);
  ph.shrink_bounding_box(pbox, POLYNOMIAL_COMPLEXITY);

  Rational_Box nbox(2);
  ph.shrink_bounding_box(nbox);

  NNC_Polyhedron known_ph(2, EMPTY);
  NNC_Polyhedron known_pph(pbox, From_Bounding_Box());
  NNC_Polyhedron known_nph(nbox, From_Bounding_Box());

  print_generators(ph, "*** test03 ph ***");
  print_generators(known_pph, "*** test03 known_pph ***");
  print_generators(known_nph, "*** test03 known_nph ***");

  return ph == known_ph && ph == known_nph && ph == known_ph;
}

// An unbounded box in 4D but bounded in 2D with strict inequalities.
bool
test04() {
  Rational_Box box(4);
  box.raise_lower_bound(1, false, -2, 3);
  box.lower_upper_bound(1, true, 4, 1);
  box.raise_lower_bound(2, false, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);
  box.raise_lower_bound(3, true, 15, 3);

  NNC_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x > -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y > -10);
  known_ph.add_constraint(z >= 5);

  print_generators(ph, "*** test04 ph ***");
  print_generators(known_ph, "*** test04 known_ph ***");

  return ph == known_ph;
}

// A bounded NNC polyhedron with strict inequalities
// causing upper and lower bounds of the box to be open.
bool
test05() {
  Rational_Box box(4);
  box.raise_lower_bound(1, true, -2, 3);
  box.lower_upper_bound(1, false, 4, 1);
  box.raise_lower_bound(2, false, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);

  NNC_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);

  NNC_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x < 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y > -10);

  print_generators(ph, "*** test05 ph ***");
  print_generators(known_ph, "*** test05 known_ph ***");

  return ph == known_ph;
}

// An empty box in 2D.
bool
test06() {
  Rational_Box box(2);
  box.set_empty();

  NNC_Polyhedron ph(box, From_Bounding_Box());

  print_constraints(ph, "*** test06 ph ***");

  NNC_Polyhedron known_ph(2, EMPTY);

  print_constraints(known_ph, "*** test06 known_ph ***");

  return ph == known_ph;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST_F8A(test05);
  DO_TEST(test06);
END_MAIN
