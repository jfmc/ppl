/* Test Grid(Box& box, From_Covering_Box()).
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

// This constructor is also tested via coveringbox2.cc.

#include "ppl_test.hh"

namespace {

#define SPACE_DIM 2

// Universe box.
bool
test01() {
  Bounding_Box box(SPACE_DIM);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(SPACE_DIM, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// The box is the positive quadrant.
bool
test02() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(SPACE_DIM);
  box.raise_lower_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 0, 1);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(SPACE_DIM);
  known_gr.add_congruence(A == 0);
  known_gr.add_congruence(B == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// A bounded box in 2D.
bool
test03() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(SPACE_DIM);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, 4, 1);
  box.raise_lower_bound(1, true, -10, 1);
  box.lower_upper_bound(1, true, 12, 3);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(SPACE_DIM);
  known_gr.add_congruence((3*A %= -2) / 14);
  known_gr.add_congruence((B %= -10) / 14);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// A 3D box which is bounded in 2D.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Bounding_Box box(3);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, 4, 1);
  box.raise_lower_bound(1, true, -10, 1);
  box.lower_upper_bound(1, true, 12, 3);
  box.raise_lower_bound(2, true, 15, 3);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(-2*A - 30*B + 15*C, 3));
  known_gr.add_generator(grid_point(4*A - 10*B + 5*C));
  known_gr.add_generator(grid_point(-2*A + 12*B + 15*C, 3));

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// Zero-dimensional box.
bool
test05() {
  Bounding_Box box(0);

  Grid gr(box, From_Covering_Box());

  Grid known_gr;

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// Empty box in 2D.
bool
test06() {
  Bounding_Box box(2);
  box.set_empty();

  Grid gr(box, From_Covering_Box());

  Grid known_gr(2, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// A box which is a point.
bool
test07() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 2, 1);
  box.lower_upper_bound(0, true, 2, 1);
  box.raise_lower_bound(1, true, 4, 1);
  box.lower_upper_bound(1, true, 4, 1);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(2);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// Unit square.
bool
test08() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 1);

  Grid gr(box, From_Covering_Box());

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);
  known_cgs.insert(B %= 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// Simple box with divisor.
bool
test09() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 2);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(2);
  known_gr.add_congruence(A == 0);
  known_gr.add_congruence(2*B %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// Box with a dimension bounded only from above.
bool
test10() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(2);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 2);

  Grid gr(box, From_Covering_Box());

  Grid known_gr(2);
  known_gr.add_congruence(7*A == 3);
  known_gr.add_congruence(2*B %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

  return ok;
}

// Box with a dimension having an open bound, where the open bound
// makes the box empty.
bool
test11() {
  Bounding_Box box(2);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, false, 1, 2);
  box.lower_upper_bound(1, true, 1, 2);

  try {
    Grid gr(box, From_Covering_Box());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Zero-dimensional empty box.
bool
test12() {
  Bounding_Box box(0);
  box.set_empty();

  Grid gr(box, From_Covering_Box());

  Grid known_gr(0, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(box, From_Covering_Box()) ***");

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
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
END_MAIN
