/* Test Grid::Grid(Box&, From_Bounding_Box).
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

// Universe box.
bool
test01() {
  Bounding_Box box(2);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(2);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box() ***");

  return ok;
}

// A 2D box which is a line parallel to the x axis.
bool
test02() {
  Variable B(1);

  Bounding_Box box(2);
  box.raise_lower_bound(1, true, 2, 3);
  box.lower_upper_bound(1, true, 2, 3);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(2);
  known_gr.add_congruence(3*B == 2);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box() ***");

  return ok;
}

// A 2D box that is a point, with divisors.
bool
test03() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(2);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, -2, 3);
  box.raise_lower_bound(1, true, -10, 1);
  box.lower_upper_bound(1, true, -10, 1);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-2*A - 30*B, 3));

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box() ***");

  return ok;
}

// A 3D box which is a 2D plane.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Bounding_Box box(3);
  box.raise_lower_bound(2, true, 15, 5);
  box.lower_upper_bound(2, true, 15, 5);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(3*C));
  known_gr.add_generator(grid_line(A));
  known_gr.add_generator(grid_line(B));

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box() ***");

  return ok;
}

// Zero-dimensional box.
bool
test05() {
  Bounding_Box box(0);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr;

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box()) ***");

  return ok;
}

// Empty box in 2D.
bool
test06() {
  Bounding_Box box(2);
  box.set_empty();

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(2, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box()) ***");

  return ok;
}

// A 4D box containing a single 3D space.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Bounding_Box box(4);
  box.raise_lower_bound(3, true, 4, 1);
  box.lower_upper_bound(3, true, 4, 1);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(4);
  known_gr.add_constraint(D == 4);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box()) ***");

  return ok;
}

// Unit square.
bool
test08() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 1);

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// Simple box with divisor and an interval bounded only from below.
bool
test09() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 2);

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// Box with a dimension bounded only from above.
bool
test10() {
  Bounding_Box box(2);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 2);

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// An otherwise valid box having a dimension with an open bound, where
// the open bound makes the box empty.
bool
test11() {
  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 3, 7);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, false, 1, 2);
  box.lower_upper_bound(1, true, 1, 2);

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// Zero-dimensional empty box.
bool
test12() {
  Bounding_Box box(0);
  box.set_empty();

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(0, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr,
		    "*** gr(box, From_Bounding_Box()) ***");

  return ok;
}

// A box from a higher dimension.
bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);

  Bounding_Box box(6);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, -2, 3);
  box.raise_lower_bound(1, true, -11, 4);
  box.lower_upper_bound(1, true, -11, 4);
  box.lower_upper_bound(3, true, 18, 3);
  box.raise_lower_bound(3, true, 18, 3);
  box.raise_lower_bound(4, true, 15, 7);
  box.lower_upper_bound(4, true, 15, 7);
  box.raise_lower_bound(5, true, -15, 7);
  box.lower_upper_bound(5, true, -15, 7);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(6, EMPTY);
  known_gr.add_generator(grid_point(-56*A - 231*B + 504*D + 180*E - 180*F, 84));
  known_gr.add_generator(grid_line(C));

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr(box, From_Bounding_Box()) ***");

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
END_MAIN
