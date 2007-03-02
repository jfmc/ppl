/* Test Box::Box(const Grid&, Complexity_Class).
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

// Minimized rectilinear grid.
bool
test01() {
  Variable A(0);
  Variable B(1);

  Rational_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 3, 1);
  box.raise_lower_bound(1, true, 1, 1);
  box.lower_upper_bound(1, true, 3, 1);

  Rational_Box known_box(box);

  Grid gr(2, EMPTY);
  gr.add_grid_generator(grid_point(B));
  gr.add_grid_generator(grid_point(3*A + B));
  gr.add_grid_generator(grid_point(3*A + 3*B));

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Skew grid.
bool
test02() {
  Variable A(0);
  Variable B(1);

  Rational_Box box(2);
  box.raise_lower_bound(0, true, 3, 2);
  box.lower_upper_bound(0, true, 4, 2);
  box.raise_lower_bound(1, true, 7, 1);
  box.lower_upper_bound(1, true, 8, 1);

  Rational_Box known_box(box);

  Grid gr(2, EMPTY);
  gr.add_grid_generator(grid_point(  A +   B));
  gr.add_grid_generator(grid_point(2*A + 3*B));
  gr.add_grid_generator(grid_point(  A + 4*B));

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Skew grid, with a divisor.
bool
test03() {
  Variable A(0);
  Variable B(1);

  Rational_Box box(2);
  box.raise_lower_bound(0, true, 0, 2);
  box.lower_upper_bound(0, true, 1, 2);
  box.raise_lower_bound(1, true, 0, 2);
  box.lower_upper_bound(1, true, 2, 2);

  Grid gr(2, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(2*A));
  gr.add_grid_generator(grid_point(  A + 2*B, 2));

  Rational_Box known_box(box);

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");
  return ok;
}

// Grid containing a line.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box(3);

  Grid gr(3, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_line(A + 2*B));
  gr.add_grid_generator(grid_point(C, 2));

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(3);

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Universe grid.
bool
test05() {
  Rational_Box box(3);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 0, 1);

  Grid gr(3);

  Rational_Box known_box(box);

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Grid which is a single point.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(3);

  Grid gr(3, EMPTY);
  gr.add_grid_generator(grid_point(16*A + 6*B - 6*C, 7));

  gr.shrink_bounding_box(box1);
  nout << "*** box1 ***" << endl << box1 << endl;

  Rational_Box known_box(3);
  known_box.raise_lower_bound(0, true, 16, 7);
  known_box.lower_upper_bound(0, true, 16, 7);
  known_box.raise_lower_bound(1, true, 6, 7);
  known_box.lower_upper_bound(1, true, 6, 7);
  known_box.raise_lower_bound(2, true, -6, 7);
  known_box.lower_upper_bound(2, true, -6, 7);

  bool ok = (box1 == known_box);
  if (ok) {
    Grid tem_gr(box1, From_Bounding_Box());
    Rational_Box box2(3);

    tem_gr.shrink_bounding_box(box2);
    nout << "*** box2 ***" << endl << box2 << endl;

    ok = (box2 == known_box);
  }

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box2) ***");

  return ok;
}

// Empty grid.
bool
test07() {
  Rational_Box box1(3);

  Grid gr(3, EMPTY);

  gr.shrink_bounding_box(box1);
  nout << "*** box1 ***" << endl << box1 << endl;

  Rational_Box known_box(3);
  known_box.set_empty();

  bool ok = (box1 == known_box);
  if (ok) {
    Grid tem_gr(box1, From_Bounding_Box());
    Rational_Box box2(3);

    tem_gr.shrink_bounding_box(box2);
    nout << "*** box2 ***" << endl << box2 << endl;

    ok = (box2 == known_box);
  }

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box2) ***");

  return ok;
}

// A grid with redundant generators.
bool
test08() {
  Variable A(0);
  Variable B(1);

  Rational_Box box(3);

  Grid gr(3, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A + B));
  gr.add_grid_generator(grid_point(A));
  gr.add_grid_generator(grid_point(2*A));

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(3);
  known_box.lower_upper_bound(2, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// A grid defined by congruences.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box(3);

  Grid gr(3);
  gr.add_congruence((A + 2*C %= 0) / 2);
  gr.add_congruence((A %= 0) / 5);
  gr.add_congruence(2*B == 3);

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(3);
  known_box.raise_lower_bound(1, true, 3, 2);
  known_box.lower_upper_bound(1, true, 3, 2);

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// A box having a dimension with an open bound.
bool
test10() {
  Variable A(0);

  Rational_Box box(2);
  box.raise_lower_bound(0, true, -3, 7);
  box.lower_upper_bound(0, false, 3, 7);
  box.raise_lower_bound(1, false, 1, 3);
  box.lower_upper_bound(1, true, 1, 2);

  Grid gr(2);
  gr.add_congruence((A == 0));

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, false, 1, 3);
  known_box.lower_upper_bound(1, true, 1, 2);

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// An empty grid defined by congruences.
bool
test11() {
  Variable A(0);

  Rational_Box box1(3);

  Grid gr(3);
  gr.add_congruence((A %= 0) / 2);
  gr.add_congruence((A %= 1) / 2);

  gr.shrink_bounding_box(box1);
  nout << "*** box1 ***" << endl << box1 << endl;

  Rational_Box known_box(3);
  known_box.set_empty();

  bool ok = (box1 == known_box);
  if (ok) {
    Grid tem_gr(box1, From_Bounding_Box());
    Rational_Box box2(3);

    tem_gr.shrink_bounding_box(box2);
    nout << "*** box2 ***" << endl << box2 << endl;

    ok = (box2 == known_box);
  }

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box2) ***");

  return ok;
}

// Simple grid where all the points have the same value in one of the
// dimensions (B).
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Rational_Box box1(4);

  Grid gr(4, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A));
  gr.add_grid_generator(grid_point(C));
  gr.add_grid_generator(grid_point(D));

  print_generators(gr, "*** gr ***");

  gr.shrink_bounding_box(box1);
  nout << "*** box1 ***" << endl << box1 << endl;

  Rational_Box known_box(4);
  known_box.lower_upper_bound(1, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);

  bool ok = (box1 == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Simple grid, with a divisor, such that the fractions for some
// intervals (B and C) will be reduced before being assigned to the
// intervals.
bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Rational_Box box(4);

  Grid gr(4, EMPTY);
  gr.add_grid_generator(grid_point(  A + 2*B + 4*C, 4));
  gr.add_grid_generator(grid_point(2*A + 2*B + 4*C, 4));
  gr.add_grid_generator(grid_point(  A + 2*B + 4*C + D, 4));

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(4);
  known_box.lower_upper_bound(1, true, 1, 2);
  known_box.raise_lower_bound(1, true, 1, 2);
  known_box.lower_upper_bound(2, true, 1, 1);
  known_box.raise_lower_bound(2, true, 1, 1);

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Zero dimension empty grid.
bool
test14() {
  Rational_Box box(0);

  Grid gr(0, EMPTY);

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(0);
  known_box.set_empty();

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// Zero dimension universe grid.
bool
test15() {
  Rational_Box box(0);

  Grid gr(0);

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(0);

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// A box having a dimension with an open bound, where
// the open bound makes the box empty.
bool
test16() {
  Rational_Box box(3);
  box.raise_lower_bound(0, true, 3, 7);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, false, 1, 2);
  box.lower_upper_bound(1, true, 1, 2);

  Grid gr(3);

  gr.shrink_bounding_box(box);
  nout << "*** box ***" << endl << box << endl;

  Rational_Box known_box(3);
  known_box.set_empty();

  bool ok = (box == known_box);

  print_congruences(gr,
      "*** gr.shrink_bounding_box(box) ***");

  return ok;
}

// A box having a different number of dimensions to that of the grid.
bool
test17() {
  Rational_Box box(2);
  box.raise_lower_bound(0, true, 3, 7);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, false, 1, 2);
  box.lower_upper_bound(1, true, 1, 2);

  Grid gr(3);

  try {
    gr.shrink_bounding_box(box);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
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
END_MAIN
