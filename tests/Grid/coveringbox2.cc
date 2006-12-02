 /* Test Grid::get_covering_box().
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

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

#define SPACE_DIM 2

// Rectilinear grid defined by points with the origin
// not a point of the grid or a point of the covering box..
bool
test01() {
  Variable A(0);
  Variable B(1);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point(B));
  gr.add_grid_generator(grid_point(3*A + B));
  gr.add_grid_generator(grid_point(3*A + 3*B));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 3, 1);
  known_box.raise_lower_bound(1, true, 1, 1);
  known_box.lower_upper_bound(1, true, 3, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Skew grid.
bool
test02() {
  Variable A(0);
  Variable B(1);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point(  A +   B));
  gr.add_grid_generator(grid_point(2*A + 3*B));
  gr.add_grid_generator(grid_point(  A + 4*B));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Skew grid, with a divisor.
bool
test03() {
  Variable A(0);
  Variable B(1);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(2*A));
  gr.add_grid_generator(grid_point(  A + 2*B, 2));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 2);
  known_box.lower_upper_bound(0, true, 1, 2);
  known_box.raise_lower_bound(1, true, 0, 2);
  known_box.lower_upper_bound(1, true, 2, 2);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

#undef SPACE_DIM
#define SPACE_DIM 3

// Grid containing a line.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_line(A + 2*B));
  gr.add_grid_generator(grid_point(C, 2));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 2);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Universe grid.
bool
test05() {
  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM);

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 0, 1);

  bool ok = (box1 == known_box);

  print_congruences(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Grid which is a single point.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point(16*A + 6*B - 6*C, 7));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 16, 7);
  known_box.raise_lower_bound(1, true, 6, 7);
  known_box.raise_lower_bound(2, true, -6, 7);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Empty grid.
bool
test07() {
  Rational_Box box1(SPACE_DIM);
  // Set bounds, to check that get_covering_box clears them.
  box1.raise_lower_bound(0, true, 16, 7);
  box1.raise_lower_bound(1, true, 6, 7);
  box1.raise_lower_bound(2, true, -6, 7);

  Grid gr(SPACE_DIM, EMPTY);

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.set_empty();

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());

    Rational_Box box2(SPACE_DIM);
    // Set bounds, to check that get_covering_box clears them.
    box2.raise_lower_bound(0, true, 1, 3);
    box2.raise_lower_bound(1, true, 2, 2);
    box2.raise_lower_bound(2, true, 3, 1);

    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// A grid which get_covering_box has to minimize.
bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A + B));
  gr.add_grid_generator(grid_point(A));
  gr.add_grid_generator(grid_point(2*A));
  gr.add_grid_generator(grid_point(C));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// A grid defined by congruences.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM);
  gr.add_congruence((A + 2*B %= 0) / 2);
  gr.add_congruence((A %= 0) / 5);

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 5, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 2);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 0, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Grid where the only line is the final generator.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A));
  gr.add_grid_generator(grid_point(B));
  gr.add_grid_generator(grid_line(C));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 0, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

#undef SPACE_DIM
#define SPACE_DIM 4

// A grid where, for a particular dimension (D), many coefficients
// between the first and last rows contribute towards the size of the
// resulting interval.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A + 2*D));
  gr.add_grid_generator(grid_point(B + 4*D));
  gr.add_grid_generator(grid_point(C + 8*D));
  gr.add_grid_generator(grid_point(16*D));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);
  known_box.lower_upper_bound(3, true, 2, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {

    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// A grid where all the points have the same value in one of the
// dimensions (B).
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A));
  gr.add_grid_generator(grid_point(C));
  gr.add_grid_generator(grid_point(D));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);
  known_box.lower_upper_bound(3, true, 1, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// An empty grid defined by congruences.
bool
test13() {
  Variable A(0);

  Rational_Box box1(SPACE_DIM);
  // Set bounds, to check that get_covering_box clears them.
  box1.raise_lower_bound(0, true, 1, 7);
  box1.raise_lower_bound(1, true, 2, 7);
  box1.raise_lower_bound(2, true, 3, 7);

  Grid gr(SPACE_DIM);
  gr.add_congruence((A %= 0) / 2);
  gr.add_congruence((A %= 1) / 2);

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.set_empty();

  bool ok = (box1 == known_box);

  print_congruences(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());

    Rational_Box box2(SPACE_DIM);
    // Set bounds, to check that get_covering_box clears them.
    box2.raise_lower_bound(0, true, 3, 7);
    box2.raise_lower_bound(1, true, 1, 7);
    box2.raise_lower_bound(2, true, 2, 7);

    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Grid which is a single point, with a divisor, such that the
// fractions for some intervals (B and C) will be reduced before being
// assigned to the intervals.
bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point(16*A + 14*B - 7*C, 7));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 16, 7);
  known_box.raise_lower_bound(1, true, 2, 1);
  known_box.raise_lower_bound(2, true, -1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Many-pointed grid, with a divisor, such that the fractions for some
// intervals (B and C) will be reduced before being assigned to the
// intervals.
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point());
  gr.add_grid_generator(grid_point(A, 6));
  gr.add_grid_generator(grid_point(B, 3));
  gr.add_grid_generator(grid_point(C, 2));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 6);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 3);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 2);
  known_box.raise_lower_bound(3, true, 0, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

#undef SPACE_DIM
#define SPACE_DIM 0

// Zero dimension empty grid.
bool
test16() {
  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.set_empty();

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

// Zero dimension universe grid.
bool
test17() {
  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM);

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);

  bool ok = (box1 == known_box);

  print_congruences(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

#undef SPACE_DIM
#define SPACE_DIM 2

// Rectilinear grid as in test01 but this time
// defined by a point and 2 parameters.
bool
test18() {
  Variable A(0);
  Variable B(1);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point(B));
  gr.add_grid_generator(parameter(3*A));
  gr.add_grid_generator(parameter(3*A + 2*B));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 3, 1);
  known_box.raise_lower_bound(1, true, 1, 1);
  known_box.lower_upper_bound(1, true, 3, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

  return ok;
}

#undef SPACE_DIM
#define SPACE_DIM 4

// 4D grid defined with points and parameters.
bool
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Rational_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_grid_generator(grid_point(A));
  gr.add_grid_generator(grid_point(7*A));
  gr.add_grid_generator(parameter(5*B-3*A));
  gr.add_grid_generator(parameter(3*A));
  gr.add_grid_generator(grid_point(C+A));

  gr.get_covering_box(box1);

  Rational_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 1, 1);
  known_box.lower_upper_bound(0, true, 4, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 5, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);

  bool ok = (box1 == known_box);

  print_generators(gr, "*** gr ***");
  nout << "box1:" << endl << box1;

  if (ok) {
    Grid tem_gr(box1, From_Covering_Box());
    Rational_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    ok = (box2 == known_box);

    nout << "box2:" << endl << box2;
  }

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
