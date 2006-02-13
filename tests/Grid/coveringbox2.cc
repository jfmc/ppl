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

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

#define SPACE_DIM 2

bool
operator==(const Bounding_Box& x, const Bounding_Box& y) {
  dimension_type dimension = x.space_dimension();
  if (dimension != y.space_dimension())
    return false;

  if (x.is_empty() && y.is_empty())
    return true;

  if (x.is_empty() || y.is_empty())
    return false;

  TEMP_INTEGER(n_x);
  TEMP_INTEGER(n_y);
  TEMP_INTEGER(d_x);
  TEMP_INTEGER(d_y);

  for (dimension_type i = dimension; i-- > 0; ) {
    bool tem;
    bool x_closed = x.get_lower_bound(i, tem, n_x, d_x);
    bool y_closed = y.get_lower_bound(i, tem, n_y, d_y);
    if (x_closed == y_closed) {
      if (x_closed && (n_x != n_y || d_x != d_y))
	return false;
    }
    else
      return false;
    x_closed = x.get_upper_bound(i, tem, n_x, d_x);
    y_closed = y.get_upper_bound(i, tem, n_y, d_y);
    if (x_closed == y_closed) {
      if (x_closed && (n_x != n_y || d_x != d_y))
	return false;
    }
    else
      return false;
  }

  return true;
}

// Minimized rectilinear grid.

void
test1() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point(B));
  gr.add_generator(grid_point(3*A + B));
  gr.add_generator(grid_point(3*A + 3*B));

  if (find_variation(gr))
    exit(1);

  gr.get_covering_box(box1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 3, 1);
  known_box.raise_lower_bound(1, true, 1, 1);
  known_box.lower_upper_bound(1, true, 3, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Skew grid.

void
test2() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point(  A +   B));
  gr.add_generator(grid_point(2*A + 3*B));
  gr.add_generator(grid_point(  A + 4*B));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Skew grid, with a divisor.

void
test3() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(2*A));
  gr.add_generator(grid_point(  A + 2*B, 2));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 2);
  known_box.lower_upper_bound(0, true, 1, 2);
  known_box.raise_lower_bound(1, true, 0, 2);
  known_box.lower_upper_bound(1, true, 2, 2);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

#undef SPACE_DIM
#define SPACE_DIM 3

// Grid containing a line.

void
test4() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_line(A + 2*B));
  gr.add_generator(grid_point(C, 2));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 2);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Universe grid.

void
test5() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM);

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 0, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Grid which is a single point.

void
test6() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point(16*A + 6*B - 6*C, 7));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 16, 7);
  known_box.raise_lower_bound(1, true, 6, 7);
  known_box.raise_lower_bound(2, true, -6, 7);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Empty grid.

void
test7() {
  Bounding_Box box1(SPACE_DIM);
  // Set bounds, to check that get_covering_box clears them.
  box1.raise_lower_bound(0, true, 16, 7);
  box1.raise_lower_bound(1, true, 6, 7);
  box1.raise_lower_bound(2, true, -6, 7);

  Grid gr(SPACE_DIM, EMPTY);

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.set_empty();

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());

    Bounding_Box box2(SPACE_DIM);
    // Set bounds, to check that get_covering_box clears them.
    box2.raise_lower_bound(0, true, 1, 3);
    box2.raise_lower_bound(1, true, 2, 2);
    box2.raise_lower_bound(2, true, 3, 1);

    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "   box:" << endl << box2
	 << "  box1:" << endl << box1
	 << "tem_gr:" << tem_gr;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// A grid which get_covering_box has to minimize.

void
test8() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A + B));
  gr.add_generator(grid_point(A));
  gr.add_generator(grid_point(2*A));
  gr.add_generator(grid_point(C));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// A grid defined by congruences.

void
test9() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM);
  gr.add_congruence((A + 2*B %= 0) / 2);
  gr.add_congruence((A %= 0) / 5);

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 5, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 2);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 0, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Grid where the only line is the final generator.

void
test10() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator(grid_point(B));
  gr.add_generator(grid_line(C));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 0, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

#undef SPACE_DIM
#define SPACE_DIM 4

// A grid where, for a particular dimension (D), many coefficients
// between the first and last rows contribute towards the size of the
// resulting interval.

void
test11() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A + 2*D));
  gr.add_generator(grid_point(B + 4*D));
  gr.add_generator(grid_point(C + 8*D));
  gr.add_generator(grid_point(16*D));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);
  known_box.lower_upper_bound(3, true, 2, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// A grid where all the points have the same value in one of the
// dimensions (B).

void
test12() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator(grid_point(C));
  gr.add_generator(grid_point(D));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);
  known_box.lower_upper_bound(3, true, 1, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// An empty grid defined by congruences.

void
test13() {
  Bounding_Box box1(SPACE_DIM);
  // Set bounds, to check that get_covering_box clears them.
  box1.raise_lower_bound(0, true, 1, 7);
  box1.raise_lower_bound(1, true, 2, 7);
  box1.raise_lower_bound(2, true, 3, 7);

  Grid gr(SPACE_DIM);
  gr.add_congruence((A %= 0) / 2);
  gr.add_congruence((A %= 1) / 2);

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.set_empty();

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());

    Bounding_Box box2(SPACE_DIM);
    // Set bounds, to check that get_covering_box clears them.
    box2.raise_lower_bound(0, true, 3, 7);
    box2.raise_lower_bound(1, true, 1, 7);
    box2.raise_lower_bound(2, true, 2, 7);

    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Grid which is a single point, with a divisor, such that the
// fractions for some intervals (B and C) will be reduced before being
// assigned to the intervals.

void
test14() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point(16*A + 14*B - 7*C, 7));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 16, 7);
  known_box.raise_lower_bound(1, true, 2, 1);
  known_box.raise_lower_bound(2, true, -1, 1);
  known_box.raise_lower_bound(3, true, 0, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Many-pointed grid, with a divisor, such that the fractions for some
// intervals (B and C) will be reduced before being assigned to the
// intervals.

void
test15() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A, 6));
  gr.add_generator(grid_point(B, 3));
  gr.add_generator(grid_point(C, 2));

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 6);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 3);
  known_box.raise_lower_bound(2, true, 0, 1);
  known_box.lower_upper_bound(2, true, 1, 2);
  known_box.raise_lower_bound(3, true, 0, 1);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

#undef SPACE_DIM
#define SPACE_DIM 0

// Zero dimension empty grid.

void
test16() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);
  known_box.set_empty();

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

// Zero dimension universe grid.

void
test17() {
  Bounding_Box box1(SPACE_DIM);

  Grid gr(SPACE_DIM);

  gr.get_covering_box(box1);

  if (find_variation(gr))
    exit(1);

  Bounding_Box known_box(SPACE_DIM);

  if (box1 == known_box) {
    Grid tem_gr(box1, From_Covering_Box());
    Bounding_Box box2(SPACE_DIM);
    tem_gr.get_covering_box(box2);

    if (box2 == known_box)
      return;

    nout << "Reproduced box should equal known box." << endl
	 << "  box:" << endl << box2;
  }
  else
    nout << "Original box should equal known box." << endl
	 << "  box:" << endl << box1;

  nout << "known:" << endl << known_box;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "coveringbox2:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);

  return 0;
}
CATCH
