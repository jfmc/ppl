/* Test Grid::get_covering_box.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

#define SPACE_DIM 2

// Rectilinear grid.

void
test1() {
  nout << "test1:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point(B));
  gr.add_generator(point(3*A + B));
  gr.add_generator(point(3*A + 3*B));

  if (find_variation(gr))
    exit(1);

  gr.get_covering_box(box);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 3, 1);
  known_box.raise_lower_bound(1, false, 1, 1);
  known_box.lower_upper_bound(1, false, 3, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// Skew grid.

void
test2() {
  nout << "test2:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point(  A +   B));
  gr.add_generator(point(2*A + 3*B));
  gr.add_generator(point(  A + 4*B));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 1, 1);
  known_box.raise_lower_bound(1, false, -1, 1);
  known_box.lower_upper_bound(1, false, 0, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// Skew grid, with a divisor.

void
test3() {
  nout << "test3:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(2*A));
  gr.add_generator(point(  A + 2*B, 2));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 2);
  known_box.lower_upper_bound(0, false, 1, 2);
  known_box.raise_lower_bound(1, false, 0, 2);
  known_box.lower_upper_bound(1, false, 2, 2);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

#undef SPACE_DIM
#define SPACE_DIM 3

// Grid containing a line.

void
test4() {
  nout << "test4:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point());
  gr.add_generator( line(A + 2*B));
  gr.add_generator(point(C, 2));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 0, 1);
  known_box.raise_lower_bound(1, false, 0, 1);
  known_box.lower_upper_bound(1, false, 0, 1);
  known_box.raise_lower_bound(2, false, 0, 1);
  known_box.lower_upper_bound(2, false, 1, 2);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// Universe grid.

void
test5() {
  nout << "test5:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM);

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 0, 1);
  known_box.raise_lower_bound(1, false, 0, 1);
  known_box.lower_upper_bound(1, false, 0, 1);
  known_box.raise_lower_bound(2, false, 0, 1);
  known_box.lower_upper_bound(2, false, 0, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// Single point.

void
test6() {
  nout << "test6:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point(16*A + 6*B - 6*C, 7));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 16, 7);
  known_box.raise_lower_bound(1, false, 6, 7);
  known_box.raise_lower_bound(2, false, -6, 7);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// Empty grid.

void
test7() {
  nout << "test7:" << endl;

  BBox box(SPACE_DIM);
  // Set bounds, to check that get_covering_box clears them.
  box.raise_lower_bound(0, false, 16, 7);
  box.raise_lower_bound(1, false, 6, 7);
  box.raise_lower_bound(2, false, -6, 7);

  Grid gr(SPACE_DIM, EMPTY);

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// A grid which get_covering_box has to minimize.

void
test8() {
  nout << "test8:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(A + B));
  gr.add_generator(point(A));
  gr.add_generator(point(2*A));
  gr.add_generator(point(C));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 1, 1);
  known_box.raise_lower_bound(1, false, 0, 1);
  known_box.lower_upper_bound(1, false, 1, 1);
  known_box.raise_lower_bound(2, false, 0, 1);
  known_box.lower_upper_bound(2, false, 1, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// A grid defined by congruences.

void
test9() {
  nout << "test9:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM);
  gr.add_congruence((A + 2*B %= 0) / 2);
  gr.add_congruence((A %= 0) / 5);

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 5, 1);
  known_box.raise_lower_bound(1, false, 0, 1);
  known_box.lower_upper_bound(1, false, 1, 2);
  known_box.raise_lower_bound(2, false, 0, 1);
  known_box.lower_upper_bound(2, false, 0, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

// Grid where the only line is the final generator.

void
test10() {
  nout << "test10:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(A));
  gr.add_generator(point(B));
  gr.add_generator( line(C));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 1, 1);
  known_box.raise_lower_bound(1, false, 0, 1);
  known_box.lower_upper_bound(1, false, 1, 1);
  known_box.raise_lower_bound(2, false, 0, 1);
  known_box.lower_upper_bound(2, false, 0, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

#undef SPACE_DIM
#define SPACE_DIM 4

// A grid where, for a particular dimension, many coefficients between
// the first and last rows contribute towards the size of the
// resulting interval.

void
test11() {
  nout << "test11:" << endl;

  BBox box(SPACE_DIM);

  Grid gr(SPACE_DIM, EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(A + 2*D));
  gr.add_generator(point(B + 4*D));
  gr.add_generator(point(C + 8*D));
  gr.add_generator(point(16*D));

  gr.get_covering_box(box);

  if (find_variation(gr))
    exit(1);

  BBox known_box(SPACE_DIM);
  known_box.raise_lower_bound(0, false, 0, 1);
  known_box.lower_upper_bound(0, false, 1, 1);
  known_box.raise_lower_bound(1, false, 0, 1);
  known_box.lower_upper_bound(1, false, 1, 1);
  known_box.raise_lower_bound(2, false, 0, 1);
  known_box.lower_upper_bound(2, false, 1, 1);
  known_box.raise_lower_bound(3, false, 0, 1);
  known_box.lower_upper_bound(3, false, 2, 1);

  if (box == known_box)
    return;

  nout << "Box should equal known box." << endl;
  box.print(nout, "  box:");
  known_box.print(nout, "known:");

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "coveringbox1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();
  test11();

  return 0;
}
CATCH
