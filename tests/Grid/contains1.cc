/* Test Grid::contains().
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

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Grid of points containing empty grid.

void
test1() {
  Grid_Generator_System gs;
  gs.insert(grid_point(A));

  Grid gr1(gs);

  Grid gr2(1, EMPTY);

  if (gr1.contains(gr2))
    return;

  nout << "gr1 should contain gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Empty grid and grid of points.

void
test2() {
  Grid gr1(2, EMPTY);

  Grid_Generator_System gs;
  gs.insert(grid_point());
  gs.insert(grid_point(B));

  Grid gr2(gs);

  if (gr1.contains(gr2)) {
    nout << "gr1 contained gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }
}

// Both empty.

void
test3() {
  Grid gr1(4, EMPTY);

  Grid gr2(4, EMPTY);

  if (gr1.contains(gr2))
    return;

  nout << "gr1 should contain gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Zero dimension universes.

void
test4() {
  Grid gr1(0);

  Grid gr2(0);

  if (gr1.contains(gr2))
    return;

  nout << "gr1 should contain gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Self containment.

void
test5() {
  Grid gr(3);
  gr.add_congruence(A - B %= 0);
  gr.add_congruence(C %= 0);

  if (gr.contains(gr))
    return;

  nout << "gr should contain gr." << endl
       << "gr:" << endl << gr << endl;

  exit(1);
}

// A grid strictly containing another.

void
test6() {
  Grid gr1(3);
  gr1.add_congruence(A - B %= 0);
  gr1.add_congruence(C %= 0);

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point());
  gr2.add_generator(grid_point(2*A + 2*B + 2*C));

  if (gr1.contains(gr2))
    return;

  nout << "gr1 should contain gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// test6, the other way round.

void
test7() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_point(2*A + 2*B + 2*C));

  Grid gr2(3);
  gr2.add_congruence(A - B %= 0);
  gr2.add_congruence(C %= 0);

  if (gr1.contains(gr2)) {
    nout << "gr1 contained gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }
}

// CHINA example that showed an error in cgs::is_included_in.

void
test8() {
  Grid gr1(1, EMPTY);
  gr1.add_generator(grid_point());

  Grid gr2(1, EMPTY);
  gr2.add_generator(grid_point(A));

  // Minimize both grids.
  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  if (gr1.contains(gr2)) {
    nout << "gr1 contained gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }
}

// Space dimension exception

void
test9() {
  Grid gr1(1, EMPTY);
  gr1.add_generator(grid_point());

  Grid gr2(21, EMPTY);

  try {
    gr1.contains(gr2);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "contains1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);

  return 0;
}
CATCH
