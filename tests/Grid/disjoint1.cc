/* Test Grid::is_disjoint_from().
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

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Grid of points and empty grid.

void
test1() {
  nout << "test1:" << endl;

  Generator_System gs;
  gs.insert(point(A));

  Grid gr1(gs);

  Grid gr2(1, EMPTY);

  if (gr1.is_disjoint_from(gr2))
    return;

  nout << "gr1 intersected gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Empty grid and grid of points.

void
test2() {
  nout << "test2:" << endl;

  Grid gr1(2, EMPTY);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(B));

  Grid gr2(gs);

  if (gr1.is_disjoint_from(gr2))
    return;

  nout << "gr1 intersected gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Both empty.

void
test3() {
  nout << "test3:" << endl;

  Grid gr1(4, EMPTY);

  Grid gr2(4, EMPTY);

  if (gr1.is_disjoint_from(gr2))
    return;

  nout << "gr1 intersected gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Zero dimension universes.

void
test4() {
  nout << "test4:" << endl;

  Grid gr1(0);

  Grid gr2(0);

  if (gr1.is_disjoint_from(gr2)) {
    nout << "gr1 should intersect gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }
}

// Grid and itself.

void
test5() {
  nout << "test5:" << endl;

  Grid gr(3);
  gr.add_congruence(A - B %= 0);
  gr.add_congruence(C %= 0);

  if (gr.is_disjoint_from(gr)) {
    nout << "gr should intersect gr." << endl
	 << "gr:" << endl << gr << endl;

    exit(1);
  }
}

// Two grids which alternate AB planes along C.

void
test6() {
  nout << "test6:" << endl;

  Grid gr1(3);
  gr1.add_congruence(A - B %= 0);
  gr1.add_congruence((C %= 0) / 2);

  Grid gr2(3, EMPTY);
  gr2.add_generator(point(C));
  gr2.add_generator(line(A + B));
  gr2.add_generator(point(C + B));
  gr2.add_generator(point(3*C));

  if (gr1.is_disjoint_from(gr2))
    return;

  nout << "gr1 intersected gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// A sequence of points and a plane.

void
test7() {
  nout << "test7:" << endl;

  Grid gr1(3, EMPTY);
  gr1.add_generator(point(A + B + C));
  gr1.add_generator(point(3*A + 3*B + 3*C));

  Grid gr2(3);
  gr2.add_congruence(A - B %= 0);
  gr2.add_congruence(C == 0);

  if (gr1.is_disjoint_from(gr2))
    return;

  nout << "gr1 intersected gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// A line and a plane.

void
test8() {
  nout << "test8:" << endl;

  Grid gr1(3, EMPTY);
  gr1.add_generator(point(A + B + C));
  gr1.add_generator(line(3*A + 3*B + 3*C));

  Grid gr2(3);
  gr2.add_congruence(A - B %= 0);
  gr2.add_congruence(C == 0);

  if (gr1.is_disjoint_from(gr2)) {
    nout << "gr1 should intersect gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }

  return;
}

// CHINA contains example that showed an error in cgs::is_included_in.

void
test9() {
  nout << "test9:" << endl;

  Grid gr1(1, EMPTY);
  gr1.add_generator(point());

  Grid gr2(1, EMPTY);
  gr2.add_generator(point(A));

  // Minimize both grids.
  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  if (gr1.is_disjoint_from(gr2))
    return;

  nout << "gr1 intersected gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "disjoint1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();

  return 0;
}
CATCH
