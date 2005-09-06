/* Test Grid::contains.
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

// Grid containing empty grid.

void
test1() {
  nout << "test1:" << endl;

  Generator_System gs;
  gs.insert(point(A));

  Grid gr1(gs);

  Grid gr2(1, EMPTY);

  if (gr1.contains(gr2))
    return;

  nout << "gr1 should contain gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Empty grid containing points.

void
test2() {
  nout << "test2:" << endl;

  Grid gr1(2, EMPTY);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(B));

  Grid gr2(gs);

  if (gr1.contains(gr2)) {
    nout << "gr1 should fail to contain gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }

  return;
}

// Both empty.

void
test3() {
  nout << "test3:" << endl;

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
  nout << "test4:" << endl;

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
  nout << "test5:" << endl;

  Grid gr1(3);
  gr1.add_congruence(A - B %= 0);
  gr1.add_congruence(C %= 0);

  if (gr1.contains(gr1))
    return;

  nout << "gr1 should contain gr1." << endl
       << "gr1:" << endl << gr1 << endl;

  exit(1);
}

// A grid strictly containing another.

void
test6() {
  nout << "test6:" << endl;

  Grid gr1(3);
  gr1.add_congruence(A - B %= 0);
  gr1.add_congruence(C %= 0);

  Grid gr2(3, EMPTY);
  gr2.add_generator(point());
  gr2.add_generator(point(2*A + 2*B + 2*C));

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
  nout << "test7:" << endl;

  Grid gr1(3, EMPTY);
  gr1.add_generator(point());
  gr1.add_generator(point(2*A + 2*B + 2*C));

  Grid gr2(3);
  gr2.add_congruence(A - B %= 0);
  gr2.add_congruence(C %= 0);

  if (gr1.contains(gr2)) {
    nout << "gr1 should fail to contain gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }

  return;
}

// CHINA example that showed an error in cgs::is_included_in.

void
test8() {
  nout << "test8:" << endl;

  Grid gr1(1, EMPTY);
  gr1.add_generator(point());

  Grid gr2(1, EMPTY);
  gr2.add_generator(point(A));

  // Minimize both grids.
  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  if (gr1.contains(gr2)) {
    nout << "gr1 should fail to contain gr2." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;

    exit(1);
  }

  return;
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "contains1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();

  return 0;
}
CATCH
