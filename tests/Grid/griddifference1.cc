/* Test Grid::grid_difference.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Simple grids.

void
test1() {
  nout << "test1:" << endl;

  Grid gr1(1);
  gr1.add_congruence(A %= 0);

  Grid gr2(1);
  gr2.add_congruence((A %= 0) / 2);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1, Grid::EMPTY);
  known_gr.add_generator(point(A));
  known_gr.add_generator(point(3*A));

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Both universe.

void
test2() {
  nout << "test2:" << endl;

  Grid gr1;
  Grid gr2;

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(0, Grid::EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// First contained in second.

void
test3() {
  nout << "test3:" << endl;

  Grid gr1(2);
  gr1.add_congruence((A - B %= 0) / 3);
  gr1.add_congruence((A %= 0) / 2);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence(B %= 0);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Second contains single equality.

void
test4() {
  nout << "test4:" << endl;

  Grid gr1(2);
  gr1.add_congruence((A - B %= 0) / 3);
  gr1.add_congruence((A %= 0) / 2);

  Grid gr2(2);
  gr2.add_congruence(A == 5);

  Grid known_gr(gr1);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// First empty.

void
test5() {
  nout << "test5:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A + 2*B %= 0);

  Grid gr2(2, Grid::EMPTY);

  Grid known_gr(gr1);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Second empty.

void
test6() {
  nout << "test6:" << endl;

  Grid gr1(2, Grid::EMPTY);

  Grid gr2(2);
  gr2.add_congruence(A + 2*B %= 0);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// More complex example, from generators.

void
test7() {
  nout << "test7:" << endl;

  Grid gr1(3, Grid::EMPTY);
  gr1.add_generator(point());
  gr1.add_generator(point(A - 2*C));
  gr1.add_generator(point(3*B));

  Grid gr2(3, Grid::EMPTY);
  gr2.add_generator(point(A));
  gr2.add_generator(point(A + 2*C));
  gr2.add_generator(point(3*A));
  gr2.add_generator(point(A + 3*B));

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(2*A + C == 0);
  known_gr.add_congruence((B %= 0) / 3);
  known_gr.add_congruence((A %= 0) / 2);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Zero dimension grids.

void
test8() {
  nout << "test8:" << endl;

  Grid gr1(0);

  Grid gr2(0);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(0, Grid::EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// The smallest resulting grid is the first grid, even though the
// first grid has more points than the second.

void
test9() {
  nout << "test9:" << endl;

  Grid gr1(2, Grid::EMPTY);
  gr1.add_generator(point());
  gr1.add_generator( line(A));
  gr1.add_generator(point(B));

  Grid gr2(2);
  gr2.add_congruence((B %= 0) / 3);

  Grid known_gr(gr1);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Simpler example where the resulting grid contains points.

void
test10() {
  nout << "test10:" << endl;

  Grid gr1(2, Grid::EMPTY);
  gr1.add_generator(point());
  gr1.add_generator(point(A));
  gr1.add_generator(point(B));

  Grid gr2(2);
  gr2.add_congruence((A - B %= 0) / 2);
  gr2.add_congruence(A %= 0);

  gr1.grid_difference_assign(gr2);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 1) / 2);
  known_gr.add_congruence(A %= 0);

  if (find_variation(gr1))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Where the 2-complements of more than one congruence are added to
// the result.

void
test11() {
  nout << "test11:" << endl;

  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence(B == 0);
  gr1.add_congruence(C == 0);

  Grid gr2(3);
  gr2.add_congruence((A + C %= 0) / 4);
  gr2.add_congruence((A + B %= 0) / 4);

  gr1.grid_difference_assign(gr2);

  Grid known_gr(3);
  known_gr.add_congruence((A %= 2) / 4);
  known_gr.add_congruence(B == 0);
  known_gr.add_congruence(C == 0);

  if (find_variation(gr1))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "griddifference1:" << endl;

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
