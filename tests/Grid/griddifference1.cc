/* Test Grid::grid_difference_assign() (a.k.a. Grid::difference_assign()).
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

// Simple grids.

void
test1() {
  Grid gr1(1);
  gr1.add_congruence(A %= 0);

  Grid gr2(1);
  gr2.add_congruence((A %= 0) / 2);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1, EMPTY);
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_point(3*A));

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
  Grid gr1;
  Grid gr2;

  gr1.difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(0, EMPTY);

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
  Grid gr1(2);
  gr1.add_congruence((A - B %= 0) / 3);
  gr1.add_congruence((A %= 0) / 2);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence(B %= 0);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);

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

  Grid gr2(2, EMPTY);

  Grid known_gr(gr1);

  gr1.difference_assign(gr2);

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
  Grid gr1(2, EMPTY);

  Grid gr2(2);
  gr2.add_congruence(A + 2*B %= 0);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);

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
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_point(A - 2*C));
  gr1.add_generator(grid_point(3*B));

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(A));
  gr2.add_generator(grid_point(A + 2*C));
  gr2.add_generator(grid_point(3*A));
  gr2.add_generator(grid_point(A + 3*B));

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
  Grid gr1(0);

  Grid gr2(0);

  gr1.grid_difference_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(0, EMPTY);

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
  Grid gr1(2, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_line(A));
  gr1.add_generator(grid_point(B));

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
  Grid gr1(2, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_point(A));
  gr1.add_generator(grid_point(B));

  Grid gr2(2);
  gr2.add_congruence((A - B %= 0) / 2);
  gr2.add_congruence(A %= 0);

  gr1.difference_assign(gr2);

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
  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence(B == 0);
  gr1.add_congruence(C == 0);

  Grid gr2(3);
  gr2.add_congruence((A + C %= 0) / 4);
  gr2.add_congruence((A + B %= 0) / 4);

  gr1.difference_assign(gr2);

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

// Space dimension exception.

void
test12() {
  Grid_Generator_System gs;
  gs.insert(grid_point(B + 0*C));

  Grid gr1(gs);

  Grid gr2(4);
  gr2.add_congruence(A == 0);
  gr2.add_congruence(B == 0);
  gr2.add_congruence(C == 0);

  try {
    gr1.difference_assign(gr2);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "griddifference1:" << endl;

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

  return 0;
}
CATCH
