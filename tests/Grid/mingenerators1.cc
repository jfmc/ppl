/* Test Grid::minimized_generators().
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

// Empty grid.

void
test1() {
  Grid gr1(17, EMPTY);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Universe grid.

void
test2() {
  Grid gr1(1);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Zero dimension empty grid.

void
test3() {
  Grid gr1(0, EMPTY);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Zero dimension universe grid.

void
test4() {
  Grid gr1(0);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Skew grid in 3D.

void
test5() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*B));
  gr1.add_generator(grid_point(5*A + 2*B));
  gr1.add_generator(grid_point(10*A + B));
  gr1.add_generator(grid_point(7*B));
  gr1.add_generator(grid_point(3*B + 4*C));
  gr1.add_generator(grid_line(C));

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// 3D rectilinear grid defined by congruences.

void
test6() {
  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 10);
  gr1.add_congruence((B %= 10) / 0);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Get a reference to the empty generators, add a point, use the
// reference to create a new grid.

void
test7() {
  Grid gr1(2, EMPTY);

  const Grid_Generator_System& gs = gr1.minimized_generators();

  // Add a point.  The idea is to check that `gs' still refers to a
  // generator system that matches the grid.
  gr1.add_generator(grid_point());

  Grid known_gr = gr1;

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// In zero dimensions get a reference to the universe generators,
// empty the grid, and then use the reference to create a new grid.

void
test8() {
  Grid gr1(0);

  const Grid_Generator_System& gs = gr1.minimized_generators();

  // Empty the grid.  The idea is to check that `gs' still refers to a
  // generator system that matches the grid.
  gr1.add_congruence_and_minimize(Congruence::zero_dim_false());

  Grid known_gr = gr1;

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Empty grid, where updating the generators finds the grid empty.

void
test9() {
  Grid gr1(4);
  gr1.add_congruence(C == 4);
  gr1.add_congruence(C == 0);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.minimized_generators();

  Grid gr2(gs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "mingenerators1:" << endl;

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
