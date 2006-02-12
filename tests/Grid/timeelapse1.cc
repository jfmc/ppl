/* Test Grid::time_elapse_assign().
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
Variable D(3);

// Zero dimension.

void
test1() {
  Grid gr1(0);
  Grid gr2(0);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(0);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Zero dimension, second grid empty.

void
test2() {
  Grid gr1(0);
  Grid gr2(0, EMPTY);

  gr1.time_elapse_assign(gr2);

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

// First grid empty.

void
test3() {
  Grid gr1(4, EMPTY);

  Grid gr2(4);
  gr2.add_congruence(A %= 3);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(4, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Second grid empty.

void
test4() {
  Grid gr1(4);
  gr1.add_congruence(A %= 3);

  Grid gr2(4, EMPTY);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(4, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Second grid a single point at the origin.

void
test5() {
  Grid gr1(2);
  gr1.add_congruence(A + 2*B %= 0);

  Grid gr2(2, EMPTY);
  gr2.add_generator(grid_point());

  Grid known_gr(gr1);

  gr1.time_elapse_assign(gr2);

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

// One dimension grids of equalities.

void
test6() {
  Grid gr1(1);
  gr1.add_congruence(A == 2);

  Grid gr2(1);
  gr2.add_congruence(A == 1);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 0);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// One dimension grids with congruences.

void
test7() {
  Grid gr1(1);
  gr1.add_congruence(A == 2);

  Grid gr2(1);
  gr2.add_congruence((A %= 0) / 3);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence((A %= 2) / 3);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Multi-dimension grids.

void
test8() {
  Grid gr1(2);
  gr1.add_congruence((A - B %= 1) / 6);

  Grid gr2(2);
  gr2.add_congruence((A %= 0) / 2);
  gr2.add_congruence(B == 0);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 1) / 2);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Multi-dimension grids with denominators, in timeelapse2.

// Multi-dimension grids from generators in sub-optimal form.

void
test9() {
  Grid gr1(4, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_point(2*A));
  gr1.add_generator(grid_point(4*A));
  gr1.add_generator(grid_point(D));

  Grid gr2(4, EMPTY);
  gr2.add_generator(grid_point(A));

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(4, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_point(D));

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// A grid of a single point, where the position of the second grid
// causes the result to be more dense than the second grid.

void
test10() {
  Grid gr1(1);
  gr1.add_congruence(A == 2);

  Grid gr2(1);
  gr2.add_congruence((A %= 1) / 3);

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 0);

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
test11() {
  Grid gr1(1, EMPTY);
  gr1.add_generator(grid_point());

  Grid gr2(19, EMPTY);

  try {
    gr1.time_elapse_assign(gr2);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "timeelapse1:" << endl;

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

  return 0;
}
CATCH
