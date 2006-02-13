/* Test Grid::join_assign_if_exact()
     (a.k.a. Grid::upper_bound_assign_if_exact()).
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

// Two empty grids.

void
test1() {
  Grid gr1(4, EMPTY);
  Grid gr2(4, EMPTY);

  Grid known_gr(4, EMPTY);

  if (gr1.join_assign_if_exact(gr2)) {
    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid empty.

void
test2() {
  Grid gr1(4, EMPTY);

  Grid gr2(4, EMPTY);
  gr2.add_generator(grid_point(2*A));

  Grid known_gr = gr2;

  if (gr1.join_assign_if_exact(gr2)) {
    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Second grid empty.

void
test3() {
  Grid gr1(4, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_line(B));

  Grid gr2(4, EMPTY);

  Grid known_gr = gr1;

  if (gr1.join_assign_if_exact(gr2)) {

    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Zero dimension universes.

void
test4() {
  Grid gr1(0);
  Grid gr2(0);

  Grid known_gr = gr1;

  if (gr1.join_assign_if_exact(gr2)) {

    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First included in second.

void
test5() {
  Grid gr1(1);
  gr1.add_congruence((A %= 0) / 2);

  Grid gr2(1);
  gr2.add_congruence(A %= 0);

  Grid known_gr = gr2;

  if (gr1.join_assign_if_exact(gr2)) {

    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Second included in first.

void
test6() {
  Grid gr1(2);
  gr1.add_congruence(A - B %= 0);

  Grid gr2(2);
  gr2.add_congruence(A - B == 0);

  Grid known_gr = gr1;

  if (gr1.join_assign_if_exact(gr2)) {

    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Exact join.

void
test7() {
  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2(2);
  gr2.add_congruence((A %= 1) / 2);
  gr2.add_congruence((B %= 1) / 2);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 0) / 2);
  known_gr.add_congruence(A %= 0);

  if (gr1.join_assign_if_exact(gr2)) {

    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }
  else
    nout << "join_assign_if_exact should return true." << endl;

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Two points (join adds more points).

void
test8() {
  Grid_Generator_System gs1;
  gs1.insert(grid_point(A));

  Grid_Generator_System gs2;
  gs2.insert(grid_point(3*A, 5));

  Grid gr1(gs1);
  Grid gr2(gs2);

  Grid known_gr = gr1;

  if (gr1.join_assign_if_exact(gr2))
    nout << "join_assign_if_exact should return false." << endl;
  else {
    if (find_variation(gr1))
      exit(1);

    if (gr1 == known_gr)
      return;

    nout << "Grid should equal known grid." << endl;
  }

  nout << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Space dimension exception.

void
test9() {
  Grid_Generator_System gs;
  gs.insert(grid_point(B + 0*C));

  Grid gr1(gs);

  Grid gr2(4);
  gr2.add_congruence(A == 0);
  gr2.add_congruence(B == 0);
  gr2.add_congruence(C == 0);

  try {
    gr1.upper_bound_assign_if_exact(gr2);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "join3:" << endl;

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
