/* Test Grid::join_assign_if_exact()
     (a.k.a. Grid::upper_bound_assign_if_exact()).
   Copyright (C) 2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

// Two empty grids.

void
test1() {
  nout << "test1:" << endl;

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
  nout << "test2:" << endl;

  Grid gr1(4, EMPTY);

  Grid gr2(4, EMPTY);
  gr2.add_generator(point(2*A));

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
  nout << "test3:" << endl;

  Grid gr1(4, EMPTY);
  gr1.add_generator(point());
  gr1.add_generator(line(B));

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
  nout << "test4:" << endl;

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
  nout << "test5:" << endl;

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
  nout << "test6:" << endl;

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
  nout << "test7:" << endl;

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
  nout << "test8:" << endl;

  Generator_System gs1;
  gs1.insert(point(A));

  Generator_System gs2;
  gs2.insert(point(3*A, 5));

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

} // namespace

int
main() TRY {
  set_handlers();

  nout << "join3:" << endl;

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
