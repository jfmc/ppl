/* Test Grid::intersection_assign().
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Simple grids, one dimensions.

void
test1() {
  Congruence_System cgs1;
  cgs1.insert((A %= 1) / 2);

  Congruence_System cgs2;
  cgs2.insert((A %= 0) / 3);

  Grid gr1(cgs1);
  Grid gr2(cgs2);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 3) / 6);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Simple grids, many dimensions.

void
test2() {
  Congruence_System cgs1;
  cgs1.insert((A + 0*C %= 0) / 2);

  Congruence_System cgs2;
  cgs2.insert((B + 0*C %= 0) / 2);

  Grid gr1(cgs1);
  Grid gr2(cgs2);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A + 0*C %= 0) / 2);
  known_cgs.insert((B %= 0) / 2);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid empty.

void
test3() {
  Congruence_System cgs;
  cgs.insert((B + 0*C %= 0) / 2);

  Grid gr1(3, EMPTY);
  Grid gr2(cgs);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3, EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Second grid empty.

void
test4() {
  Congruence_System cgs;
  cgs.insert((B + 0*C %= 0) / 2);

  Grid gr1(cgs);
  Grid gr2(3, EMPTY);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3, EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid universe.

void
test5() {
  Congruence_System cgs;
  cgs.insert((5*A + 3*B + C %= 7) / 9);

  Grid gr1(3, UNIVERSE);
  Grid gr2(cgs);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((5*A + 3*B + C %= 7) / 9);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Second grid universe.

void
test6() {
  Congruence_System cgs;
  cgs.insert((2*B + 2*C %= 1) / 3);

  Grid gr1(cgs);
  Grid gr2(3, UNIVERSE);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((2*B + 2*C %= 1) / 3);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Zero dimension grids.

void
test7() {
  Congruence_System cgs;
  Linear_Expression le;
  cgs.insert((le %= le) / 1);	// (0 %= 0) / 1

  Grid gr1(cgs);
  Grid gr2(0, UNIVERSE);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Many dimensioned grids from generators.

void
test8() {
  Grid_Generator_System gs1;
  gs1.insert(grid_point(A + C));
  gs1.insert(grid_line(C));

  Grid_Generator_System gs2;
  gs2.insert(grid_point(A));
  gs2.insert(grid_point(A + B));

  Grid gr1(gs1);

  Grid gr2(3, EMPTY);
  gr2.add_generators(gs2);

  gr1.intersection_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((C == 0) / 0);
  known_cgs.insert((A == 1) / 0);
  known_cgs.insert((B == 0) / 0);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Space dimension exception.

void
test9() {
  Grid gr1(1, EMPTY);
  gr1.add_generator(grid_point());

  Grid gr2(3);

  try {
    gr1.intersection_assign(gr2);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "intersection1:" << endl;

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
