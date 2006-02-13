/* Test class Grid_Certificate.
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

// Compare a grid to one that is more constrained (due to equalities).

void
test1() {
  Grid gr1(3);
  gr1.add_congruence(A + C %= 0);
  gr1.add_congruence(B == 3);

  Grid_Certificate grc1(gr1);

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(3*B + A));
  gr2.add_generator(grid_point(3*B + A + C));

  Grid_Certificate grc2(gr2);

  if (grc1.compare(grc2) == -1)
    return;

  nout << "gr1 should compare less than gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Compare a grid to one that is more constrained (due to proper
// congruences).

void
test2() {
  Grid gr1(3);
  gr1.add_congruence(A + C %= 0);
  gr1.add_congruence(B == 3);

  Grid_Certificate grc1(gr1);

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(3*B + A));
  gr2.add_generator(grid_point(3*B + A + C));

  if (grc1.compare(gr2) == -1)
    return;

  nout << "gr1 should compare less than gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Compare a grid to an equally constrained one.

void
test3() {
  Grid gr1(3);
  gr1.add_congruence(A + C %= 0);
  gr1.add_congruence(B == 3);

  Grid_Certificate grc1(gr1);

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(3*B));
  gr2.add_generator(grid_line(A - C));
  gr2.add_generator(grid_point(3*B + A));

  Grid_Certificate grc2(gr2);

  if (grc1.compare(grc2) == 0)
    if (grc1.is_stabilizing(gr2))
      nout << "gr1 is stabilizing with respect to gr2." << endl;
    else
      return;
  else
    nout << "gr1 should compare equal to gr2." << endl;

  nout << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Compare a grid to one that is less constrained (due to equalities).

void
test4() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*B + A));
  gr1.add_generator(grid_point(3*B + A + C));

  Grid_Certificate grc1(gr1);

  Grid gr2(3);
  gr2.add_congruence(A + C %= 0);
  gr2.add_congruence(B == 3);

  if (grc1.compare(gr2) == 1)
    if (grc1.is_stabilizing(gr2))
      return;
    else
      nout << "gr1 should be stabilising with respect to gr2." << endl;
  else
    nout << "gr1 should compare greater than gr2." << endl;

  nout << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Compare a grid to one that is less constrained (due to proper
// congruences).

void
test5() {
  Grid gr1(3);
  gr1.add_congruence((A + C %= 0) / 2);
  gr1.add_congruence((B %= 0) / 3);
  gr1.add_congruence(A %= 0);

  Grid_Certificate grc1(gr1);

  Grid gr2(3);
  gr2.add_congruence((A + C %= 0) / 2);
  gr2.add_congruence((B %= 0) / 3);

  Grid_Certificate grc2(gr2);

  Grid_Certificate::Compare cmp;

  if (cmp(grc1, grc2))
    if (grc1.is_stabilizing(gr2))
      return;
    else
      nout << "gr1 should be stabilising with respect to gr2." << endl;
  else
    nout << "gr1 should compare greater than gr2." << endl;

  nout << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

// Compare certificates for zero dimension universe grids.

void
test6() {
  Grid gr1(0);

  Grid_Certificate grc1(gr1);

  Grid gr2(0);

  Grid_Certificate grc2(gr2);

  if (grc1.compare(grc2) == 0)
    return;

  nout << "gr1 should compare equal to gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  dump_grids(gr1, gr2);

  exit(1);
}

// Compare a grid to one that is more constrained, where the minimized
// generators are used for the comparison.

void
test7() {
  Grid gr1(3);
  gr1.add_congruence(A + C %= 0);
  gr1.add_congruence(B == 3);

  Grid_Certificate grc1(gr1);

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(3*B + A + C));
  gr2.add_generator_and_minimize(grid_point(3*B + A));

  // Ensure up to date congruences and minimized generators.
  gr2.affine_image(A, 1*A);
  gr2.minimized_generators();

  if (grc1.compare(gr2) == -1)
    return;

  nout << "gr1 should compare less than gr2." << endl
       << "gr1:" << endl << gr1 << endl
       << "gr2:" << endl << gr2 << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "certificate1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);

  return 0;
}
CATCH
