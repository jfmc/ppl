/* Test Grid::minimized_congruences().
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

// Empty grid.

void
test1() {
  Grid gr1(4, EMPTY);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.minimized_congruences();

  Grid gr2(cgs);

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
  Grid gr1(6);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.minimized_congruences();

  Grid gr2(cgs);

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

  Congruence_System cgs = gr1.minimized_congruences();

  Grid gr2(cgs);

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

  Congruence_System cgs = gr1.minimized_congruences();

  Grid gr2(cgs);

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
  Grid gr1(3);
  gr1.add_congruence((A - B %= 3) / 7);
  gr1.add_congruence((A - B %= 3) / 14);
  gr1.add_congruence((A %= 0) / 5);
  gr1.add_congruence((A %= 0) / 10);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.minimized_congruences();

  Grid gr2(cgs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Get a reference to the minimized_congruences, empty the grid, use the
// reference to create a new grid.

void
test6() {
  Grid gr1(3);
  gr1.add_congruence(Congruence::zero_dim_integrality());

  const Congruence_System& cgs = gr1.minimized_congruences();

  // Empty the grid.  The idea is to check that `cgs' still refers to
  // a congruence system that matches the grid.
  gr1.add_congruence(Congruence::zero_dim_false());

  Grid known_gr = gr1;

  Grid gr2(cgs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// In zero dimensions get a reference to the universe minimized_congruences,
// empty the grid, use the reference to create a new grid.

void
test7() {
  Grid gr1(0);
  gr1.add_congruence(Congruence::zero_dim_integrality());

  const Congruence_System& cgs = gr1.minimized_congruences();

  // Empty the grid.  The idea is to check that `cgs' still refers to
  // a congruence system that matches the grid.
  gr1.add_congruence(Congruence::zero_dim_false());

  Grid known_gr = gr1;

  Grid gr2(cgs);

  if (known_gr == gr2)
    return;

  nout << "Reproduced grid should equal known grid." << endl
       << "grid:" << endl << gr2 << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Empty grid, where minimizing the congruences finds the grid empty.

void
test8() {
  Grid gr1(4);
  gr1.add_congruence(A == 1);
  gr1.add_congruence(A == 0);

  Grid known_gr(4, EMPTY);

  Congruence_System cgs = gr1.minimized_congruences();

  Grid gr2(cgs);

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

  nout << "mincongruences1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);

  return 0;
}
CATCH
