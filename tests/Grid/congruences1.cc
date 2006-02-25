/* Test Grid::congruences().
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

namespace {

// Empty grid.

bool
test01() {
  Grid gr1(7, EMPTY);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.congruences();

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// Universe grid.

bool
test02() {
  Grid gr1(7);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.congruences();

  print_congruences(cgs, "*** cgs ***");

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// Zero dimension empty grid.

bool
test03() {
  Grid gr1(0, EMPTY);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.congruences();

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// Zero dimension universe grid.

bool
test04() {
  Grid gr1(0);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.congruences();

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// Skew grid in 3D.

bool
test05() {
  Variable A(0);
  Variable B(1);

  Grid gr1(3);
  gr1.add_congruence((A + B %= 3) / 7);
  gr1.add_congruence((A %= 0) / 5);

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.congruences();

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// 3D rectilinear grid defined by generators.

bool
test06() {
  Variable A(0);
  Variable B(1);

  Grid gr1(3);
  gr1.add_generator(grid_point(10*B));
  gr1.add_generator(grid_point(10*A + 10*B));

  Grid known_gr = gr1;

  Congruence_System cgs = gr1.congruences();

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// Get a reference to the congruences, empty the grid, use the
// reference to create a new grid.

bool
test07() {
  Grid gr1(3);
  gr1.add_congruence(Congruence::zero_dim_integrality());

  const Congruence_System& cgs = gr1.congruences();

  // Empty the grid.  The idea is to check that `cgs' still refers to
  // a congruence system that matches the grid.
  gr1.add_congruence(Congruence::zero_dim_false());

  Grid known_gr = gr1;

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

// In zero dimensions get a reference to the universe congruences,
// empty the grid, use the reference to create a new grid.

bool
test08() {
  Grid gr1(0);
  gr1.add_congruence(Congruence::zero_dim_integrality());

  const Congruence_System& cgs = gr1.congruences();

  // Empty the grid.  The idea is to check that `cgs' still refers to
  // a congruence system that matches the grid.
  gr1.add_congruence_and_minimize(Congruence::zero_dim_false());

  Grid known_gr = gr1;

  Grid gr2(cgs);

  bool ok = (gr2 == known_gr);

  print_congruences(cgs, "*** cgs ***");
  print_congruences(gr2, "*** gr2(cgs) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
END_MAIN
