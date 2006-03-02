/* Test Grid::generators().
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

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Universe grid.
bool
test02() {
  Grid gr1(7);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Zero dimension empty grid.
bool
test03() {
  Grid gr1(0, EMPTY);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Zero dimension universe grid.
bool
test04() {
  Grid gr1(0);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Skew grid in 3D.
bool
test05() {
  Variable A(0);
  Variable C(2);
  Variable B(1);

  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*B));
  gr1.add_generator(grid_point(5*A + 2*B));
  gr1.add_generator(grid_point(7*B));
  gr1.add_generator(grid_line(C));

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// 3D rectilinear grid defined by congruences.
bool
test06() {
  Variable A(0);
  Variable B(1);

  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 10);
  gr1.add_congruence((B %= 10) / 0);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Get a reference to the empty generators, add a point, use the
// reference to create a new grid.
bool
test07() {
  Grid gr1(3, EMPTY);

  const Grid_Generator_System& gs = gr1.generators();

  // Add a point.  The idea is to check that `gs' still refers to a
  // generator system that matches the grid.
  gr1.add_generator(grid_point());

  Grid known_gr = gr1;

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// In zero dimensions get a reference to the universe generators,
// empty the grid, and then use the reference to create a new grid.
bool
test08() {
  Grid gr1(0);

  const Grid_Generator_System& gs = gr1.generators();

  // Empty the grid.  The idea is to check that `gs' still refers to a
  // generator system that matches the grid.
  gr1.add_congruence_and_minimize(Congruence::zero_dim_false());

  Grid known_gr = gr1;

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Empty grid, where updating the generators finds the grid empty.
bool
test09() {
  Variable A(0);

  Grid gr1(7);
  gr1.add_congruence(A == 1);
  gr1.add_congruence(A == 0);

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Skew grid in 3D defined with generators with a non-integral parameter.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*B, 2));
  gr1.add_generator(grid_point(5*A + 2*B));
  gr1.add_generator(parameter(11*B, 2));
  gr1.add_generator(grid_line(C));

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Grid defined with an integral point but non-integral parameter.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*B));
  gr1.add_generator(grid_point(5*A + 2*B));
  gr1.add_generator(parameter(11*B, 2));
  gr1.add_generator(grid_line(C));

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

  return ok;
}

// Grid defined with an integral point and parameter.
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*B));
  gr1.add_generator(grid_point(5*A + 2*B));
  gr1.add_generator(parameter(11*B));
  gr1.add_generator(grid_line(C));

  Grid known_gr = gr1;

  Grid_Generator_System gs = gr1.generators();

  Grid gr2(gs);

  bool ok = (gr2 == known_gr);

  print_generators(gr2, "*** gr2 ***");

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
  NEW_TEST(test09);
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
END_MAIN
