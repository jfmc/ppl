/* Test Grid::intersection_assign_and_minimize().
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

// Simple grids, one dimension.
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs1;
  cgs1.insert((A + 0*C %= 0) / 2);

  Congruence_System cgs2;
  cgs2.insert((2*A + 0*C %= 0) / 2);

  Grid gr1(cgs1);
  print_congruences(gr1, "*** gr1 ***");
  Grid gr2(cgs2);
  print_congruences(gr2, "*** gr2 ***");

  gr1.intersection_assign_and_minimize(gr2);

  Congruence_System known_cgs;
  known_cgs.insert((A + 0*C %= 0) / 2);
  known_cgs.insert((2*A %= 0) / 2);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

// First grid empty.
bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + B + C %= 0) / 2);

  Grid gr1(3, EMPTY);
  print_congruences(gr1, "*** gr1 ***");
  Grid gr2(cgs);

  gr1.intersection_assign_and_minimize(gr2);

  Grid known_gr(3, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

// Second grid empty.
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + B + C %= 0) / 2);

  Grid gr1(cgs);
  print_congruences(gr1, "*** gr1 ***");
  Grid gr2(3, EMPTY);

  gr1.intersection_assign_and_minimize(gr2);

  Grid known_gr(3, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

// First grid universe.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((  A +   B + C %= 7) / 9);
  cgs.insert((5*A + 3*B + C %= 7) / 9);

  Grid gr1(3, UNIVERSE);
  print_congruences(gr1, "*** gr1 ***");
  Grid gr2(cgs);

  gr1.intersection_assign_and_minimize(gr2);

  Congruence_System known_cgs;
  known_cgs.insert((  A +   B + C %= 7) / 9);
  known_cgs.insert((5*A + 3*B + C %= 7) / 9);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

// Second grid universe.
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((2*B + 2*C %= 1) / 3);
  cgs.insert((2*B %= 1) / 3);

  Grid gr1(cgs);
  print_congruences(gr1, "*** gr1 ***");
  Grid gr2(3, UNIVERSE);

  gr1.intersection_assign_and_minimize(gr2);

  Congruence_System known_cgs;
  known_cgs.insert((2*B + 2*C %= 1) / 3);
  known_cgs.insert((2*B %= 1) / 3);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

// Zero dimension grids.
bool
test06() {
  Grid gr1(0);
  print_congruences(gr1, "*** gr1 ***");
  Grid gr2(0);
  print_congruences(gr2, "*** gr2 ***");

  gr1.intersection_assign_and_minimize(gr2);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

// Many dimension grids from generators.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs1;
  gs1.insert(grid_point(A + C));
  gs1.insert(grid_point(A + 2*C));

  Grid_Generator_System gs2;
  gs2.insert(grid_point(A));
  gs2.insert(grid_point(A + B));

  Grid gr1(gs1);
  print_generators(gr1, "*** gr1 ***");

  Grid gr2(3, EMPTY);
  gr2.add_generators(gs2);
  print_generators(gr2, "*** gr2 ***");

  gr1.intersection_assign_and_minimize(gr2);

  Congruence_System known_cgs;
  known_cgs.insert((C == 0) / 0);
  known_cgs.insert((A == 1) / 0);
  known_cgs.insert((B == 0) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_generators(gr1, "*** gr.intersection_assign_and_minimize(gr2) ***");

  return ok;
}

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
END_MAIN
