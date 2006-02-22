/* Test Grid::add_generator*().
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

// grid1.cc also tests add_generator_and_minimize.

// One dimension.

bool
test01() {
  Variable A(0);

  Grid gr(1, EMPTY);

  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point(-A));

  Grid known_gr(1);
  known_gr.add_congruence((A == -1) / 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(grid_point(-A)) ***");

  return ok;
}

// Two dimensions.

bool
test02() {
  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);

  print_congruences(gr, "*** gr ***");
  gr.add_generator(grid_point(A + B));

  Grid known_gr(2);
  known_gr.add_congruence((A == 1) / 0);
  known_gr.add_congruence((B == 1) / 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(grid_point(A + B)) ***");

  return ok;
}

// Add many generators to grid of two dimensions.

bool
test03() {
  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);

  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A + 2*B));
  gr.add_generator(grid_point(A + B));
  gr.add_generator(grid_point(2*A + 2*B));
  gr.add_generator(grid_line(A));

  Grid known_gr(2);
  known_gr.add_congruence(B %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(...) ***");

  return ok;
}

// Add NNC generators.

bool
test04() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  //gs.insert(closure_point(3*A, 4)); // FIX
  gs.insert(grid_point(7*A, 4));
  gs.insert(grid_line(A - B));

  Grid gr(2, EMPTY);

  print_congruences(gr, "*** gr ***");

  for (Grid_Generator_System::const_iterator i = gs.begin(),
	 gs_end = gs.end(); i != gs_end; ++i)
    gr.add_generator(*i);

  Grid known_gr(2);
  known_gr.add_congruence((4*A + 4*B == 7) / 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(*i) ***");

  return ok;
}

// Add generators to a grid of a higher space dimension.

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4, EMPTY);
  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point(7*A, 3));
  print_congruences(gr, "*** gr.add_generator(grid_point(7*A, 3)) ***");
  gr.add_generator(grid_line(A - B));

  Grid known_gr(4);

  known_gr.add_congruence((3*A + 3*B == 7) / 0);
  known_gr.add_congruence((C == 0) / 0);
  known_gr.add_congruence((D == 0) / 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(grid_line(A - B)) ***");

  return ok;
}

// add_generator_and_minimize

bool
test06() {
  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(2*A + 2*B));

  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point(8*A + 8*B));

  gr.add_generator_and_minimize(grid_line(A));

  Grid known_gr(2);
  known_gr.add_congruence((B %= 0) / 2);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator_and_minimize(grid_line(A)) ***");

  return ok;
}

// Add a generator to a universe grid.

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4);

  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point(12*A + 7*D));

  Grid known_gr(4);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "***  ***");

  return ok;
}

// add_generator_and_minimize, adding a generator with a divisor to a
// grid of many generators.

bool
test08() {
  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);

  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));

  print_congruences(gr, "*** gr ***");

  // Minimize the grid.

  gr.add_generator_and_minimize(grid_point(B, 3));

  Grid known_gr(2);
  known_gr.add_congruence(A %= 0);
  known_gr.add_congruence(3*B %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator_and_minimize(grid_point(B, 3)) ***");

  return ok;
}

// Space dimension exception.

bool
test09() {
  Variable A(0);
  Variable C(2);

  Grid gr(2);

  try {
    gr.add_generator(grid_point(A + C));
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Zero dimensions empty.

bool
test10() {
  Grid gr(0, EMPTY);

  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point());

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(grid_point()) ***");

  return ok;
}

// Zero dimension universe.

bool
test11() {
  Grid gr(0);

  print_congruences(gr, "*** gr ***");

  gr.add_generator(grid_point());

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_generator(grid_point()) ***");

  return ok;
}

// Space dimension exception.

bool
test12() {
  Variable A(0);

  Grid gr(2, EMPTY);

  try {
    gr.add_generator(grid_line(A));
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Try add parameter to empty grid.

bool
test13() {
  Grid gr(2, EMPTY);

  try {
    gr.add_generator(parameter());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Try add parameter to zero dimension empty grid.

bool
test14() {
  Grid gr(0, EMPTY);

  try {
    gr.add_generator(parameter());
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

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
  NEW_TEST(test13);
  NEW_TEST(test14);
END_MAIN
