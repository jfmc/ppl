/* Test Grid::affine_image().
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

// Denominator, with generators having a variety of divisors.

bool
test01() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point());
  gs.insert(grid_point(A, 3));
  gs.insert(grid_point(B, 2));

  // The divisors are normalized on construction.
  Grid gr(gs);

  print_generators(gr, "*** gr ***");

  // All divisors should change, even when the coefficient of A is 0.
  gr.affine_image(A, 2*A, 5);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(2*A, 15));
  known_gr.add_generator(grid_point(5*B, 10));

  bool ok = (gr == known_gr);

  print_generators(gr,
        "*** gr.affine_image(A, 2*A, 5) ***");

  return ok;
}

// Negative denominator.

bool
test02() {
  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(A));

  print_generators(gr, "*** gr ***");

  gr.affine_image(A, B + 2, -3);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-2*A, 3));

  bool ok = (gr == known_gr);

  print_generators(gr,
        "*** gr.affine_image(A, B + 2, -3) ***");

  return ok;
}

// Empty grid.

bool
test03() {
  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);

  print_congruences(gr, "*** gr ***");

  gr.affine_image(A, 2*A + B + 1);

  Grid known_gr(2, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.affine_image(A, 2*A + B + 1) ***");

  return ok;
}

// Shift a rectilinear pointed grid along A.

bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((A %= 0) / 4);
  gr.add_congruence((B %= 0) / 2);

  print_congruences(gr, "*** gr ***");

  gr.affine_image(A, A + 3);

  Grid known_gr(3);
  known_gr.add_congruence((A %= 3) / 4);
  known_gr.add_congruence((B %= 0) / 2);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.affine_image(A, A + 3) ***");

  return ok;
}

// Slant a rectilinear pointed grid along A == B.

bool
test05() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence((A %= 0) / 4);
  gr.add_congruence((B %= 0) / 2);

  print_congruences(gr, "*** gr ***");

  gr.affine_image(A, A + B);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 0) / 4);
  known_gr.add_congruence((A %= 0) / 2);
  known_gr.add_congruence((B %= 0) / 2);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.affine_image(A, A + B) ***");

  return ok;
}

// Compress a rectilinear pointed grid to a line of points.

bool
test06() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);

  print_congruences(gr, "*** gr ***");

  gr.add_congruence((A %= 0) / 4);
  gr.add_congruence((B %= 0) / 2);

  gr.affine_image(A, B);

  Grid known_gr(2);
  known_gr.add_congruence(A - B == 0);
  known_gr.add_congruence((A %= 0) / 2);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.affine_image(A, B) ***");

  return ok;
}

// Zero denominator.

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.affine_image(B, A + 2, 0);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Expression of a greater space dimension than the grid.

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.affine_image(B, D + 2);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Variable of a greater space dimension than the grid.

bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.affine_image(D, A + 2);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
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
END_MAIN
