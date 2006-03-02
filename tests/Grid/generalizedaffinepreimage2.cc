/* Test Grid::generalized_affine_preimage(lhs, rhs, modulus).
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

// Expressions both constants.
bool
test01() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence(A %= 0);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(Linear_Expression::zero(),
				 Linear_Expression(1));

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(B));

  bool ok = (gr == known_gr);

  print_congruences(gr,
    "*** gr.generalized_affine_preimage(Linear_Expression::zero(), Linear_Expression(1)) ***");

  return ok;
}

// Left hand side constant.
bool
test02() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence(A %= 0);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(Linear_Expression::zero(), A - B, 5);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A + B));
  known_gr.add_generator(grid_point(5*A));

  bool ok = (gr == known_gr);

  print_congruences(gr,
    "*** gr.generalized_affine_preimage(Linear_Expression::zero(), A - B, 5) ***");

  return ok;
}

// Expressions with unique variables.
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence(A - B == 0);
  gr.add_congruence((C %= 0) / 3);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(A - B, C);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_line(A));
  known_gr.add_generator(grid_line(B));
  known_gr.add_generator(grid_point(3*C));

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "***  ***");

  return ok;
}

// Simple expressions having common variables.
bool
test04() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence(A - B == 0);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(A - B, A, 0);

  Grid known_gr(2);
  known_gr.add_congruence(A == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.generalized_affine_preimage(A - B, A, 0) ***");

  return ok;
}

// Expressions having common variables.
bool
test05() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence((A %= 0) / 1);
  gr.add_congruence((B %= 0) / 2);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(A + 2*B, A - B, 3);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(A + B));

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "***  ***");

  return ok;
}

// Test0 similar to the test in
// ppl/test0s/Polyhedron/generalizedaffinepreimage4.cc
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(A + B));
  gr.add_generator(grid_point(2*A));
  gr.add_generator(grid_point(2*A + 2*B));
  gr.add_generator(grid_point(3*A + B));

  Grid known_gr(gr);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(B, B+2, 1, 5);

// A longer way of computing the generalized affine preimage below.
  known_gr.add_space_dimensions_and_embed(1);
  known_gr.add_congruence((B %= C+2) / 5);
  Variables_Set vset;
  vset.insert(B);
  known_gr.remove_space_dimensions(vset);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.generalized_affine_preimage(A + 2*B, A - B, 3) ***");

  return ok;
}


// Expressions having common variables.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((C %= 0) / 3);
  gr.add_congruence(A - 2*B == 1);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(A - B + C, 2*A - B - C, 5);

  Grid known_gr(3);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.generalized_affine_preimage(A - B + C, 2*A - B - C, 5) ***");

  return ok;
}

// Expressions having common variables, where
// generalized_affine_preimage must minimize the grid.
bool
test08() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence(A - B == 0);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(A - B, 2*A - 2*B, 5);

  Grid known_gr(2);
  known_gr.add_congruence((2*A - 2*B %= 0) / 5);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.generalized_affine_preimage(A - B, 2*A - 2*B, 5) ***");

  return ok;
}

// Expressions having common variables, where
// generalized_affine_preimage must minimize the grid.
bool
test09() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence(A - B == 0);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(2*A - 2*B, A - B, 5);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 0) / 5);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.generalized_affine_preimage(2*A - 2*B, A - B, 5) ***");

  return ok;
}

// Right hand side expression of greater space dimension than the
// grid.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(3);
  gr.add_congruence(C %= -2);

  try {
    gr.generalized_affine_preimage(B + C, D + 2);
 }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Left hand side expression of space dimension greater than the grid.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(3);
  gr.add_congruence((C == -2) / 0);

  try {
    gr.generalized_affine_preimage(A + D, A + 2);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Expressions having common variables, with a negative modulus.
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((C %= 0) / 3);
  gr.add_congruence(A - B == 0);

  print_congruences(gr, "*** gr ***");

  gr.generalized_affine_preimage(A - B, C, -5);

  Grid known_gr(3);
  known_gr.add_congruence((C %= 0) / 15);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.generalized_affine_preimage(A - B, C, -5) ***");

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
