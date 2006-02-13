/* Test Grid::generalized_affine_image(lhs, rhs, modulus).
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
Variable D(3);

// Tests 1 to 13 are equivalent to tests 1 to 13 in
// generalizedaffineimage1.cc.

// Simplest expression.

void
test1() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((B %= 0) / 2);

  gr.generalized_affine_image(1*B, Linear_Expression::zero(), 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Simplest expression, with denominator.

void
test2() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence(B %= 0);

  gr.generalized_affine_image(2*B, Linear_Expression::zero());

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_point(B, 2));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Simple expression.

void
test3() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(1*B, A + 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A - B));
  known_gr.add_generator(grid_point(B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Simple expression, with denominator.

void
test4() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(2*B, A + 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_point(B, 2));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Simple expression, with denominator and modulus.

void
test5() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(2*B, A + 1, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(B, 2));
  known_gr.add_generator(grid_point(A + B));
  known_gr.add_generator(grid_point(2*B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Simple expression, with denominator and modulus.

void
test6() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(2*B, A + 1, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(B, 2));
  known_gr.add_generator(grid_point(A + B));
  known_gr.add_generator(grid_point(2*B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Negative denominator.

void
test7() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A - B %= 0) / 2);

  gr.generalized_affine_image(-2*B, A + 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-B, 2));
  known_gr.add_generator(grid_point(2*A + -3*B, 2));
  known_gr.add_generator(grid_point(-B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Negative modulus.

void
test8() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A - B %= 0) / 2);

  gr.generalized_affine_image(1*B, A + 2, -7);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(2*B));
  known_gr.add_generator(grid_point(A + 3*B));
  known_gr.add_generator(grid_point(9*B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Expression of many variables.

void
test9() {
  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_image(1*A, A - C + 2, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(2*A));
  known_gr.add_generator(grid_point(2*A + 3*B));
  known_gr.add_generator(grid_line(A + C));
  known_gr.add_generator(grid_point(4*A));  // Original modulus.
  known_gr.add_generator(grid_point(7*A));  // Transformation modulus.

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Equality expression.

void
test10() {
  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_image(1*A, A - C + 2, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(2*A));
  known_gr.add_generator(grid_point(2*A + 3*B));
  known_gr.add_generator(grid_line(A + C));
  known_gr.add_generator(grid_point(4*A));  // Original modulus.

  if (gr == known_gr) {
    gr = Grid(3);
    gr.add_congruence((B %= 0) / 3);
    gr.add_congruence((A - 2*C %= 0) / 2);

    gr.affine_image(A, A - C + 2);

    if (gr == known_gr)
      return;

    nout << "affine_image";
  }
  else
    nout << "generalized_affine_image";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty grid.

void
test11() {
  Grid gr(5, EMPTY);

  gr.generalized_affine_image(4*A, A - 2*C + 3, 7);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(5, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty with congruences.

void
test12() {
  Grid gr(1);
  gr.add_congruence(A == 0);
  gr.add_congruence(A == 3);

  gr.generalized_affine_image(1*A, A + 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Universe.

void
test13() {
  Grid gr(1);

  gr.generalized_affine_image(1*A, A + 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Expressions both constants.

void
test14() {
  Grid gr(2);
  gr.add_congruence(A %= 0);

  gr.generalized_affine_image(Linear_Expression::zero(),
			      Linear_Expression(1));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Left hand side constant.

void
test15() {
  Grid gr(2);
  gr.add_congruence(A %= 0);

  gr.generalized_affine_image(Linear_Expression::zero(), A - B, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A + B));
  known_gr.add_generator(grid_point(5*A));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Expressions with unique variables.

void
test16() {
  Grid gr(3);
  gr.add_congruence(A - B == 0);
  gr.add_congruence((C %= 0) / 3);

  gr.generalized_affine_image(A - B, C);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_line(A + B));
  known_gr.add_generator(grid_point(B));
  known_gr.add_generator(grid_point(3*C));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Simple expressions having common variables.

void
test17() {
  Grid gr(2);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_image(A - B, A, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Expressions having common variables.

void
test18() {
  Grid gr(2);
  gr.add_congruence((A %= 0) / 1);
  gr.add_congruence((B %= 0) / 2);

  gr.generalized_affine_image(A + 2*B, A - B, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(B, 2));
  known_gr.add_generator(grid_line(2*A - B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// The equivalent of test1 from Polyhedron/generalizedaffineimage10.cc
// (expressions with unique variables).

void
test19() {
  Grid gr(3);
  gr.add_congruence(C == 0);
  gr.add_congruence(A + 3*B == 2);

  gr.generalized_affine_image(A - C, B + 3, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(A - C == B + 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Expressions having common variables.

void
test20() {
  Grid gr(3);
  gr.add_congruence((C %= 0) / 3);
  gr.add_congruence(A - 2*B == 1);

  gr.generalized_affine_image(A - B + C, 2*A - B - C, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Expressions having common variables, where generalized_affine_image
// must minimize the grid.

void
test21() {
  Grid gr(2);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_image(A - B, 2*A - 2*B, 5);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 0) / 5);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

void
test22() {
  Grid gr(2);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_image(2*A - 2*B, A - B, 5);

  Grid known_gr(2);
  known_gr.add_congruence((2*A - 2*B %= 0) / 5);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Right hand side expression of greater space dimension than the
// grid.

void
test23() {
  Grid gr(3);
  gr.add_congruence(C %= -2);

  try {
    gr.generalized_affine_image(B + C, D + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Left hand side expression of space dimension greater than the grid.

void
test24() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);

  try {
    gr.generalized_affine_image(A + D, A + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generalizedaffineimage2:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST(test18);
  DO_TEST(test19);
  DO_TEST(test20);
  DO_TEST(test21);
  DO_TEST(test22);
  DO_TEST(test23);
  DO_TEST(test24);

  return 0;
}
CATCH
