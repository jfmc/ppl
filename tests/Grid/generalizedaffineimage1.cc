/* Test Grid::generalized_affine_image(var, ...).
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

// Simplest expression.

void
test1() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((B %= 0) / 2);

  gr.generalized_affine_image(B, Linear_Expression::zero(), 1, 0);

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

  gr.generalized_affine_image(B, Linear_Expression::zero(), 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_point(B));

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

  gr.generalized_affine_image(B, A + 1);

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

  gr.generalized_affine_image(B, A + 1, 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(B, 2));
  known_gr.add_generator(grid_point(B + A));
  known_gr.add_generator(grid_point(3*B, 2));

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

  gr.generalized_affine_image(B, A + 1, 2, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(B, 2));
  known_gr.add_generator(grid_point(A + B));
  known_gr.add_generator(grid_point(7*B, 2));

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
test6() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A - B %= 0) / 2);

  gr.generalized_affine_image(B, A + 2, -2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-B));
  known_gr.add_generator(grid_point(-3*B + 2*A, 2));
  known_gr.add_generator(grid_point(-2*B));

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
test7() {
  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A - B %= 0) / 2);

  gr.generalized_affine_image(B, A + 2, 1, -7);

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
test8() {
  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_image(A, A - C + 2, 1, 5);

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
test9() {
  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_image(A, A - C + 2, 1, 0);

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
test10() {
  Grid gr(5, EMPTY);

  gr.generalized_affine_image(A, A - 2*C + 3, 4, 7);

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
test11() {
  Grid gr(1);
  gr.add_congruence(A == 0);
  gr.add_congruence(A == 3);

  gr.generalized_affine_image(A, A + 2);

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
test12() {
  Grid gr(1);

  gr.generalized_affine_image(A, A + 2);

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

// Zero denominator.

void
test13() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.generalized_affine_image(B, A + 2, 0);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Expression of a greater space dimension than the grid.

void
test14() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.generalized_affine_image(B, D + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Variable of a greater space dimension than the grid.

void
test15() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.generalized_affine_image(D, A + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generalizedaffineimage1:" << endl;

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

  return 0;
}
CATCH
