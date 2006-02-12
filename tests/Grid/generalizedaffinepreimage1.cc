/* Test Grid::generalized_affine_preimage(var, ...).
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

// Simplest expression.

void
test1() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence(B %= 0);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, Linear_Expression::zero(), 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(B));

  if (gr1 == known_gr) {
    // Congruence expression.
    gr2.generalized_affine_preimage(B, Linear_Expression::zero(), 1, 2);

    if (gr2 == known_gr)
      return;

    gr1 = gr2;
    nout << "Grid gr2";
  }
  else
    nout << "Grid gr1";

  nout << " should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

void
test2() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, Linear_Expression::zero(), 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(B));

  if (gr1 == known_gr) {
    // Congruence expression.
    gr2.generalized_affine_preimage(B, Linear_Expression::zero(), 1, 3);

    if (gr2 == known_gr)
      return;

    gr1 = gr2;
    nout << "Grid gr2";
  }
  else
    nout << "Grid gr1";

  nout << " should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Simple expression, including negative modulus.

void
test3() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + 1, 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-A));
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(B));

  if (gr1 == known_gr) {

    known_gr.add_generator(grid_point());

    // Congruence expression.
    gr2.generalized_affine_preimage(B, A + 1, 1, -7);

    if (gr2 == known_gr)
      return;

    gr1 = gr2;
    nout << "Grid gr2";
  }
  else
    nout << "Grid gr1";

  nout << " should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Simple expression, with denominator.

void
test4() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((A + B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + 1, 2, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-3*A));
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(B));

  if (gr1 == known_gr) {

    known_gr.add_generator(grid_point(-A));

    // Congruence expression.
    gr2.generalized_affine_preimage(B, A + 1, 2, 3);

    if (gr2 == known_gr)
      return;

    gr1 = gr2;
    nout << "Grid gr2";
  }
  else
    nout << "Grid gr1";

  nout << " should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Negative denominator.

void
test5() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((A - B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + 2, -2, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(-2*A));
  known_gr.add_generator(grid_point(2*A));
  known_gr.add_generator(grid_line(B));

  if (gr1 == known_gr) {

    known_gr.add_generator(grid_point());

    // Congruence expression.
    gr2.generalized_affine_preimage(B, A + 2, -2);

    if (gr2 == known_gr)
      return;

    gr1 = gr2;
    nout << "Grid gr2";
  }
  else
    nout << "Grid gr1";

  nout << " should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

// Expression of many variables.

void
test6() {
  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_preimage(A, A - C + 2, 1, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(2*A));
  known_gr.add_generator(grid_point(2*A + 3*B));
  known_gr.add_generator(grid_line(3*A + C));
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
test7() {
  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_preimage(A, A - C + 2, 1, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(2*A));
  known_gr.add_generator(grid_point(2*A + 3*B));
  known_gr.add_generator(grid_line(3*A + C));
  known_gr.add_generator(grid_point(4*A));  // Original modulus.

  if (gr == known_gr) {
    gr = Grid(3);
    gr.add_congruence((B %= 0) / 3);
    gr.add_congruence((A - 2*C %= 0) / 2);

    gr.affine_preimage(A, A - C + 2);

    if (gr == known_gr)
      return;

    nout << "affine_preimage";
  }
  else
    nout << "generalized_affine_preimage";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty grid.

void
test8() {
  Grid gr(5, EMPTY);

  gr.generalized_affine_preimage(A, A - 2*C + 3, 4, 7);

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
test9() {
  Grid gr(1);
  gr.add_congruence(A == 0);
  gr.add_congruence(A == 3);

  gr.generalized_affine_preimage(A, A + 2);

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
test10() {
  Grid gr(1);

  gr.generalized_affine_preimage(A, A + 2);

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
test11() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.generalized_affine_preimage(B, A + 2, 0);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Expression of a greater space dimension than the grid.

void
test12() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.generalized_affine_preimage(B, D + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Variable of a greater space dimension than the grid.

void
test13() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.generalized_affine_preimage(D, A + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Expression with a negative modulus, where the variable occurs in
// the expression.

void
test14() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + B, 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(parameter(2*B));
  known_gr.add_generator(parameter(A + B));

  if (gr1 == known_gr) {

    known_gr.add_generator(parameter(B));

    // Congruence expression.
    gr2.generalized_affine_preimage(B, A + B, 1, -7);

    if (gr2 == known_gr)
      return;

    gr1 = gr2;
    nout << "Grid gr2";
  }
  else
    nout << "Grid gr1";

  nout << " should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generalizedaffinepreimage1:" << endl;

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

  return 0;
}
CATCH
