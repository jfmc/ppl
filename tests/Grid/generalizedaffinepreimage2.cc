/* Test Grid::generalized_affine_preimage(lhs, rhs, modulus).
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

// Expressions both constants.

void
test1() {
  Grid gr(2);
  gr.add_congruence(A %= 0);

  gr.generalized_affine_preimage(Linear_Expression::zero(),
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
test2() {
  Grid gr(2);
  gr.add_congruence(A %= 0);

  gr.generalized_affine_preimage(Linear_Expression::zero(), A - B, 5);

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
test3() {
  Grid gr(3);
  gr.add_congruence(A - B == 0);
  gr.add_congruence((C %= 0) / 3);

  gr.generalized_affine_preimage(A - B, C);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_line(A));
  known_gr.add_generator(grid_line(B));
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
test4() {
  Grid gr(2);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_preimage(A - B, A, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A == 0);

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
test5() {
  Grid gr(2);
  gr.add_congruence((A %= 0) / 1);
  gr.add_congruence((B %= 0) / 2);

  gr.generalized_affine_preimage(A + 2*B, A - B, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_line(A + B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Test similar to the test in
// ppl/tests/Polyhedron/generalizedaffinepreimage4.cc

void
test6() {
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(A + B));
  gr.add_generator(grid_point(2*A));
  gr.add_generator(grid_point(2*A + 2*B));
  gr.add_generator(grid_point(3*A + B));

  Grid known_gr(gr);

  gr.generalized_affine_preimage(B, B+2, 1, 5);

  if (find_variation(gr))
    exit(1);

// A longer way of computing the generalized affine preimage below.
  known_gr.add_space_dimensions_and_embed(1);
  known_gr.add_congruence((B %= C+2) / 5);
  Variables_Set vset;
  vset.insert(B);
  known_gr.remove_space_dimensions(vset);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}


// Expressions having common variables.

void
test7() {
  Grid gr(3);
  gr.add_congruence((C %= 0) / 3);
  gr.add_congruence(A - 2*B == 1);

  gr.generalized_affine_preimage(A - B + C, 2*A - B - C, 5);

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

// Expressions having common variables, where
// generalized_affine_preimage must minimize the grid.

void
test8() {
  Grid gr(2);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_preimage(A - B, 2*A - 2*B, 5);

  Grid known_gr(2);
  known_gr.add_congruence((2*A - 2*B %= 0) / 5);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Expressions having common variables, where
// generalized_affine_preimage must minimize the grid.

void
test9() {
  Grid gr(2);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_preimage(2*A - 2*B, A - B, 5);

  Grid known_gr(2);
  known_gr.add_congruence((A - B %= 0) / 5);

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
test10() {
  Grid gr(3);
  gr.add_congruence(C %= -2);

  try {
    gr.generalized_affine_preimage(B + C, D + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Left hand side expression of space dimension greater than the grid.

void
test11() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);

  try {
    gr.generalized_affine_preimage(A + D, A + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Expressions having common variables, with a negative modulus.

void
test12() {
  Grid gr(3);
  gr.add_congruence((C %= 0) / 3);
  gr.add_congruence(A - B == 0);

  gr.generalized_affine_preimage(A - B, C, -5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((C %= 0) / 15);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generalizedaffinepreimage2:" << endl;

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

  return 0;
}
CATCH
