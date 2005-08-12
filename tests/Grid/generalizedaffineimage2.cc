/* Test Grid::generalized_affine_image(lhs, rhs, modulus).
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Tests 1 to 13 are equivalent to tests 1 to 13 in
// generalizedaffineimage1.cc.

// Simplest expression.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((B %= 0) / 2);

  gr.generalized_affine_image(1*B, Linear_Expression::zero(), 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));

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
  nout << "test2:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence(B %= 0);

  gr.generalized_affine_image(2*B, Linear_Expression::zero());

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));
  known_gr.add_generator(point(B, 2));

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
  nout << "test3:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(1*B, A + 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A - B));
  known_gr.add_generator(point(B));

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
  nout << "test4:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(2*B, A + 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));
  known_gr.add_generator(point(B, 2));

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
  nout << "test5:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(2*B, A + 1, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point(B, 2));
  known_gr.add_generator(point(A + B));
  known_gr.add_generator(point(2*B));

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
  nout << "test6:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B %= 0) / 2);

  gr.generalized_affine_image(2*B, A + 1, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point(B, 2));
  known_gr.add_generator(point(A + B));
  known_gr.add_generator(point(2*B));

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
  nout << "test7:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A - B %= 0) / 2);

  gr.generalized_affine_image(-2*B, A + 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point(-B, 2));
  known_gr.add_generator(point(2*A + -3*B, 2));
  known_gr.add_generator(point(-B));

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
  nout << "test8:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A - B %= 0) / 2);

  gr.generalized_affine_image(1*B, A + 2, -7);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point(2*B));
  known_gr.add_generator(point(A + 3*B));
  known_gr.add_generator(point(9*B));

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
  nout << "test9:" << endl;

  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_image(1*A, A - C + 2, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generator(point(2*A));
  known_gr.add_generator(point(2*A + 3*B));
  known_gr.add_generator( line(A + C));
  known_gr.add_generator(point(4*A));  // Original modulus.
  known_gr.add_generator(point(7*A));  // Transformation modulus.

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
  nout << "test10:" << endl;

  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_image(1*A, A - C + 2, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generator(point(2*A));
  known_gr.add_generator(point(2*A + 3*B));
  known_gr.add_generator( line(A + C));
  known_gr.add_generator(point(4*A));  // Original modulus.

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
  nout << "test11:" << endl;

  Grid gr(5, Grid::EMPTY);

  gr.generalized_affine_image(4*A, A - 2*C + 3, 7);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(5, Grid::EMPTY);

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
  nout << "test12:" << endl;

  Grid gr(1);
  gr.add_congruence(A == 0);
  gr.add_congruence(A == 3);

  gr.generalized_affine_image(1*A, A + 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1, Grid::EMPTY);

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
  nout << "test13:" << endl;

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
  nout << "test14:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);

  gr.generalized_affine_image(Linear_Expression::zero(),
			      Linear_Expression(1));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));
  known_gr.add_generator( line(B));

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
  nout << "test15:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);

  gr.generalized_affine_image(Linear_Expression::zero(), A - B, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A + B));
  known_gr.add_generator(point(5*A));

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
  nout << "test16:" << endl;

  Grid gr(3);
  gr.add_congruence(A - B == 0);
  gr.add_congruence((C %= 0) / 3);

  gr.generalized_affine_image(A - B, C);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator( line(A + B));
  known_gr.add_generator(point(B));
  known_gr.add_generator(point(3*C));

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
  nout << "test17:" << endl;

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
  nout << "test18:" << endl;

  Grid gr(2);
  gr.add_congruence((A %= 0) / 1);
  gr.add_congruence((B %= 0) / 2);

  gr.generalized_affine_image(A + 2*B, A - B, 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generator(point(A));
  known_gr.add_generator(point(B));
  known_gr.add_generator(line(2*A - B));

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
  nout << "test19:" << endl;

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
  nout << "test20:" << endl;

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
  nout << "test21:" << endl;

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
  nout << "test22:" << endl;

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

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generalizedaffineimage2:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();
  test11();
  test12();
  test13();
  test14();
  test15();
  test16();
  test17();
  test18();
  test19();
  test20();
  test21();
  test22();

  return 0;
}
CATCH
