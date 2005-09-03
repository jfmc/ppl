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

// Simplest expression.

void
test1() {
  nout << "test1:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence(B %= 0);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, Linear_Expression::zero(), 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));
  known_gr.add_generator( line(B));

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
  nout << "test2:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, Linear_Expression::zero(), 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));
  known_gr.add_generator( line(B));

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
  nout << "test3:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + 1, 1, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(2*A));  // FIX why 2?
  known_gr.add_generator( line(B));

  if (gr1 == known_gr) {

    // FIX known_gr should be same for gr1 and gr2?
    known_gr.add_generator(point(A));

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
  nout << "test4:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((A + B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + 1, 2, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(4*A));  // FIX why 4?
  known_gr.add_generator( line(B));

  if (gr1 == known_gr) {

    // FIX known_gr should be same for gr1 and gr2?
    known_gr.add_generator(point(2*A));

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
  nout << "test5:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((A - B %= 0) / 2);

  Grid gr2 = gr1;

  // Equality expression.
  gr1.generalized_affine_preimage(B, A + 2, -2, 0);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(4*A));  // FIX why 4?
  known_gr.add_generator( line(B));

  if (gr1 == known_gr) {

    // FIX known_gr should be same for gr1 and gr2?
    known_gr.add_generator(point(2*A));

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
  nout << "test6:" << endl;

  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_preimage(A, A - C + 2, 1, 5);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
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
test7() {
  nout << "test7:" << endl;

  Grid gr(3);
  gr.add_congruence((B %= 0) / 3);
  gr.add_congruence((A - 2*C %= 0) / 2);

  gr.generalized_affine_preimage(A, A - C + 2, 1, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(point(2*A));
  known_gr.add_generator(point(2*A + 3*B));
  known_gr.add_generator( line(A + C));
  known_gr.add_generator(point(4*A));  // Original modulus.

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
  nout << "test8:" << endl;

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
  nout << "test9:" << endl;

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
  nout << "test10:" << endl;

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

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generalizedaffinepreimage1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
#if 0
  test6();
  test7();
#endif
  test8();
  test9();
  test10();

  return 0;
}
CATCH
