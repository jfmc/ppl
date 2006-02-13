/* Test Grid::affine_preimage().
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

// The first twelve tests mirror those in affineimage1.cc.

// Grid defined by generators.

void
test1() {
  Grid gr(3, EMPTY);
  gr.add_generator(grid_point(2*B - 2*C));

  gr.affine_preimage(B, A + 2, 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((C == -2) / 0);
  known_gr.add_congruence((A ==  0) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Grid defined by congruences.

void
test2() {
  Grid gr(3);
  gr.add_congruence((A %= 1) / 6);
  gr.add_congruence((3*A - 6*B %= 3) / 0);

  gr.affine_preimage(A, A + B + 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((A - B %= 0) / 0);
  known_gr.add_congruence((A %= 0) / 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Denominator.

void
test3() {
  Grid gr(3, EMPTY);
  gr.add_generator(grid_point(2*A + 2*B));
  gr.add_generator(grid_line(C));

  gr.affine_preimage(A, A + 1, 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((A %= 3) / 0);
  known_gr.add_congruence((B %= 2) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Invertible transformation with denominator, modulus and up-to-date
// congruences.

void
test4() {
  Grid gr(3);
  gr.add_congruence((B %= 2) / 0);
  gr.add_congruence((3*A %= 4) / 5);

  if (find_variation(gr))
    exit(1);

  gr.affine_preimage(A, A + 1, 3);

  Grid known_gr(3);
  known_gr.add_congruence((A %= 3) / 5);
  known_gr.add_congruence((B %= 2) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Simple invertible transformation with denominator and modulus.

void
test5() {
  Grid gr(3, EMPTY);
  gr.add_generator(grid_point(4*A + 6*B, 3));
  gr.add_generator(grid_point(9*A + 6*B, 3));
  gr.add_generator(grid_line(C));

  gr.affine_preimage(A, A + 1, 3);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point(3*A + 2*B));
  known_gr.add_generator(grid_point(8*A + 2*B));
  known_gr.add_generator(grid_line(C));

  if (gr == known_gr) {

    // Up-to-date, minimized congruences.

    gr = Grid(3);
    gr.add_congruence((3*A %= 4) / 5);
    gr.add_congruence((B %= 2) / 0);

    // Minimize the congruences.
    if (find_variation(gr))
      exit(1);

    gr.affine_preimage(A, A + 1, 3);

    if (gr == known_gr)
      return;

    nout << "Congruence";
  }
  else
    nout << "Generator";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Invertible transformation which changes the modulus.

void
test6() {
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(4*A));
  gr.add_generator(grid_point(7*A));
  gr.add_generator(grid_point(6*A + B));

  // Out-of-date congruences.

  gr.affine_preimage(A, 3*A + 2*B + 4);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));
  known_gr.add_generator(grid_point(B));

  if (gr == known_gr) {

    // Up-to-date congruences.

    gr = Grid(2);
    gr.add_congruence((A - 2*B %= 4) / 3);
    gr.add_congruence(B %= 0);

    gr.affine_preimage(A, 3*A + 2*B + 4);

    if (gr == known_gr)
      return;

    nout << "Up-to-date cgs";
  }
  else
    nout << "Out-of-date cgs";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// One dimension.

void
test7() {
  Grid gr(1, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(2*A));

  // Out-of-date congruences.

  gr.affine_preimage(A, 2*A);

  Grid known_gr(1, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A));

  if (gr == known_gr) {

    // Up-to-date congruences.

    gr = Grid(1);
    gr.add_congruence((A %= 0) / 2);

    gr.affine_preimage(A, 2*A);

    if (gr == known_gr)
      return;

    nout << "Up-to-date cgs";
  }
  else
    nout << "Out-of-date cgs";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// The first affine_image example described at anchor
// grid_affine_transformation in definitions.dox.

void
test8() {
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(A));
  gr.add_generator(grid_point(7*A + 3*B));
  gr.add_generator(grid_point(10*A));

  // Out-of-date congruences.

  gr.affine_preimage(A, 3*A + 2*B + 1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(3*A));
  known_gr.add_generator(grid_point(3*B));

  if (gr == known_gr) {

    // Up-to-date, minimized congruences.

    gr = Grid(2);
    // FIX Confirm that this should fail when Grid_nonpublic.cc
    //     BE_LAZY=0.
    //gr.add_congruence((3*A - 6*B %= 3) / 9);
    gr.add_congruence((A - 2*B %= 1) / 9);
    gr.add_congruence((B %= 0) / 3);

    // Minimize the congruences.
    if (find_variation(gr))
      exit(1);

    gr.affine_preimage(A, 3*A + 2*B + 1);

    if (gr == known_gr)
      return;

    nout << "Up-to-date cgs";
  }
  else
    nout << "Out-of-date cgs";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// The second affine_image example described at anchor
// grid_affine_transformation in definitions.dox.

void
test9() {
  // Out-of-date congruences.
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(3*A + 3*B));

  gr.affine_preimage(A, B);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_line(A));
  known_gr.add_generator(grid_point(3*B));

  if (gr == known_gr) {

    // Up-to-date congruences.

    gr = Grid(2);
    gr.add_congruence((A - B == 0) / 0);
    gr.add_congruence((A %= 0) / 3);

    gr.affine_preimage(A, B);

    if (gr == known_gr)
      return;

    nout << "Up-to-date cgs";
  }
  else
    nout << "Out-of-date cgs";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Denominator, with generators having a variety of divisors.

void
test10() {
  Grid_Generator_System gs;
  gs.insert(grid_point());
  gs.insert(grid_point(2*A, 15));
  gs.insert(grid_point(5*B, 10));

  // The divisors are normalized on construction.
  Grid gr(gs);

  // All divisors should change, even when the coefficient of A is 0.
  gr.affine_preimage(A, 2*A, 5);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A, 3));
  known_gr.add_generator(grid_point(B, 2));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Negative denominator.

void
test11() {
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(-2*A, 3));

  gr.affine_preimage(A, B + 2, -3);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_line(A));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Empty grid.

void
test12() {
  Grid gr(2, EMPTY);

  gr.affine_preimage(A, 11*A - B + 1);

  Grid known_gr(2, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// A negative coefficient of the variable to transform and a negative
// denominator.

void
test13() {
  // Out-of-date congruences.
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(5*A + 4*B, 7));

  gr.affine_preimage(B, A - B, -1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point(5*A + 9*B, 7));

  if (gr == known_gr) {

    // Up-to-date congruences.

    gr = Grid(2);
    gr.add_congruence((7*A == 5) / 0);
    gr.add_congruence((7*B == 4) / 0);

    gr.affine_preimage(B, A - B, -1);

    if (gr == known_gr)
      return;

    nout << "Up-to-date cgs";
  }
  else
    nout << "Out-of-date cgs";

  nout << " grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Shift a rectilinear pointed grid along A.

void
test14() {
  Grid gr(3);
  gr.add_congruence((A %= 0) / 4);
  gr.add_congruence((B %= 0) / 2);

  gr.affine_preimage(A, A + 3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((A %= -3) / 4);
  known_gr.add_congruence((B %= 0) / 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Slant a rectilinear pointed grid along A == -B.

void
test15() {
  Grid gr(2);
  gr.add_congruence((A %= 0) / 4);
  gr.add_congruence((B %= 0) / 2);

  gr.affine_preimage(A, A + B);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A + B %= 0) / 4);
  known_gr.add_congruence((A %= 0) / 2);
  known_gr.add_congruence((B %= 0) / 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Expand a rectilinear pointed grid to a sequence of lines.

void
test16() {
  Grid gr(2);
  gr.add_congruence((A %= 0) / 4);
  gr.add_congruence((B %= 0) / 2);

  gr.affine_preimage(A, B);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((B %= 0) / 4);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Transform a grid into the empty grid.

void
test17() {
  Grid gr(2);
  gr.add_congruence(A == 3);
  gr.add_congruence(B == 0);

  gr.affine_preimage(B, A);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Transform away the constraint on a dimension (B).

void
test18() {
  Grid gr(2);
  gr.add_congruence(A == 3);
  gr.add_congruence(B == 3);

  gr.affine_preimage(B, A);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A == 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Zero denominator.

void
test19() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.affine_preimage(B, A + 2, 0);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Expression of a greater space dimension than the grid.

void
test20() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.affine_preimage(B, D + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Variable of a greater space dimension than the grid.

void
test21() {
  Grid gr(3);
  gr.add_congruence((C == -2) / 0);
  gr.add_congruence((A ==  0) / 0);

  try {
    gr.affine_preimage(D, A + 2);
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "affinepreimage1:" << endl;

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

  return 0;
}
CATCH
