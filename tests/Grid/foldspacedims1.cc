/* Test Grid::fold_space_dimensions().
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
Variable D(3);

// Test with a universe polyhedron.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(3);

  Variables_Set to_fold;
  to_fold.insert(A);

  gr.fold_space_dimensions(to_fold, B);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Test with an empty polyhedron.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(3, EMPTY);

  Variables_Set to_fold;
  to_fold.insert(A);

  gr.fold_space_dimensions(to_fold, B);

  Grid known_gr(2, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Trivial fold.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(3);
  gr.add_congruence(A %= 0);
  gr.add_congruence((A + B + C %= 2) / 3);

  Grid known_gr = gr;

  Variables_Set to_fold;

  gr.fold_space_dimensions(to_fold, B);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Simple fold from congruences.

void
test4() {
  nout << "test4:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 1);
  gr.add_congruence((B %= 1) / 3);

  Variables_Set to_fold;
  to_fold.insert(A);

  gr.fold_space_dimensions(to_fold, B);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Simple fold from generators.

void
test5() {
  nout << "test5:" << endl;

  Grid gr(3, EMPTY);
  gr.add_generator(point(A + 2*B + 2*C));
  gr.add_generator(point(A + 2*B + 4*C));
  gr.add_generator(point(A + 6*B + 2*C));

  Variables_Set to_fold;
  to_fold.insert(C);

  gr.fold_space_dimensions(to_fold, B);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point(A + 2*B));
  known_gr.add_generator(point(A + 4*B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Test folding several dimensions into a higher dimension.

void
test6() {
  nout << "test6:" << endl;

  Grid gr(3);
  gr.add_congruence((A %= 2) / 3);
  gr.add_congruence((B %= 8) / 9);
  gr.add_congruence((C == 17) / 0);

  Variables_Set to_fold;
  to_fold.insert(A);
  to_fold.insert(B);

  gr.fold_space_dimensions(to_fold, C);

  Grid known_gr(1);
  known_gr.add_congruence((A %= 2) / 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Test folding dimensions into a lower dimension.

void
test7() {
  nout << "test7:" << endl;

  Grid gr(4);
  gr.add_congruence((A - B %= 3) / 4);
  gr.add_congruence((C %= 5) / 9);
  gr.add_congruence((D %= 2) / 6);

  Variables_Set to_fold;
  to_fold.insert(C);
  to_fold.insert(D);

  gr.fold_space_dimensions(to_fold, A);

  Grid known_gr(2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Test folding dimensions into an intermediate dimension.

void
test8() {
  nout << "test8:" << endl;

  Grid gr(4);
  gr.add_congruence((A %= 0) / 2);
  gr.add_congruence((B %= 0) / 9);
  gr.add_congruence((C %= 0) / 6);
  gr.add_congruence((D %= 0) / 12);

  Variables_Set to_fold;
  to_fold.insert(B);
  to_fold.insert(D);

  gr.fold_space_dimensions(to_fold, C);

  Grid known_gr(2);
  known_gr.add_congruence((A %= 0) / 2);
  known_gr.add_congruence((B %= 0) / 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Test folding dimensions of a relational grid into an intermediate
// dimension.

void
test9() {
  nout << "test9:" << endl;

  Grid gr(4);
  gr.add_congruence((A - B %= 0) / 9);
  gr.add_congruence((C %= 0) / 6);
  gr.add_congruence((D %= 0) / 12);

  Variables_Set to_fold;
  to_fold.insert(B);
  to_fold.insert(D);

  gr.fold_space_dimensions(to_fold, C);

  // FIX Is a relational grid always folded into the universe?
  Grid known_gr(2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// One dimension.

void
test10() {
  nout << "test10:" << endl;

  Grid gr(1);
  gr.add_congruence((A %= 3) / 7);

  Grid known_gr = gr;

  Variables_Set to_fold;

  gr.fold_space_dimensions(to_fold, A);

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

  nout << "foldspacedims1:" << endl;

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

  return 0;
}
CATCH
