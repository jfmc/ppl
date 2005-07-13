/* Test Grid::expand_space_dimension().
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
Variable E(4);

// Universe.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(3);

  gr.expand_space_dimension(A, 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(3, Grid::EMPTY);

  gr.expand_space_dimension(B, 1);

  Grid known_gr(4, Grid::EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Trivial expansion.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(2);
  gr.add_congruence(A %= 0);
  gr.add_congruence(A + B %= 2);

  gr.expand_space_dimension(A, 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A %= 0);
  known_gr.add_congruence(A + B %= 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// From generators, expanding one dimension.

void
test4() {
  nout << "test4:" << endl;

  Grid gr(2, Grid::EMPTY);
  gr.add_generator(point(A));
  gr.add_generator(point(A + 2*B));
  gr.add_generator(point());

  gr.expand_space_dimension(A, 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(A));
  known_gr.add_generator(point(A + 2*B));
  known_gr.add_generator(point(C));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// From congruences, expanding one dimension.

void
test5() {
  nout << "test5:" << endl;

  Grid gr(2);
  gr.add_congruence((A + B %= 2) / 7);

  gr.expand_space_dimension(A, 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((A + B     %= 2) / 7);
  known_gr.add_congruence((    B + C %= 2) / 7);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// From congruences, expanding two dimensions.

void
test6() {
  nout << "test6:" << endl;

  Grid gr(2);
  gr.add_congruence((A + 2*B %= 3) / 5);

  gr.expand_space_dimension(B, 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence((A + 2*B             %= 3) / 5);
  known_gr.add_congruence((A       + 2*C       %= 3) / 5);
  known_gr.add_congruence((A             + 2*D %= 3) / 5);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// From congruences, with an equality.

void
test7() {
  nout << "test7:" << endl;

  Grid gr(3);
  gr.add_congruence(2*C == 1);
  gr.add_congruence(A - B %= 0);

  gr.expand_space_dimension(A, 1);
  gr.expand_space_dimension(C, 1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(5);
  known_gr.add_congruence(2*C == 1);
  known_gr.add_congruence(2*E == 1);
  known_gr.add_congruence(A - B         %= 0);
  known_gr.add_congruence(  - B + D     %= 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "expandspacedim1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();

  return 0;
}
CATCH
