/* Test Grid::add_generator*().
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

// grid1.cc also tests add_generator_and_minimize.

// One dimension.

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);

  Grid gr(1, EMPTY);
  gr.add_generator(grid_point(-A));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence((A == -1) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Two dimensions.

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(A + B));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A == 1) / 0);
  known_gr.add_congruence((B == 1) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add many generators to grid of two dimensions.

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A + 2*B));
  gr.add_generator(grid_point(A + B));
  gr.add_generator(grid_point(2*A + 2*B));
  gr.add_generator(grid_line(A));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(B %= 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add NNC generators.

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  //gs.insert(closure_point(3*A, 4)); // FIX
  gs.insert(grid_point(7*A, 4));
  gs.insert(grid_line(A - B));

  Grid gr(2, EMPTY);

  for (Grid_Generator_System::const_iterator i = gs.begin(),
	 gs_end = gs.end(); i != gs_end; ++i)
    gr.add_generator(*i);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((4*A + 4*B == 7) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add generators to a grid of a higher space dimension.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4, EMPTY);
  gr.add_generator(grid_point(7*A, 3));
  gr.add_generator(grid_line(A - B));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence((3*A + 3*B == 7) / 0);
  known_gr.add_congruence((C == 0) / 0);
  known_gr.add_congruence((D == 0) / 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_generator_and_minimize

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(2*A + 2*B));
  gr.add_generator(grid_point(8*A + 8*B));

  gr.add_generator_and_minimize(grid_line(A));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((B %= 0) / 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add a generator to a universe grid.

void
test7() {
  nout << "test7:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4);
  gr.add_generator(grid_point(12*A + 7*D));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_generator_and_minimize, adding a generator with a divisor to a
// grid of many generators.

void
test8() {
  nout << "test8:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));

  // Minimize the grid.
  if (find_variation(gr))
    exit(1);

  gr.add_generator_and_minimize(grid_point(B, 3));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A %= 0);
  known_gr.add_congruence(3*B %= 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Space dimension exception.

void
test9() {
  nout << "test9:" << endl;

  Variable A(0);
  Variable C(2);

  Grid gr(2);

  try {
    gr.add_generator(grid_point(A + C));
    exit(1);
  }
  catch (std::invalid_argument) {}
}

// Zero dimensions empty.

void
test10() {
  nout << "test10:" << endl;

  Grid gr(0, EMPTY);
  gr.add_generator(grid_point());

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Zero dimension universe.

void
test11() {
  nout << "test11:" << endl;

  Grid gr(0);
  gr.add_generator(grid_point());

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Space dimension exception.

void
test12() {
  nout << "test12:" << endl;

  Variable A(0);

  Grid gr(2, EMPTY);

  try {
    gr.add_generator(grid_line(A));
    exit(1);
  }
  catch (std::invalid_argument) {}
}

// Try add parameter to empty grid.

void
test13() {
  nout << "test13:" << endl;

  Grid gr(2, EMPTY);

  try {
    gr.add_generator(parameter());
    exit(1);
  }
  catch (std::invalid_argument) {}
}

// Try add parameter to zero dimension empty grid.

void
test14() {
  nout << "test14:" << endl;

  Grid gr(0, EMPTY);

  try {
    gr.add_generator(parameter());
    exit(1);
  }
  catch (std::invalid_argument) {}
}

int
main() TRY {
  set_handlers();

  nout << "addgenerator1:" << endl;

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

  return 0;
}
CATCH
