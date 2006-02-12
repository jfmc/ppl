/* Test reduction and conversion of grids created from generators.
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

// add_generator_and_minimize, one variable.

void
test1() {
  Grid_Generator_System gs;
  gs.insert(grid_point(A));

  Grid gr(gs);

  gr.add_generator_and_minimize(grid_point(2*A));

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generator_and_minimize, two variables.

void
test2() {
  Grid_Generator_System gs;
  gs.insert(grid_point(A + B));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  Grid_Generator g(grid_point(A + 2*B));
  gr.add_generator_and_minimize(g);

  Congruence_System known_cgs;
  known_cgs.insert( 0*A + 0*B %= -1);
  known_cgs.insert((  A + 0*B %=  1) / 0);
  known_cgs.insert( 0*A + 1*B %=  1);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generators_and_minimize

void
test3() {
  Grid_Generator_System gs;
  gs.insert(grid_line(0*A +   B));
  gs.insert(grid_point(3*A + 4*B));
  gs.insert(grid_point(9*A + 0*B));

  Grid gr(2, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_line(0*A +   B));
  known_gs.insert(grid_point(3*A + 4*B));
  known_gs.insert(grid_point(9*A + 0*B));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test0 from Chiara conversion_test.cc

void
test4() {
  Grid_Generator_System gs;
  gs.insert(grid_point(4*A -   B + 0*C, 3));
  gs.insert(grid_line(2*A + 3*B + 0*C));
  gs.insert(grid_point(4*A + 0*B + 0*C, 3));
  gs.insert(grid_point(4*A -   B +   C, 3));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -2) / 2);
  known_cgs.insert((-9*A + 6*B + 0*C %= 14) / 2);
  known_cgs.insert(( 0*A - 0*B + 6*C %=  0) / 2);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test1 from Chiara conversion_test.cc.

void
test5() {
  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 4*B + 3*C, 2));
  gs.insert(grid_line( 3*A + 2*B - 4*C));
  gs.insert(grid_line( 0*A + 0*B - 2*C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(0*A + 0*B + 0*C %= -1);
  known_cgs.insert((-2*A + 3*B + 0*C %= 7) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test2 from Chiara conversion_test.cc.

void
test6() {
  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 4*B +  3*C, 2));
  gs.insert(grid_point( 2*A + 6*B -    C, 2));
  gs.insert(grid_point(-1*A + 9*B +  7*C, 2));
  gs.insert(grid_line( 0*A + 0*B -  2*C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((  0*A + 0*B + 0*C %=  15) / 15);
  known_cgs.insert((-10*A + 0*B + 0*C %=   5) / 15);
  known_cgs.insert((  4*A - 6*B + 0*C %= -14) / 15);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test3 from Chiara conversion_test.cc.

void
test7() {
  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 4*B + 3*C, 2));
  gs.insert(grid_line( 2*A +   B - 2*C));
  gs.insert(grid_point(-1*A + 9*B + 7*C, 2));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -10) / 10);
  known_cgs.insert((-2*A + 4*B + 0*C %=   9) / 10);
  known_cgs.insert(( 7*A - 4*B + 5*C %=  -4) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test4 from Chiara conversion_test.cc -- in grid1_16.cc.

// param_test0/1 from Chiara Convert_Test.cc.

void
test8() {
  Grid_Generator_System gs;
  gs.insert(grid_point(A));
  gs.insert(grid_point(2*A));
  gs.insert(grid_point(A + B));
  gs.insert(grid_point(A + C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert(C %= 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test2 from Chiara Convert_Test.cc.

void
test9() {
  Grid_Generator_System gs;
  gs.insert(grid_point(A +   B));
  gs.insert(grid_point(A + 2*B));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 1) / 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert((C %= 0) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test3 from Chiara Convert_Test.cc.

void
test10() {
  Grid_Generator_System gs;
  gs.insert(grid_point(A +   B + 0*C));
  gs.insert(grid_point(A + 2*B + 0*C));
  gs.insert(grid_point(A +   B +   C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %=  1) / 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert(C %= 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test4 from Chiara Convert_Test.cc -- in grid1_16.cc.

// param_test5 from Chiara Convert_Test.cc.

void
test11() {
  Grid_Generator_System gs;
  gs.insert(grid_point(0*A + 7*B + 0*C, 3));
  gs.insert(grid_line(3*A + 2*B + 0*C));
  gs.insert(grid_line(0*A + 0*B +   C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -1) / 1);
  known_cgs.insert((-2*A + 3*B + 0*C %=  7) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test6 from Chiara Convert_Test.cc.

void
test12() {
  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 0*B + 3*C, 4));
  gs.insert(grid_line( 3*A + 2*B + 0*C));
  gs.insert(grid_line( 0*A + 0*B +   C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((-4*A + 6*B + 0*C %= 1) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generators_and_minimize, with more rows than columns -- in
// grid1_16.cc.

// Empty grid, one dimension.

void
test13() {
  Grid gr(1, EMPTY);

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

// Empty grid, many dimensions.

void
test14() {
  Grid gr(112, EMPTY);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(112, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Bigger values (param_test7a from Chiara Convert_Test.cc) -- in
// grid1_64.cc.

// Even bigger values (param_test8 from Chiara Convert_Test.cc) -- in
// grid1_gmp.cc.

// Test reduce_line_with_line (param_test9 from Chiara Convert_Test.cc).

void
test15() {
  Grid_Generator_System gs;
  gs.insert(grid_point( -A + 0*B + 3*C, 4));
  gs.insert(grid_line(0*A + 2*B + 0*C));
  gs.insert(grid_line(0*A + 4*B + 0*C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((4*A + 0*B + 0*C %= -1) / 0);
  known_cgs.insert((0*A + 0*B + 4*C %=  3) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Grids from a water monitor example (from param_test10 from Chiara
// Convert_test.cc).

void
test16() {
  Grid_Generator_System gs;
  gs.insert(grid_point(   A));
  gs.insert(grid_point( 2*A +    B));
  gs.insert(grid_point(12*A + 11*B));
  gs.insert(grid_point(10*A + 12*B));
  gs.insert(grid_point( 2*A + 33*B, 2));
  gs.insert(grid_point( 4*A + 35*B, 2));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((C %= 0) / 0);
  known_cgs.insert((-2*A + 2*B %= 1) / 3);
  known_cgs.insert(( 3*A %= 0) / 3);
  known_cgs.insert(( 0*A %= 3) / 3);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test11 from Chiara Convert_Test.cc.

void
test17() {
  Grid_Generator_System gs;
  gs.insert(grid_point(4*A -   B + 0*C, 3));
  gs.insert(grid_line(2*A + 3*B));
  gs.insert(grid_point(4*A            , 3));
  gs.insert(grid_point(4*A -   B +   C, 3));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((-9*A + 6*B + 0*C %= 0) / 2);
  known_cgs.insert((             6*C %= 0) / 2);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Universe grid, one dimension.

void
test18() {
  Grid gr(1);

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

// Universe grid, many dimensions.

void
test19() {
  Grid gr(21);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(21);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Universe grid, zero dimensions.

void
test20() {
  Grid gr(0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// A generator system with only a line.

void
test21() {
  Grid_Generator_System gs;
  gs.insert(grid_line(0*A + 2*B + 0*C));

  try {
    Grid gr(gs);
  }
  catch (const std::invalid_argument& e) {
    return;
  }

  exit(1);
}

// A generator system containing a parameter.

void
test22() {
  Grid_Generator_System gs;
  gs.insert(grid_point(0*C));
  gs.insert(grid_line(A));
  gs.insert(grid_line(B));
  gs.insert(parameter(-C));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(C %= 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Assignment of universe grid, zero dimensions.

void
test23() {
  Grid gr(0, EMPTY);

  gr = Grid(0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Space dimension exception.

void
test24() {
  try {
    Grid gr(Grid::max_space_dimension() + 1);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::length_error& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "grid1:" << endl;

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
  DO_TEST(test20);
  DO_TEST(test21);
  DO_TEST(test22);
  DO_TEST(test23);
  DO_TEST(test24);

  return 0;
}
CATCH
