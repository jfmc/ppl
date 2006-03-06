/* Test reduction and conversion of grids created from generators.
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

namespace {

// add_generator_and_minimize, one variable.
bool
test01() {
  Variable A(0);

  Grid_Generator_System gs;
  gs.insert(grid_point(A));

  Grid gr(gs);
  print_generators(gr, "*** gr ***");

  gr.add_generator_and_minimize(grid_point(2*A));

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr,
     "*** gr.add_generator_and_minimize(grid_point(2*A)) ***");

  return ok;
}

// add_generator_and_minimize, two variables.
bool
test02() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point(A + B));

  Grid gr(gs);
  print_generators(gr, "*** gr ***");

  Grid_Generator g(grid_point(A + 2*B));
  gr.add_generator_and_minimize(g);

  Congruence_System known_cgs;
  known_cgs.insert( 0*A + 0*B %= -1);
  known_cgs.insert((  A + 0*B %=  1) / 0);
  known_cgs.insert( 0*A + 1*B %=  1);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr,
     "*** gr.add_generator_and_minimize(g) ***");

  return ok;
}

// add_generators_and_minimize
bool
test03() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_line(0*A +   B));
  gs.insert(grid_point(3*A + 4*B));
  gs.insert(grid_point(9*A + 0*B));

  Grid gr(2, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_line(0*A +   B));
  known_gs.insert(grid_point(3*A + 4*B));
  known_gs.insert(grid_point(9*A + 0*B));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr,
     "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// test from Chiara conversion_test.cc
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(4*A -   B + 0*C, 3));
  gs.insert(grid_line(2*A + 3*B + 0*C));
  gs.insert(grid_point(4*A + 0*B + 0*C, 3));
  gs.insert(grid_point(4*A -   B +   C, 3));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -2) / 2);
  known_cgs.insert((-9*A + 6*B + 0*C %= 14) / 2);
  known_cgs.insert(( 0*A - 0*B + 6*C %=  0) / 2);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr,
     "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// test1 from Chiara conversion_test.cc.
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 4*B + 3*C, 2));
  gs.insert(grid_line( 3*A + 2*B - 4*C));
  gs.insert(grid_line( 0*A + 0*B - 2*C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert(0*A + 0*B + 0*C %= -1);
  known_cgs.insert((-2*A + 3*B + 0*C %= 7) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr,
     "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// test2 from Chiara conversion_test.cc.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 4*B +  3*C, 2));
  gs.insert(grid_point( 2*A + 6*B -    C, 2));
  gs.insert(grid_point(-1*A + 9*B +  7*C, 2));
  gs.insert(grid_line( 0*A + 0*B -  2*C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((  0*A + 0*B + 0*C %=  15) / 15);
  known_cgs.insert((-10*A + 0*B + 0*C %=   5) / 15);
  known_cgs.insert((  4*A - 6*B + 0*C %= -14) / 15);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr,
     "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// test3 from Chiara conversion_test.cc.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 4*B + 3*C, 2));
  gs.insert(grid_line( 2*A +   B - 2*C));
  gs.insert(grid_point(-1*A + 9*B + 7*C, 2));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -10) / 10);
  known_cgs.insert((-2*A + 4*B + 0*C %=   9) / 10);
  known_cgs.insert(( 7*A - 4*B + 5*C %=  -4) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test1 from Chiara Convert_Test.cc.
bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(A));
  gs.insert(grid_point(2*A));
  gs.insert(grid_point(A + B));
  gs.insert(grid_point(A + C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert(C %= 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test2 from Chiara Convert_Test.cc.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(A +   B));
  gs.insert(grid_point(A + 2*B));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 1) / 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert((C %= 0) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test3 from Chiara Convert_Test.cc.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(A +   B + 0*C));
  gs.insert(grid_point(A + 2*B + 0*C));
  gs.insert(grid_point(A +   B +   C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((A %=  1) / 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert(C %= 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test5 from Chiara Convert_Test.cc.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(0*A + 7*B + 0*C, 3));
  gs.insert(grid_line(3*A + 2*B + 0*C));
  gs.insert(grid_line(0*A + 0*B +   C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -1) / 1);
  known_cgs.insert((-2*A + 3*B + 0*C %=  7) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test6 from Chiara Convert_Test.cc.
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-1*A + 0*B + 3*C, 4));
  gs.insert(grid_line( 3*A + 2*B + 0*C));
  gs.insert(grid_line( 0*A + 0*B +   C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((-4*A + 6*B + 0*C %= 1) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// Empty grid, one dimension.
bool
test13() {
  Grid gr(1, EMPTY);

  Grid known_gr(1, EMPTY);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr(1, EMPTY) ***");

  return ok;
}

// Empty grid, many dimensions.
bool
test14() {
  Grid gr(112, EMPTY);

  Grid known_gr(112, EMPTY);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr(112, EMPTY) ***");

  return ok;
}

// Bigger values (param_test7a from Chiara Convert_Test.cc) -- in
// grid1_64.cc.
// Test reduce_line_with_line (param_test9 from Chiara Convert_Test.cc).
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point( -A + 0*B + 3*C, 4));
  gs.insert(grid_line(0*A + 2*B + 0*C));
  gs.insert(grid_line(0*A + 4*B + 0*C));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((4*A + 0*B + 0*C %= -1) / 0);
  known_cgs.insert((0*A + 0*B + 4*C %=  3) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// Grids from a water monitor example (from param_test10 from Chiara
// Convert_test.cc).
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(   A));
  gs.insert(grid_point( 2*A +    B));
  gs.insert(grid_point(12*A + 11*B));
  gs.insert(grid_point(10*A + 12*B));
  gs.insert(grid_point( 2*A + 33*B, 2));
  gs.insert(grid_point( 4*A + 35*B, 2));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((C %= 0) / 0);
  known_cgs.insert((-2*A + 2*B %= 1) / 3);
  known_cgs.insert(( 3*A %= 0) / 3);
  known_cgs.insert(( 0*A %= 3) / 3);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test11 from Chiara Convert_Test.cc.
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(4*A -   B + 0*C, 3));
  gs.insert(grid_line(2*A + 3*B));
  gs.insert(grid_point(4*A            , 3));
  gs.insert(grid_point(4*A -   B +   C, 3));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((-9*A + 6*B + 0*C %= 0) / 2);
  known_cgs.insert((             6*C %= 0) / 2);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// Universe grid, one dimension.
bool
test18() {
  Grid gr(1);

  Grid known_gr(1);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr(1) ***");

  return ok;
}

// Universe grid, many dimensions.
bool
test19() {
  Grid gr(21);

  Grid known_gr(21);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(21) ***");

  return ok;
}

// Universe grid, zero dimensions.
bool
test20() {
  Grid gr(0);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr(0) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
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
END_MAIN
