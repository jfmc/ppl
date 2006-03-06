/* Test construction of grids from constraints.
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

// Grid(cs)
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(cs);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(cs) ***");

  return ok;
}

// Grid(cs), resulting grid empty.
bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(cs);

  Grid known_gr(3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(cs) ***");

  return ok;
}

// Grid(const cs)
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(2*B == A);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  const Constraint_System ccs = cs;

  Grid gr(ccs);

  Grid known_gr(3);
  known_gr.add_congruence(2*B == A);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(ccs) ***");

  return ok;
}

// Grid(const cs), resulting grid empty.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(2*B >= A);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  const Constraint_System ccs = cs;

  Grid gr(ccs);

  Grid known_gr(3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(ccs) ***");

  return ok;
}

// Space dimension exception.
bool
test05() {
  try {
    Grid gr(Constraint_System::max_space_dimension() + 1);
  }
  catch (const std::length_error& e) {
    nout << "length_error: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Even bigger values (param_test8 from Chiara Convert_Test.cc).
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-9933*A + 2000*B + 3953*C, 9113));
  gs.insert(grid_point(    0*A +    0*B + 8888*C, 7302));
  gs.insert(grid_point(   29*A +   23*B + 1111*C, 1010));
  gs.insert(grid_point( 2394*A + 7273*B +    0*C,   30));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;

  // Create coefficients with string constructors as they're too big
  // for the long type.

  // 37315344498526  0  0  0  congruence, modulus = 37315344498526
  // 0  343455281759218112380  0  0  congruence, modulus = 37315344498526
  // 0  -133815138923073144612  223892066991156  0  congruence, modulus = 37315344498526
  // -22220  -31385495955559489171  93798931757298  18255  congruence, modulus = 37315344498526

  Coefficient* tem1 = new Coefficient("37315344498526");
  known_cgs.insert((     0*A +     0*B +     0*C %= -*tem1) / *tem1);

  Coefficient* tem2 = new Coefficient("343455281759218112380");
  known_cgs.insert(( *tem2*A +     0*B +     0*C %= 0) / *tem1);
  delete tem2;

  tem2 = new Coefficient("-133815138923073144612");
  Coefficient* tem3 = new Coefficient("223892066991156");
  known_cgs.insert(( *tem2*A + *tem3*B +     0*C %= 0) / *tem1);
  delete tem2; delete tem3;

  tem2 = new Coefficient("-31385495955559489171");
  tem3 = new Coefficient("93798931757298");
  known_cgs.insert(( *tem2*A + *tem3*B + 18255*C %= 22220) / *tem1);
  delete tem1; delete tem2; delete tem3;


  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  //  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// Bigger values (param_test7a from Chiara Convert_Test.cc).
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-93*A +   0*B +  39*C, 113));
  gs.insert(grid_line( 29*A +  23*B + 111*C));
  gs.insert(grid_point(117*A + 200*B +  88*C, 33));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((       0*A +       0*B +      0*C %=  280730) / 280730);
  known_cgs.insert((  -85767*A +  108141*B +      0*C %=   70587) / 280730);
  known_cgs.insert((-2309489*A + 1557137*B + 280730*C %= 1997619) / 0);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// test4 from Chiara conversion_test.cc.
bool
test08 () {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid_Generator_System gs;
  gs.insert(grid_point( 3*A +   B + 0*C, 4));
  gs.insert(grid_point(11*A + 2*B + 0*C, 4));
  gs.insert(grid_point( 3*A + 6*B + 0*C, 4));
  gs.insert(grid_point( 3*A +   B + 2*C, 4));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A +  0*B +  0*C %= -40) / 40);
  known_cgs.insert((20*A +  0*B +  0*C %=  15) / 40);
  known_cgs.insert((-4*A + 32*B +  0*C %=   5) / 40);
  known_cgs.insert(( 0*A +  0*B + 80*C %=   0) / 40);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// param_test4 from Chiara Convert_Test.cc.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid_Generator_System gs;
  gs.insert(grid_point( 3*A +   B + 0*C, 4));
  gs.insert(grid_point(11*A + 2*B + 0*C, 4));
  gs.insert(grid_point( 3*A + 6*B + 0*C, 4));
  gs.insert(grid_point( 3*A +   B + 2*C, 4));

  Grid gr(3, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((20*A +  0*B        %= 15) / 40);
  known_cgs.insert((-4*A + 32*B        %=  5) / 40);
  known_cgs.insert((              80*C %=  0) / 40);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// add_generators_and_minimize, with more rows than columns.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid_Generator_System gs;
  gs.insert(grid_point(3*A + 7*B - 2*C + 3*D));
  gs.insert(grid_point(0*A + 0*B +   C +   D));
  gs.insert(grid_point(3*A + 4*B + 2*C + 0*D));
  gs.insert(grid_point(3*A + 2*B +   C + 2*D));
  gs.insert(grid_point(9*A + 0*B + 4*C +   D));

  Grid gr(4, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((  9*A +   0*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-18*A +  27*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-90*A + 135*B + 27*C + 0*D %=  27) / 27);
  known_cgs.insert((-17*A +  25*B +  6*C +   D %=   7) / 27);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// Example from Muller-Olm and Seidl SAS 2005 paper
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid_Generator_System gs;
  gs.insert(grid_point(2*A + 0*B));
  gs.insert(grid_point(30*A + 36*B));
  gs.insert(grid_point(450*A + 564*B));

  Grid gr(2, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.add_generators_and_minimize(gs);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 2) / 28);
  known_cgs.insert((B %= 0) / 12);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_generators_and_minimize(gs) ***");

  return ok;
}

// A generator system with only a line.
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_line(0*A + 2*B + 0*C));

  try {
    Grid gr(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// A generator system containing a parameter.
bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(0*C));
  gs.insert(grid_line(A));
  gs.insert(grid_line(B));
  gs.insert(parameter(-C));

  Grid gr(gs);

  Grid known_gr(3);
  known_gr.add_congruence(C %= 0);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr(gs) ***");

  return ok;
}

// Assignment of universe grid, zero dimensions.
bool
test14() {
  Grid gr(0, EMPTY);

  gr = Grid(0);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr ***");

  return ok;
}

// Space dimension exception.
bool
test15() {
  try {
    Grid gr(Grid::max_space_dimension() + 1);
  }
  catch (const std::length_error& e) {
    nout << "length_error: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// cong_test4 from Chiara Convert_Test.cc.
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((3*A             %= -2) / 3);
  cgs.insert((5*A + 9*B +   C %= -1) / 3);
  cgs.insert((        B + 3*C %= -2) / 3);
  cgs.insert((      2*B + 3*C %= -2) / 3);

  Grid gr(3);

  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(-2*A + 0*B +  7*C, 3));
  known_gs.insert(grid_point( 1*A + 0*B +    C, 3));
  known_gs.insert(grid_point(-2*A + 9*B +  7*C, 3));
  known_gs.insert(grid_point(-2*A + 0*B + 16*C, 3));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_generators(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST_F64(test06);
  DO_TEST_F32(test07);
  DO_TEST_F8(test08);
  DO_TEST_F8(test09);
  DO_TEST_F8(test10);
  DO_TEST_F8(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST_F8(test16);
END_MAIN
