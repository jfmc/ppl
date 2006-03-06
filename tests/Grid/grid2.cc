/* Test reduction and conversion of grids created from congruences.
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

// add_congruence_and_minimize, one dimension.
bool
test01() {
  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);

  Grid gr(cgs);

  Congruence cg(A %= 0);
  print_congruences(gr, "*** gr ***");
  gr.add_congruence_and_minimize(cg);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(0*A));
  known_gs.insert(grid_point(2*A));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruence_and_minimize(cg) ***");

  return ok;
}

// add_congruence_and_minimize, two dimensions.
bool
test02() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A - 3*B %= 4) / 5);

  Grid gr(cgs);

  Congruence cg((2*A - B %= 3) / 4);
  print_congruences(gr, "*** gr ***");
  gr.add_congruence_and_minimize(cg);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(0*A - 15*B, 5));
  known_gs.insert(grid_point(  A + 27*B, 5));
  known_gs.insert(grid_point(0*A + 85*B, 5));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruence_and_minimize(cg) ***");

  return ok;
}

// add_congruences_and_minimize, one dimension.
bool
test03() {
  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 3);

  Grid gr(1);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(0*A));
  known_gs.insert(grid_point(3*A));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// add_congruences_and_minimize, one dimension with factors.
bool
test04() {
  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 3);

  Grid gr(1);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(1*A));
  known_gs.insert(grid_point(4*A));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_recycled_congruences(cgs) ***");

  return ok;
}

// add_congruences_and_minimize, two dimensions.
bool
test05() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A - B %= 0) / 3);

  Grid gr(2);
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_point(3*A));
  known_gs.insert(grid_point(3*B));
  known_gs.insert(grid_line(A + B));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// cong_test0 from Chiara Convert_Test.cc.

bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(A %= 0);
  cgs.insert(A + B %= 0);
  cgs.insert(A + B + C %= 0);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_point(A));
  known_gs.insert(grid_point(B));
  known_gs.insert(grid_point(C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// cong_test1 from Chiara Convert_Test.cc.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(  -A %= 64);
  cgs.insert(-6*A +   B + 0*C %= -8);
  cgs.insert( 3*A + 2*B +   C %= -4);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_point(A));
  known_gs.insert(grid_point(B));
  known_gs.insert(grid_point(C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_recycled_congruences(cgs) ***");

  return ok;
}

// Adding a false equality (cong_test2 from Chiara Convert_Test.cc).
bool
test08() {
  Variable A(0);

  Congruence_System cgs;
  cgs.insert((0*A %= -1) / 0);
  cgs.insert((  A %= -1) / 2);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid known_gr(3, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
 }

// cong_test3 from Chiara Convert_Test.cc.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A         %= 0) / 2);
  cgs.insert((    B     %= 0) / 2);
  cgs.insert((A + B + C %= 0) / 2);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_point(2*A));
  known_gs.insert(grid_point(2*B));
  known_gs.insert(grid_point(2*C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// cong_test4 from Chiara Convert_Test.cc -- in grid4.cc.

// cong_test5 from Chiara Convert_Test.cc.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + 2*B +   C %= -2) / 5);
  cgs.insert((    3*B       %=  0) / 5);
  cgs.insert((      B       %=  0) / 5);
  cgs.insert((          3*C %= -4) / 5);
  cgs.insert((    3*B +   C %= -3) / 5);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(  A       - 3*C));
  known_gs.insert(grid_point(6*A       - 3*C));
  known_gs.insert(grid_point(  A + 5*B - 3*C));
  known_gs.insert(grid_point(  A       + 2*C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// cong_test6 from Chiara Convert_Test.cc.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((3*A           %= -2) / 5);
  cgs.insert((      B + 2*C %=  0) / 5);
  cgs.insert((    2*B + 3*C %= -3) / 5);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(-2*A -  3*B - 6*C, 3));
  known_gs.insert(grid_point( 3*A -  3*B - 6*C, 3));
  known_gs.insert(grid_point(-2*A + 12*B - 6*C, 3));
  known_gs.insert(grid_point(-2*A -  3*B + 9*C, 3));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// cong_test7 from Chiara Convert_Test.cc.
bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 1) / 0);
  cgs.insert((B %= 1) / 0);
  cgs.insert((C %= 1) / 0);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(A + B + C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// cong_test8 from Chiara Convert_Test.cc.
bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 1) / 0);
  cgs.insert(B %= 1);
  cgs.insert((C %= 1) / 0);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(A + C));
  known_gs.insert(grid_point(A + B + C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// An empty grid constructed from congruences.
bool
test14() {
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((C %= 2) / 5);
  cgs.insert((C %= 3) / 5);

  Grid gr(3);
  print_congruences(gr, "*** gr ***");
  gr.add_congruences_and_minimize(cgs);

  Grid known_gr(3, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs) ***");

  return ok;
}

// Adding a congruence system with a capacity larger than the capacity
// of the existing system.
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs1;
  cgs1.insert(B %= 2);

  Grid gr(2);
  gr.add_congruences_and_minimize(cgs1);

  gr.add_space_dimensions_and_embed(1);
  print_congruences(gr, "*** gr ***");

  // gr.con_sys is likely to be expanded within capacity.

  Congruence_System cgs2;
  cgs2.insert(C %= 2);

  // cgs2 is likely to now have more capacity than gr.con_sys does.

  gr.add_congruences_and_minimize(cgs2);

  Congruence_System cgs3;
  cgs3.insert(B %= 2);
  cgs3.insert(C %= 2);

  Grid known_gr(cgs3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruences_and_minimize(cgs2) ***");

  return ok;
}

// Zero dimension empty grid.

bool
test16() {
  Congruence_System cgs;
  cgs.insert(Congruence::zero_dim_false());

  Grid gr(cgs);

  Grid known_gr(0, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(cgs) ***");

  return ok;
}

// Zero dimension universe grid.

bool
test17() {
  Congruence_System cgs;
  cgs.insert(Congruence::zero_dim_integrality());

  Grid gr(cgs);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr(cgs) ***");

  return ok;
}

// add_recycled_congruence, add empty system to zero dimension
// universe.

bool
test18() {
  Grid gr(0);

  Congruence_System cgs;
  gr.add_recycled_congruences(cgs);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_recycled_congruences(cgs) ***");

  return ok;
}

// add_recycled_congruence, zero dimension empty.

bool
test19() {
  Grid gr(0);

  Congruence_System cgs;
  cgs.insert(Congruence::zero_dim_false());
  print_congruences(gr, "*** gr ***");

  gr.add_recycled_congruences(cgs);

  Grid known_gr(0, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_recycled_congruences(cgs) ***");

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
END_MAIN
