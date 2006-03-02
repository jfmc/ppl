/* Test Grid::join_assign() (a.k.a. Grid::upper_bound_assign()).
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

// Two grids each with one point and two lines
bool
test01() {
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs1;
  gs1.insert(grid_point(C));

  Grid_Generator_System gs2;
  gs2.insert(grid_point(B + 0*C));

  Grid gr1(gs1);
  print_generators(gr1, "*** gr1 ***");
  Grid gr2(gs2);
  print_generators(gr2, "*** gr2 ***");

  gr1.join_assign(gr2);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(C));
  known_gs.insert(grid_point(B));

  Grid known_gr(known_gs);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.join_assign(gr2) ***");

  return ok;
}

// Two universe grids.
bool
test02() {
  Grid gr1(3);
  print_generators(gr1, "*** gr1 ***");
  Grid gr2(3);
  print_generators(gr2, "*** gr2 ***");

  gr1.upper_bound_assign(gr2);

  Grid known_gr(3);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.upper_bound_assign(gr2) ***");

  return ok;
}

// Second grid universe.
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3);
  print_generators(gr1, "*** gr1 ***");

  Grid_Generator_System gs;
  gs.insert(grid_point());
  gs.insert(grid_line(A));
  gs.insert(grid_line(B));
  gs.insert(grid_line(-C));

  Grid gr2(gs);
  print_generators(gr2, "*** gr2 ***");

  gr1.join_assign(gr2);

  Grid known_gr(3);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.join_assign(gr2) ***");

  return ok;
}

// Inserting a parameter.
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs1;
  gs1.insert(grid_point(0*C));
  gs1.insert(grid_line(A));
  gs1.insert(grid_line(B));

  Grid gr1(gs1);

  gr1.add_generator(parameter(-C));
  print_generators(gr1, "*** gr1 ***");

  Grid_Generator_System gs2;
  gs2.insert(grid_point(0*C));

  Grid gr2(gs2);
  print_generators(gr2, "*** gr2 ***");

  gr1.upper_bound_assign(gr2);

  Grid known_gr(3);
  known_gr.add_congruence(C %= 0);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.join_assign(gr2) ***");

  return ok;
}

// join_assign_and_minimize - Divisor normalization.
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs1;
  gs1.insert(grid_point(0*C));
  gs1.insert(grid_line(A));
  gs1.insert(grid_line(B));

  Grid gr1(gs1);
  print_generators(gr1, "*** gr1 ***");

  Grid_Generator_System gs2;
  gs2.insert(grid_point());
  gs2.insert(grid_point(C, 3));

  Grid gr2(gs2);
  print_generators(gr2, "*** gr2 ***");

  gr1.join_assign_and_minimize(gr2);

  Congruence_System known_cgs;
  known_cgs.insert((3*C %= 0) / 1);

  Grid known_gr(known_cgs);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.join_assign_and_minimize(gr2) ***");

  return ok;
}

// Out-of-date generators in the first grid.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(B == 0);
  gr1.add_congruence(C == 0);
  print_generators(gr1, "*** gr1 ***");

  Grid_Generator_System gs2;
  gs2.insert(grid_point(B + 0*C));

  Grid gr2(gs2);
  print_generators(gr2, "*** gr2 ***");

  gr1.join_assign(gr2);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_point(B + 0*C));

  Grid known_gr(known_gs);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.join_assign(gr2) ***");

  return ok;
}

// Out-of-date generators in the second grid.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(B + 0*C));

  Grid gr1(gs);
  print_generators(gr1, "*** gr1 ***");

  Grid gr2(3);
  gr2.add_congruence(A == 0);
  gr2.add_congruence(B == 0);
  gr2.add_congruence(C == 0);
  print_congruences(gr2, "*** gr2 ***");

  gr1.upper_bound_assign(gr2);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_point(B + 0*C));

  Grid known_gr(known_gs);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.upper_bound_assign(gr2) ***");

  return ok;
}

// Space dimension exception.
bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(B + 0*C));

  Grid gr1(gs);

  Grid gr2(4);
  gr2.add_congruence(A == 0);
  gr2.add_congruence(B == 0);
  gr2.add_congruence(C == 0);

  try {
    gr1.upper_bound_assign(gr2);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Out-of-date generators in the first grid, which is empty.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(A == 1);
  print_congruences(gr1, "*** gr1 ***");

  Grid_Generator_System gs2;
  gs2.insert(grid_point(B + 0*C));

  Grid gr2(gs2);
  print_generators(gr2, "*** gr2 ***");

  Grid known_gr = gr2;

  gr1.join_assign(gr2);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.join_assign(gr2) ***");

  return ok;
}

// Based on an example in a paper by Muller-Olm and Seidl in SAS 2005
// Here there is an input and output version of each variable
// A, B being input and A1, B1 the output.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr0(4); // initial point
  gr0.add_congruence(A == 2);
  gr0.add_congruence(B == 0);

  Grid gr1(4); // a pass through the procedure may do nothing
  gr1.add_congruence(A == C);
  gr1.add_congruence(B == D);

  gr1.intersection_assign(gr0); // add the inital point
  print_congruences(gr1, "*** gr1 ***");

  Grid gr2(4); // one non-trivial pass through procedure
  gr2.add_congruence(15 * A == C);
  gr2.add_congruence(18 * A + B == D);

  gr2.intersection_assign(gr0); // add the inital point
  print_congruences(gr2, "*** gr2 ***");

  Grid gr3(4); // two non-trivial passes through procedure
  gr3.add_congruence(225 * A == C);
  gr3.add_congruence(282 * A + B == D);

  gr3.intersection_assign(gr0); // add the inital point
  print_congruences(gr3, "*** gr3 ***");

  gr1.join_assign(gr2); // combine alternative paths 1 and 2
  print_generators(gr1, "*** gr1.join_assign(gr2) ***");
  gr1.join_assign(gr3); // combine alternative paths 1, 2 and 3
  print_generators(gr1, "*** gr1.join_assign(gr3) ***");

  Variables_Set vars;
  vars.insert(A);
  vars.insert(B);

  gr1.remove_space_dimensions(vars);

  Grid known_gr(2); // as in paper

  known_gr.add_congruence((A %= 2) / 28);
  known_gr.add_congruence((B %= 0) / 12);

  bool ok = (gr1 == known_gr);

  print_congruences(gr1, "*** gr1.remove_space_dimensions(vars) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
  NEW_TEST(test09);
  NEW_TEST_F8(test10);
END_MAIN
