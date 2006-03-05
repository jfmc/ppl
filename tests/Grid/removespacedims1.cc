/* Test Grid::remove_space_dimensions().
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

// Simple grid.
bool
test01() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence(A - B == 0);
  gr.add_congruence(A %= 0);
  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// Empty grid.
bool
test02() {
  Variable B(1);

  Grid gr(4, EMPTY);
  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  Grid known_gr(3, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// Universe grid.

bool
test03() {
  Variable C(2);
  Variable D(3);

  Grid gr(7, UNIVERSE);

  Variables_Set vars;
  vars.insert(C);
  vars.insert(D);

  gr.remove_space_dimensions(vars);

  Grid known_gr(5, UNIVERSE);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// From generators.
bool
test04() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point(0*A));
  gs.insert(grid_point(2*A));
  gs.insert(grid_point(3*B));

  Grid gr(gs);
  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 2);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// From congruences.
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  Variables_Set vars;
  vars.insert(B);
  vars.insert(D);

  Congruence_System cgs;
  cgs.insert((A + 2*C %= 0) / 3);
  cgs.insert((B - E %= 0) / 2);

  Grid gr(cgs);
  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  gr.remove_space_dimensions(vars);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_line(2*A - B));
  known_gs.insert(grid_point(3*B, 2));
  known_gs.insert(grid_line(C));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// Variable set includes first dimension.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence(A - B == 0);
  gr.add_congruence(A %= 0);

  Variables_Set vars;
  vars.insert(A);
  vars.insert(C);

  gr.remove_space_dimensions(vars);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr ***");

  return ok;
}

// The resulting grid contains a parameter that is all zeros.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));
  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  Grid known_gr(2);
  known_gr.add_congruence(A %= 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Empty variable set.

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));
  print_generators(gr, "*** gr ***");

  Variables_Set vars;

  Grid known_gr = gr;

  gr.remove_space_dimensions(vars);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// Space dimension exception.

bool
test09() {
  Variable B(1);

  Grid gr(1, EMPTY);
  print_congruences(gr, "*** gr ***");

  Variables_Set vars;
  vars.insert(B);

  try {
    gr.remove_space_dimensions(vars);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Zero dimension universe resulting grid.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));
  print_generators(gr, "*** gr ***");

  Variables_Set vars;
  vars.insert(A);
  vars.insert(B);
  vars.insert(C);

  gr.remove_space_dimensions(vars);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_space_dimensions(vars) ***");

  return ok;
}

// From congruences.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + 2*C %= 0) / 3);

  Grid gr(cgs);
  print_generators(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(2);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(0*B));
  known_gs.insert(grid_line(A));
  known_gs.insert(grid_line(B));

  Grid known_gr(known_gs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(2) ***");

  return ok;
}

// Empty grid.
bool
test12() {
  Grid gr(2, EMPTY);
  print_generators(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(1);

  Grid known_gr(1, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(1) ***");

  return ok;
}

// Universe grid.
bool
test13() {
  Grid gr(7);

  gr.remove_higher_space_dimensions(3);

  Grid known_gr(3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(3) ***");

  return ok;
}

// From generators.
bool
test14() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point(0*A));
  gs.insert(grid_point(2*A));
  gs.insert(grid_point(3*B));

  Grid gr(gs);
  print_generators(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 2);

  Grid known_gr(known_cgs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(1) ***");

  return ok;
}

// Resulting grid the same.

bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));
  print_generators(gr, "*** gr ***");

  Grid known_gr = gr;

  gr.remove_higher_space_dimensions(gr.space_dimension());

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions() ***");

  return ok;
}

// Space dimension exception.
bool
test16() {
  Grid gr(1, EMPTY);
  print_generators(gr, "*** gr ***");

  try {
    gr.remove_higher_space_dimensions(6);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Zero dimension universe resulting grid.
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));
  print_generators(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(0);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(0) ***");

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
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
  NEW_TEST(test13);
  NEW_TEST(test14);
  NEW_TEST(test15);
  NEW_TEST(test16);
  NEW_TEST(test17);
END_MAIN
