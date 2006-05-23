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

// Testing remove_higher_space_dimensions

// From congruences.
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + 2*C %= 0) / 3);

  Grid gr(cgs);
  print_congruences(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(2);

  Grid_Generator_System known_ggs;
  known_ggs.insert(grid_point(0*B));
  known_ggs.insert(grid_line(A));
  known_ggs.insert(grid_line(B));

  Grid known_gr(known_ggs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(2) ***");

  return ok;
}

// Empty grid.
bool
test02() {
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
test03() {
  Grid gr(7);
  print_generators(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(3);

  Grid known_gr(3);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(3) ***");

  return ok;
}

// From generators.
bool
test04() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System ggs;
  ggs.insert(grid_point(0*A));
  ggs.insert(grid_point(2*A));
  ggs.insert(grid_point(3*B));

  Grid gr(ggs);
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
test05() {
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

// Zero dimension universe resulting grid.
bool
test06() {
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

#if 0
// Grid_Generator_System::remove_higher_space_dimensions is now private.

// Remove all space dimensions from a nonempty generator system.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System ggs;
  ggs.insert(grid_point());
  ggs.insert(grid_point(A));
  ggs.insert(grid_point(B));
  ggs.insert(grid_line(C));
  print_generators(ggs, "*** ggs ***");

  ggs.remove_higher_space_dimensions(0);
  print_generators(ggs, "*** ggs.remove_higher_space_dimensions(0) ***");

  Grid gr(ggs);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(0) ***");

  return ok;
}

// Remove all space dimensions from an empty generator system.
// Showed a bug in remove_higher_space_dimensions() which is now corrected.
bool
test08() {
  Grid_Generator_System ggs;
  print_generators(ggs, "*** ggs ***");

  ggs.remove_higher_space_dimensions(0);
  print_generators(ggs, "*** ggs.remove_higher_space_dimensions(0) ***");

  Grid gr(ggs);

  Grid known_gr(0, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(0) ***");

  return ok;
}
#endif

// Space dimension exception.
bool
test09() {
  Grid gr(1, EMPTY);
  print_generators(gr, "*** gr ***");

  try {
    gr.remove_higher_space_dimensions(6);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// From congruences.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence(B - C == 0);
  gr.add_congruence(B %= 0);
  gr.add_congruence(A == 4);
  print_congruences(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(2);

  Grid_Generator_System known_ggs;
  known_ggs.insert(grid_point(4*A));
  known_ggs.insert(parameter(B));

  Grid known_gr(known_ggs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(2) ***");

  return ok;
}

// From congruences.
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence(B - C == 0);
  gr.add_congruence(B %= 0);
  gr.add_congruence(A == 4);
  print_congruences(gr, "*** gr ***");

  gr.remove_higher_space_dimensions(1);

  Grid_Generator_System known_ggs;
  known_ggs.insert(grid_point(4*A));

  Grid known_gr(known_ggs);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.remove_higher_space_dimensions(2) ***");

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
  //DO_TEST(test07);
  //DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
  DO_TEST(test11);
END_MAIN
