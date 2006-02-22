/* Test methods which can add multiple generators to a grid.
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

// grid1*.cc use add_generators_and_minimize often.

// add_recycled_generators -- space dimension exception.

bool
test01() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point(B));

  Grid gr(1);

  try {
    gr.add_recycled_generators(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// add_recycled_generators_and_minimize -- space dimension exception.

bool
test02() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point(B));

  Grid gr(1);

  try {
    gr.add_recycled_generators_and_minimize(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// add_recycled_generators -- zero dimension universe.

bool
test03() {
  Grid_Generator_System gs;
  gs.insert(grid_point());

  Grid gr(0);

  print_generators(gr, "*** gr ***");

  gr.add_recycled_generators(gs);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_recycled_generators(gs) ***");

  return ok;
}

// add_recycled_generators -- zero dimension empty.

bool
test04() {
  Grid_Generator_System gs;
  gs.insert(grid_point());

  Grid gr(0, EMPTY);

  print_generators(gr, "*** gr ***");

  gr.add_recycled_generators(gs);

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_recycled_generators(gs) ***");

  return ok;
}

// add_recycled_generators -- add system with a single parameter
// generator to the zero dimension empty grid.

bool
test05() {
  Variable A(0);

  Grid_Generator_System gs;
  gs.insert(parameter(A));

  Variables_Set vs;
  vs.insert(A);

  // This may be the only was to create a zero dimension generator
  // system that contains only parameters and/or lines.
  gs.remove_space_dimensions(vs);

  Grid gr(0, EMPTY);

  try {
    gr.add_recycled_generators(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// add_recycled_generators_and_minimize -- add system with a single
// parameter generator to the zero dimension empty grid.

bool
test06() {
  Variable A(0);

  Grid_Generator_System gs;
  gs.insert(parameter(A));

  Variables_Set vs;
  vs.insert(A);

  // This may be the only was to create a zero dimension generator
  // system that contains only parameters and/or lines.
  gs.remove_space_dimensions(vs);

  Grid gr(0, EMPTY);

  try {
    gr.add_recycled_generators_and_minimize(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// add_recycled_generators -- add system with a single parameter
// generator to the empty grid.

bool
test07() {
  Variable A(0);

  Grid_Generator_System gs;
  gs.insert(parameter(A));

  Grid gr(2, EMPTY);

  try {
    gr.add_recycled_generators(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// add_recycled_generators_and_minimize -- add to the zero dim grid.

bool
test08() {
  Grid_Generator_System gs;
  gs.insert(grid_point());

  Grid gr(0, EMPTY);
  gr.add_recycled_generators_and_minimize(gs);

  print_generators(gr, "*** gr ***");

  Grid known_gr(0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_recycled_generators_and_minimize(gs) ***");

  return ok;
}

// add_recycled_generators_and_minimize -- try add system with a
// single parameter generator to the empty grid.

bool
test09() {
  Variable A(0);

  Grid_Generator_System gs;
  gs.insert(parameter(A));

  Grid gr(2, EMPTY);

  try {
    gr.add_recycled_generators_and_minimize(gs);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// add_recycled_generators_and_minimize -- add an empty system.

bool
test10() {
  Grid_Generator_System gs;

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());

  print_generators(gr, "*** gr ***");

  Grid known_gr = gr;

  gr.add_recycled_generators_and_minimize(gs);

  bool ok = (gr == known_gr);

  print_generators(gr,
      "*** gr.add_recycled_generators_and_minimize(gs) ***");

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
END_MAIN
