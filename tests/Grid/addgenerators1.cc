/* Test methods which can add multiple generators to a grid.
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

// grid1*.cc use add_generators_and_minimize often.

// add_recycled_generators -- space dimension exception.

void
test1() {
  nout << "test1:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(B));

  Grid gr(1);

  try {
    gr.add_recycled_generators(gs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_generators_and_minimize -- space dimension exception.

void
test2() {
  nout << "test2:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(B));

  Grid gr(1);

  try {
    gr.add_recycled_generators_and_minimize(gs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_generators -- zero dimension universe.

void
test3() {
  nout << "test3:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point());

  Grid gr(0);
  gr.add_recycled_generators(gs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_generators -- zero dimension empty.

void
test4() {
  nout << "test4:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point());

  Grid gr(0, EMPTY);
  gr.add_recycled_generators(gs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_generators -- add system with a single parameter
// generator to the zero dimension empty grid.

void
test5() {
  nout << "test5:" << endl;

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
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_generators_and_minimize -- add system with a single
// parameter generator to the zero dimension empty grid.

void
test6() {
  nout << "test6:" << endl;

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
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_generators -- add system with a single parameter
// generator to the empty grid.

void
test7() {
  nout << "test7:" << endl;

  Grid_Generator_System gs;
  gs.insert(parameter(A));

  Grid gr(2, EMPTY);

  try {
    gr.add_recycled_generators(gs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_generators_and_minimize -- add to the zero dim grid.

void
test8() {
  nout << "test8:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point());

  Grid gr(0, EMPTY);
  gr.add_recycled_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_generators_and_minimize -- try add system with a
// single parameter generator to the empty grid.

void
test9() {
  nout << "test9:" << endl;

  Grid_Generator_System gs;
  gs.insert(parameter(A));

  Grid gr(2, EMPTY);

  try {
    gr.add_recycled_generators_and_minimize(gs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_generators_and_minimize -- add an empty system.

void
test10() {
  nout << "test10:" << endl;

  Grid_Generator_System gs;

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());

  Grid known_gr = gr;

  gr.add_recycled_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "addgenerators1:" << endl;

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

  return 0;
}
CATCH
