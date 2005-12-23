/* Test Grid::is_universe().
   Copyright (C) 2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
Variable D(3);
Variable E(4);

// One dimension.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(1);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Many dimensions.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(6);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Zero dimension universe.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(0);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Zero dimension empty.

void
test4() {
  nout << "test4:" << endl;

  Grid gr(0);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Empty grid.

void
test5() {
  nout << "test5:" << endl;

  Grid gr(2, EMPTY);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

// Grid of congruences.

void
test6() {
  nout << "test6:" << endl;

  Congruence_System cgs;
  cgs.insert((A + B + C %= 0) / 3);

  Grid gr(cgs);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

// Universe grid of congruences.

void
test7() {
  nout << "test7:" << endl;

  Congruence_System cgs;
  cgs.insert((0*C %= 6) / 3);

  Grid gr(cgs);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Grid of generators.

void
test8() {
  nout << "test8:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(A + 3*E));

  Grid gr(gs);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

// Universe grid of generators.

void
test9() {
  nout << "test9:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(A + 3*E));
  gs.insert(grid_line(A));
  gs.insert(grid_line(B));
  gs.insert(grid_line(C));
  gs.insert(grid_line(D));
  gs.insert(grid_line(E));

  Grid gr(gs);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Empty grid of generators.

void
test10() {
  nout << "test10:" << endl;

  Grid_Generator_System gs;

  Grid gr(4);
  gr.add_generators(gs);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Minimized congruences.

void
test11() {
  nout << "test11:" << endl;

  Congruence_System cgs;
  cgs.insert((A + B + C %= 0) / 3);

  Grid gr(cgs);

  // Minimize the congruences.
  if (find_variation(gr))
    exit(1);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

// Minimized universe congruences.

void
test12() {
  nout << "test12:" << endl;

  Congruence_System cgs;
  cgs.insert((0*C %= 3) / 3);

  Grid gr(cgs);

  // Minimize the congruences.
  if (find_variation(gr))
    exit(1);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

// Minimized universe congruences.

void
test13() {
  nout << "test13:" << endl;

  Congruence_System cgs;
  cgs.insert((0*C %= 4) / 2);

  Grid gr(cgs);

  // Minimize the congruences.
  if (find_variation(gr))
    exit(1);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

void
test14() {
  nout << "test14:" << endl;

  Congruence_System cgs;
  cgs.insert((A + 0*C %= 4) / 2);

  Grid gr(cgs);

  Variables_Set vars;
  vars.insert(A);

  gr.remove_space_dimensions(vars);

  // Minimize the congruences.
  if (find_variation(gr))
    exit(1);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

void
test15() {
  nout << "test15:" << endl;

  Congruence_System cgs;
  cgs.insert(0*C == 0);

  Grid gr(cgs);

  // Minimize the congruences.
  if (find_variation(gr))
    exit(1);

  if (gr.is_universe())
    return;

  nout << "Grid should be universe." << endl
       << "grid:" << endl << gr << endl;
  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "isuniverse1:" << endl;

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
  test15();

  return 0;
}
CATCH
