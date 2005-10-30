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

#define find_variation find_variation_template<Grid>

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

// Zero dimensions.

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

// Empty grid.

void
test4() {
  nout << "test4:" << endl;

  Grid gr(2, EMPTY);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

// Grid of congruences.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + B + C %= 0) / 3);

  Grid gr(cgs);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

// Grid of generators.

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable E(2);

  Generator_System gs;
  gs.insert(point(A + 3*E));

  Grid gr(gs);

  if (gr.is_universe()) {
    nout << "Grid::is_universe should return false." << endl
	 << "grid:" << endl << gr << endl;
    exit(1);
  }
}

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

  return 0;
}
CATCH
