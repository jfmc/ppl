/* Test reduction and conversion of grids created from congruences.
   Tests that fail with checked-int8.
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

// cong_test4 from Chiara Convert_Test.cc.

void
test1() {
  nout << "test1:" << endl;

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

  if (find_variation(gr))
    exit(1);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(-2*A + 0*B +  7*C, 3));
  known_gs.insert(grid_point( 1*A + 0*B +    C, 3));
  known_gs.insert(grid_point(-2*A + 9*B +  7*C, 3));
  known_gs.insert(grid_point(-2*A + 0*B + 16*C, 3));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

int
main() TRY {
  set_handlers();

  nout << "grid4:" << endl;

  test1();

  return 0;
}
CATCH
