/* Test reduction and conversion of grids created from generators.
   Tests expected to pass only with 64 bit or GMP integers.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

// Bigger values (param_test7a from Chiara Convert_Test.cc).

void
test1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-93*A +   0*B +  39*C, 113));
  gs.insert(grid_line( 29*A +  23*B + 111*C));
  gs.insert(grid_point(117*A + 200*B +  88*C, 33));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((       0*A +       0*B +      0*C %=  280730) / 280730);
  known_cgs.insert((  -85767*A +  108141*B +      0*C %=   70587) / 280730);
  known_cgs.insert((-2309489*A + 1557137*B + 280730*C %= 1997619) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

int
main() TRY {
  set_handlers();

  nout << "grid1_64:" << endl;

  DO_TEST(test1);

  return 0;
}
CATCH
