/* Test reduction and conversion of grids created from generators.
   Tests that require at least 16 bit coefficient integers.
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

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

// test4 from Chiara conversion_test.cc.

void
test1() {
  nout << "test1:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point( 3*A +   B + 0*C, 4));
  gs.insert(grid_point(11*A + 2*B + 0*C, 4));
  gs.insert(grid_point( 3*A + 6*B + 0*C, 4));
  gs.insert(grid_point( 3*A +   B + 2*C, 4));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A +  0*B +  0*C %= -40) / 40);
  known_cgs.insert((20*A +  0*B +  0*C %=  15) / 40);
  known_cgs.insert((-4*A + 32*B +  0*C %=   5) / 40);
  known_cgs.insert(( 0*A +  0*B + 80*C %=   0) / 40);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test4 from Chiara Convert_Test.cc.

void
test2() {
  nout << "test2:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point( 3*A +   B + 0*C, 4));
  gs.insert(grid_point(11*A + 2*B + 0*C, 4));
  gs.insert(grid_point( 3*A + 6*B + 0*C, 4));
  gs.insert(grid_point( 3*A +   B + 2*C, 4));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((20*A +  0*B        %= 15) / 40);
  known_cgs.insert((-4*A + 32*B        %=  5) / 40);
  known_cgs.insert((              80*C %=  0) / 40);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generators_and_minimize, with more rows than columns.

void
test3() {
  nout << "test3:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(3*A + 7*B - 2*C + 3*D));
  gs.insert(grid_point(0*A + 0*B +   C +   D));
  gs.insert(grid_point(3*A + 4*B + 2*C + 0*D));
  gs.insert(grid_point(3*A + 2*B +   C + 2*D));
  gs.insert(grid_point(9*A + 0*B + 4*C +   D));

  Grid gr(4, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((  9*A +   0*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-18*A +  27*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-90*A + 135*B + 27*C + 0*D %=  27) / 27);
  known_cgs.insert((-17*A +  25*B +  6*C +   D %=   7) / 27);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Example from Muller-Olm and Seidl SAS 2005 paper

void
test4() {
  nout << "test1:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(2*A + 0*B));
  gs.insert(grid_point(30*A + 36*B));
  gs.insert(grid_point(450*A + 564*B));

  Grid gr(2, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 2) / 28);
  known_cgs.insert((B %= 0) / 12);

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

  nout << "grid1_16:" << endl;

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
