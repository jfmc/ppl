/* Test Grid::remove_space_dimensions.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

// From congruences.

void
test1() {
  nout << "test1:" << endl;

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

  if (find_variation(gr))
    exit(1);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  // FIX check
  Generator_System known_gs;
  known_gs.insert(point(0*A, 2));
  known_gs.insert(point(2*A - B));
  known_gs.insert(point(3*B));
  known_gs.insert(point(2*C));

  Grid known_gr(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Empty grid.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(4, Grid::EMPTY);

  if (find_variation(gr))
    exit(1);

  Variable B(1);

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Universe grid.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(7, Grid::UNIVERSE);

  if (find_variation(gr))
    exit(1);

  Variable C(2);
  Variable D(3);

  Variables_Set vars;
  vars.insert(C);
  vars.insert(D);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(5, Grid::UNIVERSE);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// From generators.

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point(0*A));
  gs.insert(point(2*A));
  gs.insert(point(3*B));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 2);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

int
main() TRY {
  set_handlers();

  nout << "removespacedims1:" << endl;

  test1();
  test2();
  test3();
#if 0
  test4();
#endif

  return 0;
}
CATCH
