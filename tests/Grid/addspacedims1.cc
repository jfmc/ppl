/* Test Grid::add_space_dimensions_and_emdeb.
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

// From congruences, adding to both congruences and generators.

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);
  Variable C(2);
  Variable E(4);

  Congruence_System cgs;
  cgs.insert((A + 0*C %= 0) / 2);

  Grid gr(cgs);

  if (find_variation(gr))
    exit(1);

  gr.add_space_dimensions_and_embed(2);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A + 0*E %= 0) / 2);

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

// An empty grid.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(2, Grid::EMPTY);

  if (find_variation(gr))
    exit(1);

  gr.add_space_dimensions_and_embed(3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(5, Grid::EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// A universe grid.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(1);

  if (find_variation(gr))
    exit(1);

  gr.add_space_dimensions_and_embed(4);

  if (find_variation(gr))
    exit(1);

  if (gr.is_universe() == false) {
    nout << "Grid should be universe." << endl;
    exit(1);
  }

  Grid known_gr(5);

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
  Variable C(2);
  Variable E(4);

  Generator_System gs;
  gs.insert(point(A));
  gs.insert(point(A + C));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  gr.add_space_dimensions_and_embed(2);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A == 1) / 0);
  known_cgs.insert((C + 0*E %= 0) / 1);
  known_cgs.insert((B == 0) / 0);

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

// From congruences, where dimensions are only added to the grid's
// congruence system.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable C(2);
  Variable E(4);

  Congruence_System cgs;
  cgs.insert((A + 0*C %= 0) / 2);

  Grid gr(cgs);

  // Add space dimensions directly after creating the grid, to ensure
  // that only the congruences are up to date.

  gr.add_space_dimensions_and_embed(2);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A + 0*E %= 0) / 2);

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

  nout << "addspacedims1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
CATCH
