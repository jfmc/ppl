/* Test Grid::concatenate_assign().
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

// From congruences.

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);

  Grid gr1(cgs);

  cgs.clear();
  cgs.insert((A %= 1) / 2);

  Grid gr2(cgs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Variable B(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 2);
  known_cgs.insert((B %= 1) / 2);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// One of the grids empty.

void
test2() {
  nout << "test2:" << endl;

  Grid gr1(2, EMPTY);

  Variable A(0);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + 0*C %= 0) / 2);

  Grid gr2(cgs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(5, EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid a universe.

void
test3() {
  nout << "test3:" << endl;

  Grid gr1(1, UNIVERSE);

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(A));
  gs.insert(point(A + C));

  Grid gr2(gs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Variable D(3);

  Generator_System known_gs;
  known_gs.insert(point(B));
  known_gs.insert(point(B + D));
  known_gs.insert( line(A));

  Grid known_gr(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
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

  Generator_System gs;
  gs.insert(point(A));
  gs.insert(point(A + C));

  Grid gr1(gs);

  gs.clear();
  gs.insert(point(0*B));
  gs.insert(point(B));

  Grid gr2(gs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Variable D(3);
  Variable E(4);

  Congruence_System known_cgs;
  known_cgs.insert((A == 1) / 0);
  known_cgs.insert((C %= 0) / 1);
  known_cgs.insert((B == 0) / 0);
  known_cgs.insert((D == 0) / 0);
  known_cgs.insert((E %= 0) / 1);

  Grid known_gr(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

int
main() TRY {
  set_handlers();

  nout << "concatenate1:" << endl;

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
