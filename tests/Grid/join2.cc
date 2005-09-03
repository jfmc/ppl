/* Test Grid::join_assign_and_minimize.
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

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs1;
  gs1.insert(point(A + 0*C));

  Generator_System gs2;
  gs2.insert(point(2*A + 0*C));

  Grid gr1(gs1);
  Grid gr2(gs2);

  if (find_variation(gr1) || find_variation(gr2))
    exit(1);

  gr1.join_assign_and_minimize(gr2);

  if (find_variation(gr1))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(A + 0*C));
  known_gs.insert(point(2*A));

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

// Two universe grids.

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3);
  Grid gr2(3);

  gr1.join_assign_and_minimize(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3);

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

  nout << "join2:" << endl;

  test1();
  test2();

  return 0;
}
CATCH
