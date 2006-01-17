/* Test Grid::time_elapse_assign().
   Tests that are expected to fail with checked-int8.
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

// Multi-dimension grids with denominators.

void
test1() {
  nout << "test1:" << endl;

  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(grid_point(A + 2*B - 3*C, 3));

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(3*A - B + 4*C, 7));

  gr1.time_elapse_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(A + 2*B - 3*C, 3));
  known_gr.add_generator(grid_point(3*A - B + 4*C, 7));

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr1, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "timeelapse2:" << endl;

  test1();

  return 0;
}
CATCH
