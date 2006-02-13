/* Test Grid::topological_closure_assign().
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Empty.

void
test1() {
  Grid gr(6, EMPTY);

  Grid known_gr = gr;

  gr.topological_closure_assign();

  if (gr == known_gr)
    return;

  exit(1);
}

// Universe

void
test2() {
  Grid gr(5);

  Grid known_gr = gr;

  gr.topological_closure_assign();

  if (gr == known_gr)
    return;

  exit(1);
}

void
test3() {
  Grid_Generator_System gs;
  gs.insert(grid_point(0*C));
  gs.insert(grid_line(A));
  gs.insert(grid_line(B));
  gs.insert(parameter(-C));

  Grid gr(gs);

  Grid known_gr = gr;

  gr.topological_closure_assign();

  if (gr == known_gr)
    return;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
}
CATCH
