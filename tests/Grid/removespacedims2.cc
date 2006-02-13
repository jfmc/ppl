/* Test Grid::remove_higher_space_dimensions().
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// From congruences.

void
test1() {
  Congruence_System cgs;
  cgs.insert((A + 2*C %= 0) / 3);

  Grid gr(cgs);

  if (find_variation(gr))
    exit(1);

  gr.remove_higher_space_dimensions(2);

  if (find_variation(gr))
    exit(1);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(0*B));
  known_gs.insert(grid_line(A));
  known_gs.insert(grid_line(B));

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
  Grid gr(2, EMPTY);

  if (find_variation(gr))
    exit(1);

  gr.remove_higher_space_dimensions(1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1, EMPTY);

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
  Grid gr(7);

  if (find_variation(gr))
    exit(1);

  gr.remove_higher_space_dimensions(3);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);

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
  Grid_Generator_System gs;
  gs.insert(grid_point(0*A));
  gs.insert(grid_point(2*A));
  gs.insert(grid_point(3*B));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  gr.remove_higher_space_dimensions(1);

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

// Resulting grid the same.

void
test5() {
  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));

  Grid known_gr = gr;

  gr.remove_higher_space_dimensions(gr.space_dimension());

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Space dimension exception.

void
test6() {
  Grid gr(1, EMPTY);

  try {
    gr.remove_higher_space_dimensions(6);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Zero dimension universe resulting grid.

void
test7() {
  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));

  gr.remove_higher_space_dimensions(0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "removespacedims2:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);

  return 0;
}
CATCH
