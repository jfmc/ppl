/* Test Grid::remove_space_dimensions().
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);
Variable E(4);

// Simple grid.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(2);
  gr.add_congruence(A - B == 0);
  gr.add_congruence(A %= 0);

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 0);

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

  Grid gr(4, EMPTY);

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);

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

  Grid gr(7, UNIVERSE);

  Variables_Set vars;
  vars.insert(C);
  vars.insert(D);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(5, UNIVERSE);

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

  Grid_Generator_System gs;
  gs.insert(grid_point(0*A));
  gs.insert(grid_point(2*A));
  gs.insert(grid_point(3*B));

  Grid gr(gs);

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 2);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// From congruences.

void
test5() {
  nout << "test5:" << endl;

  Variables_Set vars;
  vars.insert(B);
  vars.insert(D);

  Congruence_System cgs;
  cgs.insert((A + 2*C %= 0) / 3);
  cgs.insert((B - E %= 0) / 2);

  Grid gr(cgs);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point());
  known_gs.insert(grid_line(2*A - B));
  known_gs.insert(grid_point(3*B, 2));
  known_gs.insert(grid_line(C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Variable set includes first dimension.

void
test6() {
  nout << "test6:" << endl;

  Grid gr(3);
  gr.add_congruence(A - B == 0);
  gr.add_congruence(A %= 0);

  Variables_Set vars;
  vars.insert(A);
  vars.insert(C);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence(A %= 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// The resulting grid contains a parameter that is all zeros.

void
test7() {
  nout << "test7:" << endl;

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));

  Variables_Set vars;
  vars.insert(B);

  gr.remove_space_dimensions(vars);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A %= 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Empty variable set.

void
test8() {
  nout << "test8:" << endl;

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));

  Variables_Set vars;

  Grid known_gr = gr;

  gr.remove_space_dimensions(vars);

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
test9() {
  nout << "test9:" << endl;

  Grid gr(1, EMPTY);

  Variables_Set vars;
  vars.insert(B);

  try {
    gr.remove_space_dimensions(vars);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Zero dimension universe resulting grid.

void
test10() {
  nout << "test10:" << endl;

  Grid gr(3, EMPTY);
  gr.add_generator(grid_point());
  gr.add_generator(grid_point(A));
  gr.add_generator_and_minimize(grid_point(B));
  gr.add_generator(grid_line(C));

  Variables_Set vars;
  vars.insert(A);
  vars.insert(B);
  vars.insert(C);

  gr.remove_space_dimensions(vars);

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

  nout << "removespacedims1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();

  return 0;
}
CATCH
