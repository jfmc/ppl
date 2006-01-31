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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);
Variable E(4);

// From congruences.

void
test1() {
  nout << "test1:" << endl;

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);

  Grid gr1(cgs);

  cgs.clear();
  cgs.insert((A %= 1) / 2);

  Grid gr2(cgs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 2);
  known_cgs.insert((B %= 1) / 2);

  Grid known_gr(known_cgs);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid empty.

void
test2() {
  nout << "test2:" << endl;

  Grid gr1(2, EMPTY);

  Congruence_System cgs;
  cgs.insert((A + 0*C %= 0) / 2);

  Grid gr2(cgs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(5, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Second grid empty.

void
test3() {
  nout << "test3:" << endl;

  Congruence_System cgs;
  cgs.insert((A + 0*C %= 0) / 2);

  Grid gr1(cgs);

  Grid gr2(2, EMPTY);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(5, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid a universe.

void
test4() {
  nout << "test4:" << endl;

  Grid gr1(1, UNIVERSE);

  Grid_Generator_System gs;
  gs.insert(grid_point(A));
  gs.insert(grid_point(A + C));

  Grid gr2(gs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid_Generator_System known_gs;
  known_gs.insert(grid_point(B));
  known_gs.insert(grid_point(B + D));
  known_gs.insert(grid_line(A));

  Grid known_gr(known_gs);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// From generators.

void
test5() {
  nout << "test5:" << endl;

  Grid_Generator_System gs;
  gs.insert(grid_point(A));
  gs.insert(grid_point(A + C));

  Grid gr1(gs);

  gs.clear();
  gs.insert(grid_point(0*B));
  gs.insert(grid_point(B));

  Grid gr2(gs);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A == 1) / 0);
  known_cgs.insert((C %= 0) / 1);
  known_cgs.insert((B == 0) / 0);
  known_cgs.insert((D == 0) / 0);
  known_cgs.insert((E %= 0) / 1);

  Grid known_gr(known_cgs);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// First grid empty via the congruence system.

void
test6() {
  nout << "test6:" << endl;

  Grid gr1(1);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((A %= 1) / 2);

  Grid gr2(2);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Second grid empty via the congruence system.

void
test7() {
  nout << "test7:" << endl;

  Grid gr1(2);

  Grid gr2(1);
  gr2.add_congruence((A %= 0) / 2);
  gr2.add_congruence((A %= 1) / 2);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(3, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Zero dimension universe.

void
test8() {
  nout << "test8:" << endl;

  Grid gr1(0);

  Grid gr2(1);
  gr2.add_congruence((A %= 0) / 2);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence((A %= 0) / 2);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Zero dimension empty.

void
test9() {
  nout << "test9:" << endl;

  Grid gr1(0, EMPTY);

  Grid gr2(1);
  gr2.add_congruence((A %= 0) / 2);

  gr1.concatenate_assign(gr2);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(1, EMPTY);

  if (gr1 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr1 << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Space dimension exception.

void
test10() {
  nout << "test10:" << endl;

  Grid gr1(7);

  Grid gr2(1);
  gr2.add_congruence(Congruence::zero_dim_integrality());
  gr2.minimized_congruences();
  gr2.ascii_dump();
  // This needs to allocate a lot of memory, in order to create the
  // integrality congruence.  The presence of the integrality
  // congruence is required by the conversion.
  gr2.add_space_dimensions_and_embed(Grid::max_space_dimension() - 1);

  try {
    gr1.concatenate_assign(gr2);
    exit(1);
  }
  catch (const std::length_error& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "concatenate1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  //test10();

  return 0;
}
CATCH
