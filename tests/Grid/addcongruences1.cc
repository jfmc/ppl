/* Test methods which can add multiple congruences to a grid.
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

// add_congruences

void
test1() {
  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);
  cgs.insert((B == 0) / 2);

  Grid gr(2);
  gr.add_congruences(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A %= 0) / 2);
  known_gr.add_congruence((B == 0) / 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences

void
test2() {
  Congruence_System cgs;
  cgs.insert((A + B %= 0) / 2);

  Grid gr(2);
  gr.add_recycled_congruences(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A + B %= 0) / 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_congruences_and_minimize

void
test3() {
  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);
  cgs.insert(A + B == 0);

  Grid gr(2);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(2*A - 2*B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences_and_minimize

void
test4() {
  Congruence_System cgs;
  cgs.insert((B %= 0) / 2);
  cgs.insert(A - B == 0);

  Grid gr(2);
  gr.add_recycled_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_point(2*A + 2*B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences(cgs) -- space dimension exception

void
test5() {
  Congruence_System cgs;
  cgs.insert((A + B %= 0) / 2);

  Grid gr(1);

  try {
    gr.add_recycled_congruences(cgs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_congruences(cgs) -- space dimension exception

void
test6() {
  Congruence_System cgs;
  cgs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_congruences(cgs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_congruences_and_minimize(cgs) -- space dimension
// exception

void
test7() {
  Congruence_System cgs;
  cgs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_congruences_and_minimize(cgs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_congruences, empty grid.

void
test8() {
  Congruence_System cgs;
  cgs.insert((A + B %= 0) / 2);

  Grid gr(2, EMPTY);
  gr.add_recycled_congruences(cgs);

  Grid known_gr(2, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences_and_minimize, add empty system.

void
test9() {
  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(3*A + B));

  Grid known_gr = gr;

  Congruence_System cgs;

  gr.add_recycled_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences_and_minimize, add system of single trivial
// congruence to zero dim grid.

void
test10() {
  Grid gr(0);

  Grid known_gr = gr;

  Congruence_System cgs;
  cgs.insert(Congruence::zero_dim_integrality());

  gr.add_recycled_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences_and_minimize, add to empty grid.

void
test11() {
  Grid gr(2, EMPTY);

  Grid known_gr = gr;

  Congruence_System cgs;
  cgs.insert(A + B == 0);

  gr.add_recycled_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences_and_minimize, add empty system to grid
// with minimized generators and up to date congruences.

void
test12() {
  Grid gr(2);

  // Ensure both systems are up to date with only generators minimal.
  gr.affine_image(A, 1*A);
  gr.minimized_generators();

  Congruence_System cgs;

  gr.add_recycled_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_recycled_congruences_and_minimize, add empty system to grid
// with up to date congruences and generators.

void
test13() {
  Grid gr(2);

  // Ensure both systems are just up to date.
  gr.affine_image(A, 1*A);

  Congruence_System cgs;

  gr.add_recycled_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);

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

  nout << "addcongruences1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);

  return 0;
}
CATCH
