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
  nout << "test1:" << endl;

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
  nout << "test2:" << endl;

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
  nout << "test3:" << endl;

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
  nout << "test4:" << endl;

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

} // namespace

int
main() TRY {
  set_handlers();

  nout << "addcongruences1:" << endl;

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
