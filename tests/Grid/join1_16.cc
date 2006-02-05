/* Test Grid::join_assign() (a.k.a. Grid::upper_bound_assign()).
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

// Based on an example in a paper by Muller-Olm and Seidl in SAS 2005
// Here there is an input and output version of each variable
// A, B being input and A1, B1 the output.

void
test1() {
  nout << "test1:" << endl;

  Grid gr0(4); // initial point
  gr0.add_congruence(A == 2);
  gr0.add_congruence(B == 0);

  Grid gr1(4); // a pass through the procedure may do nothing
  gr1.add_congruence(A == C);
  gr1.add_congruence(B == D);

  gr1.intersection_assign(gr0); // add the inital point

  Grid gr2(4); // one non-trivial pass through procedure
  gr2.add_congruence(15 * A == C);
  gr2.add_congruence(18 * A + B == D);

  gr2.intersection_assign(gr0); // add the inital point

  Grid gr3(4); // two non-trivial passes through procedure
  gr3.add_congruence(225 * A == C);
  gr3.add_congruence(282 * A + B == D);

  gr3.intersection_assign(gr0); // add the inital point

  gr1.join_assign(gr2); // combine alternative paths 1 and 2
  gr1.join_assign(gr3); // combine alternative paths 1, 2 and 3

  Variables_Set vars;
  vars.insert(A);
  vars.insert(B);

  gr1.remove_space_dimensions(vars);

  if (find_variation(gr1))
    exit(1);

  Grid known_gr(2); // as in paper

  known_gr.add_congruence((A %= 2) / 28);
  known_gr.add_congruence((B %= 0) / 12);

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

  nout << "join1:" << endl;

  test1();

  return 0;
}
CATCH
