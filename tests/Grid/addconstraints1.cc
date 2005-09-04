/* Test adding constraints.
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

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// add_congruences_and_minimize(cs)

void
test1() {
  nout << "test1:" << endl;

  Grid gr(3);

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  gr.add_congruences_and_minimize(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_constraint

void
test2() {
  nout << "test2:" << endl;

  Grid gr(2);
  gr.add_constraint(A == 3);
  gr.add_constraint(B >= 0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A == 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_constraints

void
test3() {
  nout << "test3:" << endl;

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(3);

  gr.add_constraints(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_constraints, resulting grid empty

void
test4() {
  nout << "test4:" << endl;

  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(3);

  gr.add_constraints(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Grid(cs)

void
test5() {
  nout << "test5:" << endl;

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Grid(cs), resulting grid empty

void
test6() {
  nout << "test6:" << endl;

  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "addconstraints1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();

  return 0;
}
CATCH
