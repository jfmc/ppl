/* Test adding single constraints to grids.
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
Variable D(3);

// add_constraint

void
test1() {
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

// Add an NNC constraint with add_constraint.

void
test2() {
  Constraint_System cs;
  cs.insert(B + 0*C == 0);

  NNC_Polyhedron ph(cs);

  Grid gr(3);

  gr.add_constraint(*ph.constraints().begin());

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

// add_constraint_and_minimize(cs)

void
test3() {
  Grid gr(4);
  gr.add_constraint_and_minimize(2*A == C);
  gr.add_constraint_and_minimize(D == 0);
  gr.add_constraint_and_minimize(B > 2);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence(2*A == C);
  known_gr.add_congruence(D == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruence(c), adding equality

void
test4() {
  Grid gr(4);
  gr.add_congruence(D == 4);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence(D == 4);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruence(c), where grid stays the same

void
test5() {
  Grid gr(4);

  Grid known_gr = gr;

  gr.add_congruence(D > 4);

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruence_and_minimize(c), add equality.

void
test6() {
  Grid gr(3);
  gr.add_congruence_and_minimize(C == 4*A);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(C == 4*A);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruence_and_minimize(c), where grid stays the same.

void
test7() {
  Grid gr(3);
  gr.add_congruence((B == 0) / 0);

  Grid known_gr = gr;

  gr.add_congruence_and_minimize(C > 4*A);

  if (find_variation(gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_constraint -- space dimension exception

void
test8() {
  Grid gr(1);

  try {
    gr.add_constraint(B == 0);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_constraint_and_minimize -- space dimension exception

void
test9() {
  Grid gr(1);

  try {
    gr.add_constraint_and_minimize(B == 0);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "addconstraint1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);

  return 0;
}
CATCH
