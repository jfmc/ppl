/* Test adding constraints to a grid.
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
Variable D(3);

// add_congruences_and_minimize(cs)

void
test1() {
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

// add_constraints

void
test2() {
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

// add_constraints, resulting grid empty.

void
test3() {
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

// add_congruences(cs)

void
test4() {
  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(B > 0);
  cs.insert(A == 0);
  cs.insert(C > 0);

  Grid gr(3);
  gr.add_congruences(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(A == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_recycled_congruences(cs)

void
test5() {
  Constraint_System cs;
  cs.insert(2*B == 3);
  cs.insert(A == 0);
  cs.insert(C > 0);

  Grid gr(3);
  gr.add_recycled_congruences(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(A == 0);
  known_gr.add_congruence(2*B == 3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_recycled_congruences_and_minimize(cs)

void
test6() {
  Constraint_System cs;
  cs.insert(2*B >= 3);
  cs.insert(2*A == 7);
  cs.insert(C > 0);

  Grid gr(3);
  gr.add_recycled_congruences_and_minimize(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(2*A == 7);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_constraints_and_minimize(cs)

void
test7() {
  Constraint_System cs;
  cs.insert(2*B >= 3);
  cs.insert(D == 0);
  cs.insert(2*A == C);

  Grid gr(4);
  gr.add_constraints_and_minimize(cs);

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

// add_recycled_constraints

void
test8() {
  Constraint_System cs;
  cs.insert(2*B > 2);
  cs.insert(2*D == 0);

  Grid gr(4);
  gr.add_recycled_constraints(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence(D == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_recycled_constraints_and_minimize

void
test9() {
  Constraint_System cs;
  cs.insert(2*B > 6);
  cs.insert(2*C == 6*D);

  Grid gr(4);
  gr.add_recycled_constraints_and_minimize(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence(C == 3*D);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_recycled_congruences(cs) -- space dimension exception

void
test10() {
  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_congruences(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_congruences(cs) -- space dimension exception

void
test11() {
  Constraint_System cs;
  cs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_congruences(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_congruences_and_minimize(cs) -- space dimension
// exception

void
test12() {
  Constraint_System cs;
  cs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_congruences_and_minimize(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_congruences_and_minimize(cs) -- space dimension exception

void
test13() {
  Constraint_System cs;
  cs.insert(B == 0);

  Grid gr(1);

  try {
    gr.add_congruences_and_minimize(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_constraints(cs) -- space dimension exception

void
test14() {
  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_constraints(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_constraints(cs) -- space dimension exception

void
test15() {
  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_constraints(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_constraints(cs) -- space dimension exception

void
test16() {
  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_constraints(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_recycled_constraints_and_minimize(cs) -- space dimension
// exception

void
test17() {
  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_recycled_constraints_and_minimize(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

// add_constraints_and_minimize(cs) -- space dimension exception

void
test18() {
  Constraint_System cs;
  cs.insert(A + B == 0);

  Grid gr(1);

  try {
    gr.add_constraints_and_minimize(cs);
    nout << "Exception expected." << endl;
    exit(1);
  } catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "addconstraints1:" << endl;

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
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST(test18);

  return 0;
}
CATCH
