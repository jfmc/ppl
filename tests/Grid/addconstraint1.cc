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

namespace {

// add_constraint
bool
test01() {

  Variable A(0);
  Variable B(1);

  Grid gr(2);
  print_congruences(gr, "*** gr ***");

  gr.add_constraint(A == 3);
  print_congruences(gr, "*** gr.add_constraint(A == 3) ***");
  gr.add_constraint(B >= 0);

  Grid known_gr(2);
  known_gr.add_congruence(A == 3);
  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_constraint(B >= 0) ***");

  return ok;
}

// Add an NNC constraint with add_constraint.
bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(B + 0*C == 0);

  NNC_Polyhedron ph(cs);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_constraint(*ph.constraints().begin());

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_constraint(*ph.constraints().begin()) ***");

  return ok;
}

// add_constraint_and_minimize(cs)
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4);
  print_congruences(gr, "*** gr ***");

  gr.add_constraint_and_minimize(2*A == C);
  print_congruences(gr, "*** gr.add_constraint_and_minimize(2*A == C) ***");
  gr.add_constraint_and_minimize(D == 0);
  print_congruences(gr, "*** gr.add_constraint_and_minimize(D == 0) ***");
  gr.add_constraint_and_minimize(B > 2);

  Grid known_gr(4);
  known_gr.add_congruence(2*A == C);
  known_gr.add_congruence(D == 0);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_constraint_and_minimize(B > 2) ***");

  return ok;
}

// add_congruence(c), adding equality
bool
test04() {
  Variable D(3);

  Grid gr(4);
  print_congruences(gr, "*** gr ***");

  gr.add_congruence(D == 4);

  Grid known_gr(4);

  known_gr.add_congruence(D == 4);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruence(D == 4) ***");

  return ok;
}

// add_congruence(c), where grid stays the same
bool
test05() {
  Variable D(3);

  Grid gr(4);
  print_congruences(gr, "*** gr ***");

  Grid known_gr = gr;

  gr.add_congruence(D > 4);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruence(D > 4) ***");

  return ok;
}

// add_congruence_and_minimize(c), add equality.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);

  print_congruences(gr, "*** gr ***");

  gr.add_congruence_and_minimize(C == 4*A);

  Grid known_gr(3);
  known_gr.add_congruence(C == 4*A);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** add_congruence_and_minimize(C == 4*A) ***");

  return ok;
}

// add_congruence_and_minimize(c), where grid stays the same.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((B == 0) / 0);

  Grid known_gr = gr;

  print_congruences(gr, "*** gr ***");

  gr.add_congruence_and_minimize(C > 4*A);

  bool ok = (gr == known_gr);

  print_congruences(gr,
      "*** gr.add_congruence_and_minimize(C > 4*A) ***");

  return ok;
}

// add_constraint -- space dimension exception
bool
test08() {
  Variable B(1);

  Grid gr(1);

  try {
    gr.add_constraint(B == 0);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

// add_constraint_and_minimize -- space dimension exception
bool
test09() {
  Variable B(1);

  Grid gr(1);

  try {
    gr.add_constraint_and_minimize(B == 0);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
END_MAIN
