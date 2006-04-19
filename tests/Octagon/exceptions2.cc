/* Some incorrect uses of the functions of PPL.
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
error1() {
  Variable x(0);
  Variable y(1);

  TOctagon oc(2);

  try {
    // This is an invalid use of the function
    // Octagon::add_constraint(cs): it is illegal to
    // add a constraint that is a strict-inequality.
    oc.add_constraint(x - y < 0);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

static void
error2() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagon oc1(2);

  try {
    // This is an invalid use of function
    // Octagon::add_constraint: it is illegal
    // to add a constraint with bigger dimension.
    oc1.add_constraint(x <= 0);
    oc1.add_constraint(y - x + z >= 0);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}


static void
error3() {
  Variable x(0);
  Variable y(1);

  TOctagon oc1(2);
  oc1.add_constraint(x - y >= 0);
  oc1.add_constraint(x >= 0);
  oc1.add_constraint(x <= 2);

  TOctagon oc2(2);
  oc2.add_constraint(x - y >= 0);
  oc2.add_constraint(x >= 0);
  oc2.add_constraint(x <= 5);

  Constraint_System cs;
  cs.insert(x < 5);

  try {
    // This is an invalid use of the function
    // Octagon::limited_BHMZ05_extrapolation_assign(bd, cs): it is
    // illegal to apply this function to a system of constraints that
    // has a strict-inequality.
    oc2.limited_BHMZ05_extrapolation_assign(oc1, cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}


static void
error4() {
  Variable x(0);
  Variable y(1);

  TOctagon oc1(2);
  oc1.add_constraint(x - y >= 0);
  oc1.add_constraint(x >= 0);
  oc1.add_constraint(x <= 2);

  TOctagon oc2(2);
  oc2.add_constraint(x - y >= 0);
  oc2.add_constraint(x >= 0);
  oc2.add_constraint(x <= 5);

  Constraint_System cs;
  cs.insert(x < 5);

  try {
    // This is an invalid use of the function
    // Octagon::limited_CC76_extrapolation_assign(bd, cs): it is
    // illegal to apply this function to a system of constraints that
    // has a strict-inequality.
    oc2.limited_CC76_extrapolation_assign(oc1, cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

static void
error5() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagon oct1(1);
  oct1.add_constraint(x == 1);

  TOctagon oct2(3);
  oct2.add_constraint(y + z <= 6);

  try {
    // This is an invalid use of the function
    // Octagon::time_elapse_assign(oct1): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    oct2.time_elapse_assign(oct1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

int
main() TRY {
  error1();
  error2();
  error3();
  error4();
  error5();

  return 0;
}
CATCH
