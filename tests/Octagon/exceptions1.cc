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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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

  TOctagon oc1(2);
  oc1.add_constraint(x >= y);

  TOctagon oc2(3);

  try {
    // This is an invalid use of function
    // Octagon::intersection_assign_and_minimize(oc2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    oc1.intersection_assign_and_minimize(oc2);
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

  TOctagon oc1(7);

  TOctagon oc2(15);

  try {
    // This is an invalid use of function
    // Octagon::intersection_assign(oc2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    oc1.intersection_assign(oc2);
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
  TOctagon oc(5);

  try {
    // This is an invalid use of the function
    // Octagon::remove_higher_dimensions(n): it is illegal to erase
    // a variable that is not in the space of the polyhedron.
    oc.remove_higher_space_dimensions(7);
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

  TOctagon oc(1);

  try {
    // This is an invalid use of the function
    // Octagon::add_constraints_and_minimize(cs): it is illegal to
    // add a system of constraints that is not dimensional incompatible
    // with the polyhedron.
    Constraint_System cs;
    cs.insert(x - y >= 0);
    oc.add_constraints_and_minimize(cs);
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
  Variable y(1);

  TOctagon oc(1);

  try {
    // This is an invalid use of the function
    // Octagon::add_constraint(c): it is illegal to insert a
    // constraints that contains a variable that is not in the space
    // of the polyhedron.
    oc.add_constraint(y >= 0);
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
error6() {
  Variable x(0);
  Variable y(1);

  TOctagon oc(1);

  try {
    // This is an invalid use of the function
    // Octagon::add_constraints(cs): it is illegal to add a system
    // of constraints that is dimensional incompatible with the
    // polyhedron.
    Constraint_System cs;
    cs.insert(x - y == 0);
    oc.add_constraints(cs);
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
error7() {
  TOctagon oc1(5);
  TOctagon oc2(10);

  try {
    // This is an invalid use of the function
    // Octagon::CC76_widening_assign(oc2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    oc2.CC76_extrapolation_assign(oc1);
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
error8() {
  Variable y(1);

  TOctagon oc1(1);
  TOctagon oc2(2);

  Constraint_System cs;
  cs.insert(y <= 9);

  try {
    // This is an invalid use of the function
    // Octagon::limited_CC76_extrapolation_assign(oc2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
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
error9() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagon oc1(2);
  oc1.add_constraint(x - y >= 0);
  oc1.add_constraint(x >= 0);
  oc1.add_constraint(x <= 2);

  TOctagon oc2(2);
  oc2.add_constraint(x - y >= 0);
  oc2.add_constraint(x >= 0);
  oc2.add_constraint(x <= 5);

  Constraint_System cs;
  cs.insert(z <= 5);

  try {
    // This is an invalid use of the function
    // Octagon::limited_CC76_extrapolation_assign(oc, cs): it is
    // illegal to apply this function to a system of constraints that
    // is not dimension-compatible with the two polyhedra.
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
error10() {
  Variable x(0);
  Variable y(1);

  TOctagon oc1(3);
  oc1.add_constraint(x - y >= 0);

  TOctagon oc2(2);
  oc2.add_constraint(x - y == 0);

  try {
    // This is an invalid use of Polyhedron::contains(): it is
    // illegal to apply this method to two polyhedra that are not
    // dimension-compatible.
    oc1.contains(oc2);
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
error11() {

  TOctagon oc1(3);
  TOctagon oc2(5);

  try {
    // This is an incorrect use of function
    // Octagon::poly_difference_assign(oc2): it is impossible to apply
    // this function to two polyhedra of different dimensions.
    oc1.poly_difference_assign(oc2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}


static void
error12() {

  TOctagon oc1(12);
  TOctagon oc2(5);

  try {
    // This is an incorrect use of function
    // Octagon::poly_hull_assign(oc2): it is impossible to apply
    // this function to two polyhedra of different dimensions.
    oc1.poly_hull_assign(oc2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

static void
error13() {
  Variable x(0);
  Variable y(1);

  TOctagon oc1(2);
  oc1.add_constraint(x >= y);

  TOctagon oc2(3);

  try {
    // This is an invalid use of function
    // Octagon::poly_hull_assign_and_minimize(oc2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    oc1.poly_hull_assign_and_minimize(oc2);
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
error14() {
  Variable y(1);

  TOctagon oc1(1);
  TOctagon oc2(2);

  Constraint_System cs;
  cs.insert(y >= 6);

  try {
    // This is an invalid use of the function
    // Octagon::CC76_narrowing_assign(oc2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    oc2.CC76_narrowing_assign(oc1);
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
error15() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs;
  cs.insert(x <= 3);
  cs.insert(y - z <= 2);
  TOctagon oc(cs);

  Variables_Set to_be_removed;
  to_be_removed.insert(z);

  oc.remove_space_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // This is an incorrect use use of function
    // Octagon::remove_space_dimensions(to_be_remove).
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    oc.remove_space_dimensions(to_be_removed);
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
error16() {
  TOctagon oc1(5);
  TOctagon oc2(10);

  try {
    // This is an invalid use of the function
    // Octagon::CH78_widening_assign(oc2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    oc2.CH78_widening_assign(oc1);
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
error17() {
  Variable y(1);

  TOctagon oc1(1);
  TOctagon oc2(2);

  Constraint_System cs;
  cs.insert(y <= 9);

  try {
    // This is an invalid use of the function
    // Octagon::limited_CH78_extrapolation_assign(oc2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    oc2.limited_CH78_extrapolation_assign(oc1, cs);
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
error18() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagon oc1(2);
  oc1.add_constraint(x - y >= 0);
  oc1.add_constraint(x >= 0);
  oc1.add_constraint(x <= 2);

  TOctagon oc2(2);
  oc2.add_constraint(x - y >= 0);
  oc2.add_constraint(x >= 0);
  oc2.add_constraint(x <= 5);

  Constraint_System cs;
  cs.insert(z <= 5);

  try {
    // This is an invalid use of the function
    // Octagon::limited_CH78_extrapolation_assign(oc, cs): it is
    // illegal to apply this function to a system of constraints that
    // is not dimension-compatible with the two polyhedra.
    oc2.limited_CH78_extrapolation_assign(oc1, cs);
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
  error6();
  error7();
  error8();
  error9();
  error10();
  error11();
  error12();
  error13();
  error14();
  error15();
  error16();
  error17();
  error18();

  return 0;
}
CATCH
