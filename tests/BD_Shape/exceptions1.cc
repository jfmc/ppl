/* Some incorrect uses of the functions of PPL.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

  TBD_Shape bd1(2);
  bd1.add_constraint(x >= y);

  TBD_Shape bd2(3);

  try {
    // This is an invalid use of function
    // BD_Shape::intersection_assign_and_minimize(bd2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    bd1.intersection_assign_and_minimize(bd2);
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

  TBD_Shape bd1(7);

  TBD_Shape bd2(15);

  try {
    // This is an invalid use of function
    // BD_Shape::intersection_assign(bd2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    bd1.intersection_assign(bd2);
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
  TBD_Shape bd(5);

  try {
    // This is an invalid use of the function
    // BD_Shape::remove_higher_dimensions(n): it is illegal to erase
    // a variable that is not in the space of the polyhedron.
    bd.remove_higher_space_dimensions(7);
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

  TBD_Shape bd(1);

  try {
    // This is an invalid use of the function
    // BD_Shape::add_constraints_and_minimize(cs): it is illegal to
    // add a system of constraints that is not dimensional incompatible
    // with the polyhedron.
    Constraint_System cs;
    cs.insert(x - y >= 0);
    bd.add_constraints_and_minimize(cs);
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

  TBD_Shape bd(1);

  try {
    // This is an invalid use of the function
    // RBD_Shape::add_constraint(c): it is illegal to insert a
    // constraints that contains a variable that is not in the space
    // of the polyhedron.
    bd.add_constraint(y >= 0);
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

  TBD_Shape bd(1);

  try {
    // This is an invalid use of the function
    // BD_Shape::add_constraints(cs): it is illegal to add a system
    // of constraints that is dimensional incompatible with the
    // polyhedron.
    Constraint_System cs;
    cs.insert(x - y == 0);
    bd.add_constraints(cs);
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
  TBD_Shape bd1(5);
  TBD_Shape bd2(10);

  try {
    // This is an invalid use of the function
    // BD_Shape::CC76_widening_assign(bd2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    bd2.CC76_extrapolation_assign(bd1);
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

  TBD_Shape bd1(1);
  TBD_Shape bd2(2);

  Constraint_System cs;
  cs.insert(y <= 9);

  try {
    // This is an invalid use of the function
    // BD_Shape::limited_CC76_extrapolation_assign(bd2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    bd2.limited_CC76_extrapolation_assign(bd1, cs);
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

  TBD_Shape bd1(2);
  bd1.add_constraint(x - y >= 0);
  bd1.add_constraint(x >= 0);
  bd1.add_constraint(x <= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(x - y >= 0);
  bd2.add_constraint(x >= 0);
  bd2.add_constraint(x <= 5);

  Constraint_System cs;
  cs.insert(z <= 5);

  try {
    // This is an invalid use of the function
    // BD_Shape::limited_CC76_extrapolation_assign(bd, cs): it is
    // illegal to apply this function to a system of constraints that
    // is not dimension-compatible with the two polyhedra.
    bd2.limited_CC76_extrapolation_assign(bd1, cs);
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

  TBD_Shape bd1(3);
  bd1.add_constraint(x - y >= 0);

  TBD_Shape bd2(2);
  bd2.add_constraint(x - y == 0);

  try {
    // This is an invalid use of Polyhedron::contains(): it is
    // illegal to apply this method to two polyhedra that are not
    // dimension-compatible.
    bd1.contains(bd2);
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

  TBD_Shape bd1(3);
  TBD_Shape bd2(5);

  try {
    // This is an incorrect use of function
    // BD_Shape::poly_difference_assign(bd2): it is impossible to apply
    // this function to two polyhedra of different dimensions.
    bd1.poly_difference_assign(bd2);
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

  TBD_Shape bd1(12);
  TBD_Shape bd2(5);

  try {
    // This is an incorrect use of function
    // BD_Shape::poly_hull_assign(bd2): it is impossible to apply
    // this function to two polyhedra of different dimensions.
    bd1.poly_hull_assign(bd2);
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

  TBD_Shape bd1(2);
  bd1.add_constraint(x >= y);

  TBD_Shape bd2(3);

  try {
    // This is an invalid use of function
    // BD_Shape::poly_hull_assign_and_minimize(bd2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    bd1.poly_hull_assign_and_minimize(bd2);
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

  TBD_Shape bd1(1);
  TBD_Shape bd2(2);

  Constraint_System cs;
  cs.insert(y >= 6);

  try {
    // This is an invalid use of the function
    // BD_Shape::CC76_narrowing_assign(bd2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    bd2.CC76_narrowing_assign(bd1);
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
  TBD_Shape bd(cs);

  Variables_Set to_be_removed;
  to_be_removed.insert(z);

  bd.remove_space_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // This is an incorrect use use of function
    // BD_Shape::remove_dimensions(to_be_remove).
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    bd.remove_space_dimensions(to_be_removed);
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
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(2);
  bd1.add_constraint(x - y >= 0);
  bd1.add_constraint(x >= 0);
  bd1.add_constraint(x <= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(x - y >= 0);
  bd2.add_constraint(x >= 0);
  bd2.add_constraint(x <= 5);

  Constraint_System cs;
  cs.insert(x < 5);

  try {
    // This is an invalid use of the function
    // BD_Shape::limited_CC76_extrapolation_assign(bd, cs): it is
    // illegal to apply this function to a system of constraints that
    // in which there is a strict inequality.
    bd2.limited_CC76_extrapolation_assign(bd1, cs);
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
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(1);
  bd1.add_constraint(x == 1);

  TBD_Shape bd2(2);

  try {
    // This is an invalid use of the function
    // BD_Shape::time_elapse_assign(bd2): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    bd1.time_elapse_assign(bd2);
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

  return 0;
}
CATCH
