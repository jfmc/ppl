/* Some incorrect uses of the function BD_Shape::add_constraint().
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

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a strict inequality.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y < 0);
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

  TBD_Shape bd1(2);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a constraint with bigger dimension.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y - x + z >= 0);
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
  TBD_Shape bd1(5);
  TBD_Shape bd2(10);

  try {
    // This is an invalid use of the function
    // BD_Shape::CH78_widening_assign(bd1): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    bd2.CH78_widening_assign(bd1);
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
    // BD_Shape::limited_CH78_extrapolation_assign(bd, cs): it is
    // illegal to apply this function to a system of constraints that
    // is not dimension-compatible with the two polyhedra.
    bd2.limited_CH78_extrapolation_assign(bd1, cs);
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

  TBD_Shape bd1(1);
  TBD_Shape bd2(2);

  Constraint_System cs;
  cs.insert(y <= 9);

  try {
    // This is an invalid use of the function
    // BD_Shape::limited_CH78_extrapolation_assign(bd2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    bd2.limited_CH78_extrapolation_assign(bd1, cs);
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
    // BD_Shape::limited_CH78_extrapolation_assign(bd, cs): it is
    // illegal to apply this function to a system of constraints that
    // has a strict-inequality.
    bd2.limited_CH78_extrapolation_assign(bd1, cs);
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
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x - y >= 0);
  bd.add_constraint(x >= 0);
  bd.add_constraint(x <= 2);

  Linear_Expression coeff1 = x + 1;

  try {
    // This is an incorrect use of function
    // BD_Shape::affine_image(v, expr,d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    Integer d = 0;
    bd.affine_image(x, coeff1, d);
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
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(1);
  bd.add_constraint(x >= 1);

  try {
    // This is an invalid used of the function
    // BD_Shape::affine_image(v, expr, d): it is illegal to
    // apply this function to a variable that is not in the space of
    // the polyhedron.
    bd.affine_image(y, x + 1);
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

  TBD_Shape bd(2);
  bd.add_constraint(x >= 1);
  bd.add_constraint(y >= 1);

  try {
    // This is an invalid used of the function
    // BD_Shape::affine_image(v, expr, d): it is illegal to
    // use a variable in the expression that does not appear in the
    // space of the polyhedron.
    bd.affine_image(y, x + z + 1);
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
  Variable y(1);

  TBD_Shape bd1(1);
  TBD_Shape bd2(2);

  try {
    // This is an invalid use of the function
    // BD_Shape::CC76_extrapolation_assign(bd): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
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
error11() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x - y >= 0);
  bd.add_constraint(x >= 0);
  bd.add_constraint(x <= 2);

  Linear_Expression coeff1 = 2*x + 1;

  try {
    // This is an incorrect use of function
    // BD_Shape::affine_image(v, expr,d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    Integer d = 3;
    bd.affine_image(x, coeff1, d);
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
error12() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(2);
  bd.add_constraint(x >= 1);
  bd.add_constraint(y >= 1);

  try {
    // This is an invalid used of the function
    // BD_Shape::affine_image(v, expr, d): it is illegal to
    // use two variables in the expression.
    bd.affine_image(y, y - x);
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
error13() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(3);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a constraint with three dimension.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y - x + z >= 0);
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
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd1(3);

  try {
    // This is an invalid use of function
    // BD_Shape::add_constraint: it is illegal
    // to add a constraint with two different coefficients.
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(2*y - 3*x <= 0);
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

  return 0;

}
CATCH
