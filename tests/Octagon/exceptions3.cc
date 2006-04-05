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

  TOctagon oct(2);
  oct.add_constraint(x - y >= 0);
  oct.add_constraint(x >= 0);
  oct.add_constraint(x <= 2);

  Linear_Expression coeff1 = x + 1;

  try {
    // This is an invalid use of the function
    // Octagon::affine_image(v, e, d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    //    Integer d = 0;
    Coefficient_traits::const_reference d = 0;
    oct.affine_image(y, coeff1, d);
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

  TOctagon oct(2);
  oct.add_constraint(x + y >= 2);

  try {
    // This is an invalid use of function
    // Octagon::affine_image(v, expr, d): it is illegal to
    // apply this function to a variable that is not in the space of
    // the polyhedron.
    oct.affine_image(y, z - 2);
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
  Variable z(2);

  TOctagon oct(2);
  oct.add_constraint(x - y >= 0);
  oct.add_constraint(x >= 0);
  oct.add_constraint(x <= 2);

  try {
    // This is an invalid use of the function
    // Octagon::affine_image(v, expr, d): it is illegal to
    // use a variable in the expression that does not appear in the
    // space of the polyhedron.
    oct.affine_image(x, y - z + 1);
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

  TOctagon oct(2);
  oct.add_constraint(x - y >= 0);
  oct.add_constraint(x >= 0);
  oct.add_constraint(x <= 2);

    try {
      // This is an invalid use of the function
      // Octagon::affine_image(v, e, d): it is illegal to use
      // an expression where the variable coefficient is not equal
      // to the denominator.
    oct.affine_image(y, 2*x + 1);
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

  TOctagon oct(3);
  oct.add_constraint(x + y >= 0);
  oct.add_constraint(x >= 0);
  oct.add_constraint(z <= 2);

  Linear_Expression coeff1 = y + 1;

  try {
    // This is an invalid use of the function
    // Octagon::affine_preimage(v, e, d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    //    Integer d = 0;
    Coefficient_traits::const_reference d = 0;
    oct.affine_preimage(y, coeff1, d);
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
  Variable z(2);

  TOctagon oct(2);
  oct.add_constraint(x - y >= 2);
  oct.add_constraint(y <= 7);

  try {
    // This is an invalid use of function
    // Octagon::affine_preimage(v, expr, d): it is illegal to
    // apply this function to a variable that is not in the space of
    // the polyhedron.
    oct.affine_preimage(x, z - 2);
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
  Variable z(2);

  TOctagon oct(2);
  oct.add_constraint(x + y >= 0);
  oct.add_constraint(x >= 0);
  oct.add_constraint(y <= 2);

  try {
    // This is an invalid use of the function
    // Octagon::affine_preimage(v, expr, d): it is illegal to
    // use a variable in the expression that does not appear in the
    // space of the polyhedron.
    oct.affine_preimage(x, y - z + 1);
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

  TOctagon oct(2);
  oct.add_constraint(x - y >= 0);
  oct.add_constraint(y >= 0);
  oct.add_constraint(x <= 2);

    try {
      // This is an invalid use of the function
      // Octagon::affine_preimage(v, e, d): it is illegal to use
      // an expression where the variable coefficient is not equal
      // to the denominator.
    oct.affine_preimage(y, 2*x + 2, 3);
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

  TOctagon oct(3);
  oct.add_constraint(x + y >= 0);
  oct.add_constraint(y >= 0);
  oct.add_constraint(z <= 2);
  oct.add_constraint(z - x >= 9);

  Linear_Expression coeff1 = y + 1;

  try {
    // This is an invalid use of the function
    // Octagon::generalized_affine_image(v, e, d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    //    Integer d = 0;
    Coefficient_traits::const_reference d = 0;
    oct.generalized_affine_image(y, LESS_THAN_OR_EQUAL, coeff1, d);
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

  TOctagon oct(2);
  oct.add_constraint(x >= y);

  Linear_Expression expr(x + 1);

  try {
    // This is an incorrect use of the function
    // Octagon::generalized_affine_image(v, r, expr, d): it is illegal
    // to use a strict relation symbol.
    oct.generalized_affine_image(x, LESS_THAN, expr);
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

  TOctagon oct(2);
  oct.add_constraint(x >= y);

  Linear_Expression expr(x + 1);

  try {
    // This is an incorrect use of the function
    // Octagon::generalized_affine_image(v, r, expr, d): it is illegal
    // to use a strict relation symbol.
    oct.generalized_affine_image(x, GREATER_THAN, expr);
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

  TOctagon oct(2);
  oct.add_constraint(x >= y);
  Linear_Expression expr(x + y +1);
  try {
    // This is an incorrect use of the function
    // Octagon::generalized_affine_image(v, r, expr, d): it is illegal
    // to apply to a expression with two variables.
    oct.generalized_affine_image(y, LESS_THAN_OR_EQUAL, expr);
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

  TOctagon oct(2);
  oct.add_constraint(x >= y);

  Linear_Expression expr(x + y +1);

  try {
    // This is an incorrect use of the function
    // Octagon::affine_preimage(v, expr, d): it is illegal
    // to apply to a expression with two variables.
    oct.affine_preimage(y, expr);
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

  TOctagon oct(2);
  oct.add_constraint(x >= y);
  Linear_Expression expr(z);
  try {
    // This is an incorrect use of the function
    // Octagon::generalized_affine_image(v, r, expr, d): it is illegal
    // to apply to a expression which space dimension is
    // greather than octiffs space dimension.
    oct.generalized_affine_image(y, GREATER_THAN_OR_EQUAL, expr);
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

  TOctagon oct(2);
  oct.add_constraint(x >= y);
  Linear_Expression expr(2*x);
  try {
    // This is an incorrect use of the function
    // Octagon::generalized_affine_image(v, r, expr, d): it is illegal
    // to apply to a expression where the coefficient of the
    // variable is not equal to the denominator.
    oct.generalized_affine_image(y, GREATER_THAN_OR_EQUAL, expr);
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
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oct(2);
  oct.add_constraint(A >= 0);

  try {
    // This is an incorrect use of function
    // Octagon::generalized_affine_image(lhs, r, rhs):
    // it is illegal to use a variable in the `rhs' expression that
    // does not appear in the octagon.
    oct.generalized_affine_image(A + B, GREATER_THAN_OR_EQUAL, B + C);
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
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oct(2);
  oct.add_constraint(A >= 1);

  try {
    // This is an incorrect use of function
    // Octagon::generalized_affine_image(lhs, r, rhs):
    // it is illegal to use a variable in the `lhs' expression that
    // does not appear in the octagon.
    oct.generalized_affine_image(B + C, LESS_THAN_OR_EQUAL, A + 1);
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
