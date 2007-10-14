/* Computing the polynomial pre-image of Polynomial_Spaces.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

bool
test01() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  Polynomial_Space<2> ps1(2);
  ps1.add_polynomial_constraint(Polynomial(x1) == 0);

  Polynomial expr(x1 + 1);

  try {
    // This is an invalid use of function
    // Polynomial_Space::polynomial_preimage:
    // it is illegal to compute the affine pre-image with
    // a zero denominator.

  ps1.polynomial_preimage(x1,expr,0);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test02() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  Polynomial_Space<2> ps1(2);
  ps1.add_polynomial_constraint(Polynomial(x1) == 0);

  Polynomial expr(x3 + 1);

  try {
    // This is an invalid use of function
    // Polynomial_Space::polynomial_preimage:
    // it is illegal to compute the polynomial pre-image with
    // a right-hand side which is dimension-incompatible.

  ps1.polynomial_preimage(x1,expr);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  Polynomial_Space<2> ps1(2);
  ps1.add_polynomial_constraint(Polynomial(x1) == 0);

  Polynomial expr(x1 + 1);

  try {
    // This is an invalid use of function
    // Polynomial_Space::polynomial_preimage:
    // it is illegal to compute the affine pre-image with
    // a variable which is dimension-incompatible.

  ps1.polynomial_preimage(x3,expr);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test04() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if computing the pre-image of an invertible affine map
  // and then that of its inverse yields the same space.

  Polynomial_Space<2> ps1(3);
  ps1.add_polynomial_constraint(Polynomial(3 + 2*x1 + x2) == 0);

  Polynomial e1( x1 + 1 );
  ps1.polynomial_preimage(x1,e1);

  Polynomial e2( x1 - 1 );
  ps1.polynomial_preimage(x1,e2);

  Polynomial_Space<2> known_result(3);
  known_result.add_polynomial_constraint(Polynomial(3 + 2*x1 + x2) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test05() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if computing the pre-image of an invertible affine map
  // and then that of its inverse yields the same space.

  Polynomial_Space<2> ps1(3);
  ps1.add_polynomial_constraint(Polynomial(1 + 2*x1*x2 - x3) == 0);
  ps1.add_polynomial_constraint(Polynomial(1 + x2 + x2*x2) == 0);

  Polynomial e1( 3*x2 + 1 + x1 + x3);
  ps1.polynomial_preimage(x2,e1,2);

  Polynomial e2( 2*x2 - 1 - x1 - x3);
  ps1.polynomial_preimage(x2,e2,3);

  Polynomial_Space<2> known_result(3);
  known_result.add_polynomial_constraint(Polynomial(1 + 2*x1*x2 - x3) == 0);
  known_result.add_polynomial_constraint(Polynomial(1 + x2 + x2*x2) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test06() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if computing the pre-image of an invertible affine map
  // and then that of its inverse yields the same space.

  Polynomial_Space<3> ps1(3);
  ps1.add_polynomial_constraint
    (Polynomial(-1 - x3 - x1*x3 + x3*x3 - x1*x2*x2) == 0);
  ps1.add_polynomial_constraint
    (Polynomial(1 + x3 - x3*x3 + 2*x1*x2*x3 - x3*x3*x3) == 0);

  Polynomial e1( 5*x3 + 1 + x1);
  ps1.polynomial_preimage(x3,e1,3);

  Polynomial e2( 3*x3 - 1 - x1);
  ps1.polynomial_preimage(x3,e2,5);

  Polynomial_Space<3> known_result(3);
  known_result.add_polynomial_constraint
    (Polynomial(-1 - x3 - x1*x3 + x3*x3 - x1*x2*x2) == 0);
  known_result.add_polynomial_constraint
    (Polynomial(1 + x3 - x3*x3 + 2*x1*x2*x3 - x3*x3*x3) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test07() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing non-invertible pre-images of polynomial maps.

  Polynomial_Space<3> ps1(3);
  ps1.add_polynomial_constraint
    (Polynomial(x1 - x2) == 0);
  ps1.add_polynomial_constraint
    (Polynomial(x1 + 1) == 0);

  Polynomial e1(x2);
  ps1.polynomial_preimage(x1,e1);

  Polynomial_Space<3> known_result(3);
  known_result.add_polynomial_constraint
    (Polynomial(x2 + 1) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test08() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing non-invertible pre-images of affine maps.

  Polynomial_Space<3> ps1(3);
  ps1.add_polynomial_constraint
    (Polynomial(2*x1 - x2) == 0);
  ps1.add_polynomial_constraint
    (Polynomial(2*x1 - x3) == 0);

  Polynomial e1(x2);
  ps1.polynomial_preimage(x1,e1,2);

  Polynomial_Space<3> known_result(3);
  known_result.add_polynomial_constraint
    (Polynomial(x2 - x3) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test09() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if computing the pre-image of an invertible polynomial
  // map and then that of its inverse yields the same space.

  Polynomial_Space<5> ps1(3);
  ps1.add_polynomial_constraint
    (Polynomial(-1 - x3 - x1*x3 + x3*x3 - x1*x2*x2) == 0);
  ps1.add_polynomial_constraint
    (Polynomial(1 + x3 - x3*x3 + 2*x1*x3) == 0);

  Polynomial e1( 5*x3 + x2*x2 + x1*x1);
  ps1.polynomial_preimage(x3,e1,3);

  Polynomial e2( 3*x3 - x2*x2 - x1*x1);
  ps1.polynomial_preimage(x3,e2,5);

  Polynomial_Space<5> known_result(3);
  known_result.add_polynomial_constraint
    (Polynomial(-1 - x3 - x1*x3 + x3*x3 - x1*x2*x2) == 0);
  known_result.add_polynomial_constraint
    (Polynomial(1 + x3 - x3*x3 + 2*x1*x3) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
  DO_TEST(test03)
  DO_TEST(test04)
  DO_TEST(test05)
  DO_TEST(test06)
  DO_TEST(test07)
  DO_TEST(test08)
  DO_TEST(test09)
END_MAIN
