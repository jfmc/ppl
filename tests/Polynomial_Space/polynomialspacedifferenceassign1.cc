/* Computing the difference of Polynomial_Spaces.
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
  ps1.add_constraint(Linear_Expression(x1) == 0);

  Polynomial_Space<2> ps2(3);
  ps2.add_constraint(Linear_Expression(x2) == 0);

  try {
    // This is an invalid use of function
    // Polynomial_Space::polynomial_space_difference_assign:
    // it is illegal to compute the difference of
    // dimension-incompatible polynomial spaces.

    ps1.polynomial_space_difference_assign(ps2);
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

  // Testing if the difference of a polynomial space with itself is the bottom.

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression(x1) == 0);
  ps1.add_constraint(Linear_Expression(2 + 2*x2 + x3) == 0);

  Polynomial_Space<2> ps2(ps1);

  ps1.polynomial_space_difference_assign(ps2);

  Polynomial_Space<2> known_result(3, Polynomial_Space<2>::BOTTOM);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if the difference of a polynomial space with the bottom
  // polynomial space does nothing.

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression(x1) == 0);
  ps1.add_constraint(Linear_Expression(2 + 2*x2 + x3) == 0);

  Polynomial_Space<2> ps2(3, Polynomial_Space<2>::BOTTOM);

  ps1.polynomial_space_difference_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(x1) == 0);
  known_result.add_constraint(Linear_Expression(2 + 2*x2 + x3) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test04() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if difference(space(x1 * x2), space(x1)) = space(x2).

  Polynomial_Space<2> ps1(3);
  ps1.add_polynomial_constraint(Polynomial(x1 * x2) == 0);

  Polynomial_Space<1> ps2(3);
  ps2.add_constraint(Linear_Expression(x1) == 0);

  ps1.polynomial_space_difference_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(x2) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test05() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if difference(space(x1 * (x1 - 1), space(x1 - 1)) = space(x1).

  Polynomial_Space<2> ps1(3);
  ps1.add_polynomial_constraint(Polynomial(x1) * (x1 - 1) == 0);

  Polynomial_Space<1> ps2(3);
  ps2.add_constraint(Linear_Expression(x1 - 1) == 0);

  ps1.polynomial_space_difference_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(x1) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test06() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if
  // difference(space((x1 - 1) * (x1 - 2), (x1 - 1) * x2, space(x1 - 1)) =
  // = space(x1 - 2, x2).

  Polynomial_Space<2> ps1(3);
  ps1.add_polynomial_constraint(Polynomial(x1 - 1) * (x1 - 2) == 0);
  ps1.add_polynomial_constraint(Polynomial(x1 - 1) * x2 == 0);

  Polynomial_Space<1> ps2(3);
  ps2.add_constraint(Linear_Expression(x1 - 1) == 0);

  ps1.polynomial_space_difference_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(x1 - 2) == 0);
  known_result.add_constraint(Linear_Expression(x2) == 0);

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
END_MAIN
