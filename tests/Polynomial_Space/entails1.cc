/* Checking entailment for Polynomial_Spaces.
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
  Polynomial_Space<2> ps2(3);

  try {
    // This is an invalid use of function
    // Polynomial_Space::entails:
    // it is illegal to check dimension-incompatible
    // polynomial spaces for containment.
    ps1.entails(ps2);
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

  // Checking that space(x1) is entailed by the bottom polynomial
  // space.

  Polynomial_Space<3> ps1(3);
  ps1.add_constraint(Linear_Expression(x1) == 0);

  Polynomial_Space<3> ps2(3, Polynomial_Space<3>::BOTTOM);

  bool known_result = true;

  bool ok = (ps2.entails(ps1) == known_result);

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Checking that space(x1 + x2) is entailed by space(x1, x2).

  Polynomial_Space<3> ps1(3);
  ps1.add_constraint(Linear_Expression(x1 + x2) == 0);

  Polynomial_Space<3> ps2(3);
  ps2.add_constraint(Linear_Expression(x1) == 0);
  ps2.add_constraint(Linear_Expression(x2) == 0);

  bool known_result = true;

  bool ok = (ps2.entails(ps1) == known_result);

  return ok;
}

bool
test04() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Checking that space(x1 + x3) is not entailed by space(x1, x2).

  Polynomial_Space<3> ps1(3);
  ps1.add_constraint(Linear_Expression(x1 + x3) == 0);

  Polynomial_Space<3> ps2(3);
  ps2.add_constraint(Linear_Expression(x1) == 0);
  ps2.add_constraint(Linear_Expression(x2) == 0);

  bool known_result = false;

  bool ok = (ps2.entails(ps1) == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
  DO_TEST(test03)
  DO_TEST(test04)
END_MAIN
