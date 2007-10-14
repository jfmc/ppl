/* Computing the join of Polynomial_Spaces.
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
    // Polynomial_Space::join_assign:
    // it is illegal to compute the join of
    // dimension-incompatible polynomial spaces.

    ps1.join_assign(ps2);
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

  // Testing if the join is idempotent.

  Polynomial_Space<3> ps1(3);
  ps1.add_constraint(Linear_Expression(x1 + x2 - 1) == 0);
  ps1.add_constraint(Linear_Expression(x2 + x3) == 0);
  ps1.add_constraint(Linear_Expression(x3 + x1 + 1) == 0);

  Polynomial_Space<3> ps2(3);
  ps2.add_constraint(Linear_Expression(x1 + x2 - 1) == 0);
  ps2.add_constraint(Linear_Expression(x2 + x3) == 0);
  ps2.add_constraint(Linear_Expression(x3 + x1 + 1) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<3> known_result(3);
  known_result.add_constraint(Linear_Expression(x1 + x2 - 1) == 0);
  known_result.add_constraint(Linear_Expression(x2 + x3) == 0);
  known_result.add_constraint(Linear_Expression(x3 + x1 + 1) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if join(space(x1),space(x2)) = space(x1 * x2).

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression(x1) == 0);

  Polynomial_Space<2> ps2(3);
  ps2.add_constraint(Linear_Expression(x2) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_polynomial_constraint(Polynomial(x1 * x2) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test04() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if join(sp1,sp2) = sp1 if sp1 is entailed by sp2.

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression(3*x1 + 2*x2 + x3) == 0);
  ps1.add_constraint(Linear_Expression( x1  - 3*x2 + 2*x3) == 0);

  Polynomial_Space<2> ps2(ps1);
  ps2.add_polynomial_constraint(Polynomial(x2 * x2 - 1) == 0);
  ps2.add_polynomial_constraint(Polynomial(x1 * x1 - 4) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(3*x1 + 2*x2 + x3) == 0);
  known_result.add_constraint(Linear_Expression( x1  - 3*x2 + 2*x3) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test05() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // The same as test04, but exchanging the arguments.

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression(3*x1 + 2*x2 + x3) == 0);
  ps1.add_constraint(Linear_Expression( x1  - 3*x2 + 2*x3) == 0);

  Polynomial_Space<2> ps2(ps1);
  ps2.add_polynomial_constraint(Polynomial(x2 * x2 - 1) == 0);
  ps2.add_polynomial_constraint(Polynomial(x1 * x1 - 4) == 0);

  ps2.join_assign(ps1);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(3*x1 + 2*x2 + x3) == 0);
  known_result.add_constraint(Linear_Expression( x1  - 3*x2 + 2*x3) == 0);

  bool ok = (ps2 == known_result);

  return ok;
}

bool
test06() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if
  // join(space(x1),space(x1 - 1))
  // = space( x1*x1 - x*1).

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression( x1 ) == 0);

  Polynomial_Space<2> ps2(3);
  ps2.add_polynomial_constraint(Polynomial( x1 - 1 ) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_polynomial_constraint(Polynomial( x1*x1 - x1 ) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test07() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if
  // join(space(x1 - 2),space(x1 - 3))
  // = space( (x1 - 2) * (x1 - 3)).

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression( x1 - 2) == 0);

  Polynomial_Space<2> ps2(3);
  ps2.add_polynomial_constraint(Polynomial( x1 - 3 ) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_polynomial_constraint
    (Polynomial( (x1 - 2) * (x1 - 3) ) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test08() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Testing if
  // join(space(x1, x1 + x2),space(x1 - 1, x1 + x3 - 1))
  // = space( x1*x1 - x1,  x2*x3, x1*x3, x1*x2 - x2).

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression( x1 ) == 0);
  ps1.add_constraint(Linear_Expression( x1 + x2 ) == 0);

  Polynomial_Space<2> ps2(3);
  ps2.add_polynomial_constraint(Polynomial( x1 - 1 ) == 0);
  ps2.add_polynomial_constraint(Polynomial( x1 + x3 - 1 ) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<2> known_result(3);
  known_result.add_polynomial_constraint(Polynomial( x1*x1 - x1 ) == 0);
  known_result.add_polynomial_constraint(Polynomial( x2*x3      ) == 0);
  known_result.add_polynomial_constraint(Polynomial( x1*x2 - x2 ) == 0);
  known_result.add_polynomial_constraint(Polynomial( x1*x3      ) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test09() {
  
  Variable x1(0);
  Variable x2(1);

  Polynomial_Space<2> ps1(2);
  ps1.add_constraint(Linear_Expression( x1 ) == 0);
  ps1.add_constraint(Linear_Expression( x2 ) == 0);

  Polynomial_Space<2> ps2(2);
  ps2.add_constraint(Linear_Expression( 1 - x1 + x2 ) == 0);

  ps1.join_assign(ps2);

  Polynomial_Space<2> known_result(2);
  known_result.add_polynomial_constraint(Polynomial( -x1*x2 + x2*x2 + x2 ) == 0);
  known_result.add_polynomial_constraint(Polynomial( -x1*x1 + x1*x2 + x1 ) == 0);

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
