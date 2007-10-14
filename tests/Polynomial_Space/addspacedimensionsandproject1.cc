/* Adding space dimensions and then projecting Polynomial Spaces.
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

  Polynomial_Space<2> ps1
    (Polynomial_Space<2>::max_space_dimension());

  try {
    // This is an invalid use of function
    // Polynomial_Space::add_space_dimensions_and_project:
    // it is illegal to add more dimensions than allowed by
    // max_space_dimension().

    ps1.add_space_dimensions_and_project(1);
  }
  catch (std::length_error& e) {
    nout << "length_error: " << e.what() << endl;
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

  // Testing if adding 1 extra space dimension to space<1>(x1 - 1) by
  // projection yields space<1>(x1 - 1, x2).

  Polynomial_Space<1> ps1(1);
  ps1.add_constraint(Linear_Expression(x1 - 1) == 0);

  ps1.add_space_dimensions_and_project(1);

  Polynomial_Space<1> known_result(2);
  known_result.add_constraint(Linear_Expression(x1 - 1) == 0);
  known_result.add_constraint(Linear_Expression(x2) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);

  // Testing if adding 2 extra space dimension to space<2>(x1 - 1, x2 + 1) by
  // projection yields space<2>(x1 - 1, x2 + 1, x3 , x4).

  Polynomial_Space<2> ps1(2);
  ps1.add_constraint(Linear_Expression(x1 - 1) == 0);
  ps1.add_constraint(Linear_Expression(x2 + 1) == 0);

  ps1.add_space_dimensions_and_project(2);

  Polynomial_Space<2> known_result(4);
  known_result.add_constraint(Linear_Expression(x1 - 1) == 0);
  known_result.add_constraint(Linear_Expression(x2 + 1) == 0);
  known_result.add_constraint(Linear_Expression(x3) == 0);
  known_result.add_constraint(Linear_Expression(x4) == 0);

  bool ok = (ps1 == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
  DO_TEST(test03)
END_MAIN
