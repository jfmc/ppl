/* Adding constraints to a Polynomial_Space.
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

  Polynomial_Space<1> ps1(2);

  try {
    // This is an invalid use of function
    // Polynomial_Space::add_polynomial_constraint:
    // it is illegal to add a dimension-incompatible constraint.
    ps1.add_constraint(Linear_Expression(x1 - x3) == 0);
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

  // Testing if adding 1 == 0 yields the bottom polynomial space.

  Polynomial_Space<3> ps1(3);
  ps1.add_constraint(Linear_Expression(1) == 0);

  Polynomial_Space<3> known_result(3, Polynomial_Space<3>::BOTTOM);

  bool ok = (ps1 == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
END_MAIN
