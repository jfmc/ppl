/* Remove higher space dimensions from the space.
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

  try {
    // This is an invalid use of function
    // Polynomial_Space::remove_higher_space_dimensions:
    // removing a variable which is not dimension-compatible.

    ps1.remove_higher_space_dimensions(3);
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
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);

  Polynomial_Space<2> ps1(6);
  ps1.add_constraint(Linear_Expression(x2 - x3) == 0);
  ps1.add_constraint(Linear_Expression(   x3  ) == 2);
  ps1.add_constraint(Linear_Expression(x6 - x5) == 2);
  ps1.add_constraint(Linear_Expression(   x5  ) == 3);

  ps1.remove_higher_space_dimensions(4);

  Polynomial_Space<2> known_result(4);
  known_result.add_constraint(Linear_Expression(x2 - x3) == 0);
  known_result.add_constraint(Linear_Expression(   x3  ) == 2);

  bool ok = (ps1 == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
END_MAIN
