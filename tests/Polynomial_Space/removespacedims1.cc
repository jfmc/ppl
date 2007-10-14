/* Remove some variables from the space.
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

  Variables_Set to_be_removed;
  to_be_removed.insert(x3);

  try {
    // This is an invalid use of function
    // Polynomial_Space::remove_space_dimensions:
    // removing a variable which is not dimension-compatible.

    ps1.remove_space_dimensions(to_be_removed);
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

  // Removing just a single variable, the last one.

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression( x2 - x3) == 0);
  ps1.add_constraint(Linear_Expression(    x3  ) == 2);

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x3);

  ps1.remove_space_dimensions(to_be_removed);

  Polynomial_Space<2> known_result(2);
  known_result.add_constraint(Linear_Expression(x2) == 2);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  // Removing just a single variable, not the last one.

  Polynomial_Space<2> ps1(3);
  ps1.add_constraint(Linear_Expression( x2 - x3 - x1) == 0);
  ps1.add_constraint(Linear_Expression(    x1  ) == 2);

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x1);

  ps1.remove_space_dimensions(to_be_removed);

  Polynomial_Space<2> known_result(2);
  known_result.add_constraint(Linear_Expression(x1 - x2) == 2);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test04() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);

  // Removing two variables, not the last ones.

  Polynomial_Space<2> ps1(6);
  ps1.add_constraint(x2 - x3 == 0);
  ps1.add_constraint(Linear_Expression(x3) == 2);
  ps1.add_constraint(x6 - x5 == 2);
  ps1.add_constraint(Linear_Expression(x5) == 3);

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x3);
  to_be_removed.insert(x5);

  ps1.remove_space_dimensions(to_be_removed);

  Polynomial_Space<2> known_result(4);
  known_result.add_constraint(Linear_Expression(x2) == 2);
  known_result.add_constraint(Linear_Expression(x4) == 5);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test05() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);

  // Removing all variables.

  Polynomial_Space<3> ps1(6);
  ps1.add_constraint(x2 - x3 == 0);
  ps1.add_constraint(Linear_Expression(x3) == 2);
  ps1.add_constraint(x6 - x5 == 2);
  ps1.add_constraint(Linear_Expression(x5) == 3);

  Variables_Set to_be_removed;
  to_be_removed.insert(x1);
  to_be_removed.insert(x2);
  to_be_removed.insert(x3);
  to_be_removed.insert(x4);
  to_be_removed.insert(x5);
  to_be_removed.insert(x6);

  ps1.remove_space_dimensions(to_be_removed);

  Polynomial_Space<3> known_result(0);

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
END_MAIN
