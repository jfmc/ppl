/* Remove the higher variables from the space.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

namespace {

bool
test01() {
  Variable x2(1);

  TOctagon oct1(3);
  oct1.add_constraint(x2 <= 3);

  print_constraints(oct1, "*** oct1 ***");

  oct1.remove_higher_space_dimensions(2);

  Octagon<mpq_class> known_result(2);
  known_result.add_constraint(x2 <= 3);

  bool ok = (Octagon<mpq_class>(oct1) == known_result);

  print_constraints(oct1, "*** oct1.remove_higher_space_dimensions(2) ***");

  return ok;
}

bool
test02() {
  Variable x2(1);
  Variable x3(2);
  Variable x5(4);

  TOctagon oc1(6);
  oc1.add_constraint(x2 - x3 <= 0);
  oc1.add_constraint(x3 <= 2);
  oc1.add_constraint(x5 <= 3);

  print_constraints(oc1, "*** oc1 ***");

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x3);
  to_be_removed.insert(x5);
  oc1.remove_space_dimensions(to_be_removed);

  Octagon<mpq_class> known_result(4);
  known_result.add_constraint(x2 <= 2);

  bool ok = (Octagon<mpq_class>(oc1) == known_result);

  print_constraints(oc1, "*** oct1.remove_space_dimensions({x3, x5}) ***");

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);

  TOctagon oct1(6);
  oct1.add_constraint(x1 >= 1);
  oct1.add_constraint(x1 + x3 >= 2);
  oct1.add_constraint(x2 - x3 <= 4);
  oct1.add_constraint(x4 - x1  >= 0);
  oct1.add_constraint(x6 <= 7);
  oct1.add_constraint(x5 + x4 >= 1);

  print_constraints(oct1, "*** oct1 ***");

  oct1.remove_higher_space_dimensions(3);

  Octagon<mpq_class> known_result(3);
  known_result.add_constraint(x1 >= 1);
  known_result.add_constraint(x1 + x3 >= 2);
  known_result.add_constraint(x2 - x3 <= 4);

  bool ok = (Octagon<mpq_class>(oct1) == known_result);

  print_constraints(oct1, "*** oct1.remove_higher_space_dimensions(3) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);
  Variable z(2);
  Variable w(6);

  // A 10-dim space, empty polyhedron.
  TOctagon oc(10, EMPTY);

  print_constraints(oc, "*** oc ***");

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  to_be_removed.insert(w);

  oc.remove_space_dimensions(to_be_removed);

  // A 7-dim space, empty polyhedron.
  Octagon<mpq_class> known_result(7, EMPTY);

  bool ok = (Octagon<mpq_class>(oc) == known_result);

  print_constraints(oc, "*** oc.remove_space_dimensions({y, z, w}) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);
  Variable z(2);
  Variable w(3);

  // A 10-dim space, empty polyhedron.
  TOctagon oc(4, EMPTY);

  print_constraints(oc, "*** oc ***");

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  to_be_removed.insert(w);
  to_be_removed.insert(x);

  oc.remove_space_dimensions(to_be_removed);

  Octagon<mpq_class> known_result(0, EMPTY);

  bool ok = (Octagon<mpq_class>(oc) == known_result);

  print_constraints(oc, "*** oc.remove_space_dimensions() ***");

  return ok;
}

bool
test06() {
  TOctagon oc(5);

  try {
    // This is an invalid use of the function
    // Octagon::remove_higher_dimensions(n): it is illegal to erase
    // a variable that is not in the space of the polyhedron.
    oc.remove_higher_space_dimensions(7);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test07() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Constraint_System cs;
  cs.insert(x <= 3);
  cs.insert(y - z <= 2);
  TOctagon oc(cs);

  Variables_Set to_be_removed;
  to_be_removed.insert(z);

  oc.remove_space_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // This is an incorrect use use of function
    // Octagon::remove_space_dimensions(to_be_remove).
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    oc.remove_space_dimensions(to_be_removed);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
END_MAIN
