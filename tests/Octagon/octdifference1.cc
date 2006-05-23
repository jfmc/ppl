/* Test Octagonal_Shape::oct_difference_assign(): if `oct1' is
   contained in `oct2', the result of `oct1.oct_difference_assign(oct2)'
   is an empty octagon.
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
  Variable A(0);
  Variable B(1);

  TOctagonal_Shape oct1(2);
  oct1.add_constraint(A >= 0);
  oct1.add_constraint(A <= -2);
  oct1.add_constraint(B == 0);

  print_constraints(oct1, "*** oct1 ***");

  TOctagonal_Shape oct2(2);
  oct2.add_constraint(A >= 0);
  oct2.add_constraint(A <= 2);
  oct2.add_constraint(B >= 0);
  oct2.add_constraint(B <= 2);

  print_constraints(oct2, "*** oct2 ***");

  oct1.oct_difference_assign(oct2);

  Octagonal_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (Octagonal_Shape<mpq_class>(oct1) == known_result);

  print_constraints(oct1, "*** oct1.oct_difference_assign(oc2) ***");

  return ok;
}

bool
test02() {
  TOctagonal_Shape oct1(0);
  TOctagonal_Shape oct2(0);

  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");

  oct1.oct_difference_assign(oct2);

  Octagonal_Shape<mpq_class> known_result(0, EMPTY);

  bool ok = (Octagonal_Shape<mpq_class>(oct1) == known_result);

  print_constraints(oct1, "*** oct1.intersection_assign(oc2) ***");

  return ok;
}

bool
test03() {
  Variable A(0);

  TOctagonal_Shape oct1(1);
  oct1.add_constraint(A >= 0);
  oct1.add_constraint(A <= 7);

  print_constraints(oct1, "*** oct1 ***");

  TOctagonal_Shape oct2(1);
  oct2.add_constraint(A == 5);

  print_constraints(oct2, "*** oct2 ***");

  oct1.oct_difference_assign(oct2);

  Octagonal_Shape<mpq_class> known_result(1);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 7);

  bool ok = (Octagonal_Shape<mpq_class>(oct1) == known_result);

  print_constraints(oct1, "*** oct1.intersection_assign(oc2) ***");

  return ok;
}

bool
test04() {
  TOctagonal_Shape oc1(3);
  TOctagonal_Shape oc2(5);

  try {
    // This is an incorrect use of function
    // Octagonal_Shape::oct_difference_assign(oc2): it is impossible to apply
    // this function to two polyhedra of different dimensions.
    oc1.oct_difference_assign(oc2);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return true;
  }
  catch (...) {
    return false;
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
END_MAIN
