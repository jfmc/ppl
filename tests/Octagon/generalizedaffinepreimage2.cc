/* Test Octagonal_Shape::generalized_affine_preimage().
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
  Variable C(2);

  TOctagonal_Shape oct(3);
  oct.add_constraint(2*A == 1);
  oct.add_constraint(B >= 5);
  oct.add_constraint(3*C <= 7);
  oct.add_constraint(5*C >= 7);

  print_constraints(oct, "*** oct ***");

  Octagonal_Shape<mpq_class> known_result(3);
  known_result.add_constraint(2*A == 1);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  oct.generalized_affine_preimage(B, EQUAL, 3*A+2);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(B, "
		    "EQUAL, -1) ***");

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);

  TOctagonal_Shape oct(2);
  oct.add_constraint(2*A == 1);
  oct.add_constraint(B <= 5);

  print_constraints(oct, "*** oct ***");

  Octagonal_Shape<mpq_class> known_result(2);
  known_result.add_constraint(2*A == 1);

  oct.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL,
				  Linear_Expression(-1));

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, -1) ***");

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagonal_Shape oct(3);
  oct.add_constraint(2*A == 1);
  oct.add_constraint(B <= 5);
  oct.add_constraint(3*C <= 8);
  oct.add_constraint(2*C >= 7);

  print_constraints(oct, "*** oct ***");

  Octagonal_Shape<mpq_class> known_result(3, EMPTY);

  oct.generalized_affine_preimage(B, EQUAL, 3*A+2);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(B, "
		    "EQUAL, 3*A+2) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagonal_Shape oct(3);
  oct.add_constraint(x + y >= 0);
  oct.add_constraint(y >= 0);
  oct.add_constraint(z <= 2);
  oct.add_constraint(z - x >= 9);

  try {
    // This is an invalid use of the function
    // Octagonal_Shape::generalized_affine_preimage(v, e, d):
    // it is illegal to call the method with a zero denominator.
    Coefficient d = 0;
    oct.generalized_affine_preimage(y, LESS_THAN_OR_EQUAL, y + 1, d);
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
test05() {
  Variable x(0);
  Variable y(1);

  TOctagonal_Shape oct(2);
  oct.add_constraint(x >= y);

  try {
    // This is an incorrect use of the function
    // Octagonal_Shape::generalized_affine_preimage(v, r, expr, d):
    // it is illegal to use a strict relation symbol.
    oct.generalized_affine_preimage(x, LESS_THAN, x + 1);
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
test06() {
  Variable x(0);
  Variable y(1);

  TOctagonal_Shape oct(2);
  oct.add_constraint(x >= y);

  try {
    // This is an incorrect use of the function
    // Octagonal_Shape::generalized_affine_preimage(v, r, expr, d):
    // it is illegal to use a strict relation symbol.
    oct.generalized_affine_preimage(x, GREATER_THAN, x + 1);
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

  TOctagonal_Shape oct(2);
  oct.add_constraint(x >= y);

  try {
    // This is an incorrect use of the function
    // Octagonal_Shape::generalized_affine_preimage(v, r, expr, d):
    // it is illegal to pass an expression whose space dimension is
    // greather than the octagon's space dimension.
    oct.generalized_affine_preimage(y, GREATER_THAN_OR_EQUAL, z);
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
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagonal_Shape oct(2);
  oct.add_constraint(A >= 0);

  try {
    // This is an incorrect use of function
    // Octagonal_Shape::generalized_affine_preimage(v, r, expr, d):
    // it is illegal to use a variable in the 'expr' expression that
    // does not appear in the octagon.
    oct.generalized_affine_preimage(A, GREATER_THAN_OR_EQUAL, B + C);
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
test09() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<mpq_class> oct(2);
  oct.add_constraint(B - A <= 2);
  oct.add_constraint(B <= 5);

  print_constraints(oct, "*** oct ***");

  oct.generalized_affine_preimage(A, LESS_THAN_OR_EQUAL, B, 5);

  Octagonal_Shape<mpq_class> known_result(2);
  known_result.add_constraint(B <= 3);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(A, "
		    "LESS_THAN_OR_EQUAL, B, 5) ***");

  return ok;
}

bool
test10() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<mpq_class> oct(2);
  oct.add_constraint(B - A <= 2);
  oct.add_constraint(B <= 5);

  print_constraints(oct, "*** oct ***");

  oct.generalized_affine_preimage(A, LESS_THAN_OR_EQUAL, B + 3, 5);

  Octagonal_Shape<mpq_class> known_result(2);
  known_result.add_constraint(5*B <= 18);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(A, "
		    "LESS_THAN_OR_EQUAL, B + 3, 5) ***");

  return ok;
}

bool
test11() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<mpq_class> oct(2);
  oct.add_constraint(B - A >= 2);
  oct.add_constraint(B >= 1);

  print_constraints(oct, "*** oct ***");

  oct.generalized_affine_preimage(A, GREATER_THAN_OR_EQUAL, B + 3, 5);

  Octagonal_Shape<mpq_class> known_result(2);
  known_result.add_constraint(5*B >= 14);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(A, "
		    "GREATER_THAN_OR_EQUAL, B + 3, 5) ***");

  return ok;
}

bool
test12() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<mpq_class> oct(2);
  oct.add_constraint(B - A >= 2);
  oct.add_constraint(B <= 4);

  print_constraints(oct, "*** oct ***");

  oct.generalized_affine_preimage(A, GREATER_THAN_OR_EQUAL, -B + 3, 5);

  Octagonal_Shape<mpq_class> known_result(2);
  known_result.add_constraint(B <= 4);
  known_result.add_constraint(5*B >= 9);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(A, "
		    "GREATER_THAN_OR_EQUAL, -B + 3, 5) ***");

  return ok;
}

bool
test13() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<mpq_class> oct(2);
  oct.add_constraint(B - A >= 2);
  oct.add_constraint(B <= 1);

  print_constraints(oct, "*** oct ***");

  oct.generalized_affine_preimage(A, GREATER_THAN_OR_EQUAL, -B + 3, 5);

  Octagonal_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (Octagonal_Shape<mpq_class>(oct) == known_result);

  print_constraints(oct,
		    "*** oct.generalized_affine_preimage(A, "
		    "GREATER_THAN_OR_EQUAL, -B + 3, 5) ***");

  return ok;
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
  DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
END_MAIN
