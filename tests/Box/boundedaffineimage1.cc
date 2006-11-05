/* Test Box::bounded_affine_image().
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
  Variable x(0);
  Variable y(1);

  TBox box1(3);
  box1.add_constraint(x <= 2);
  box1.add_constraint(x - y <= 3);
  box1.add_constraint(y <= 2);

  print_constraints(box1, "*** box1 ***");

  box1.bounded_affine_image(x, y, y);

  Box<mpq_class> known_result(3);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(x - y == 0);

  bool ok = (Box<mpq_class>(box1) == known_result);

  print_constraints(box1, "*** box1.bounded_affine_image(x, y, y) ***");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);

  TBox box1(3);
  box1.add_constraint(x <= 2);
  box1.add_constraint(x - y <= 3);
  box1.add_constraint(y <= 2);

  print_constraints(box1, "*** box1 ***");

  box1.bounded_affine_image(x, x + 4, x + 4);

  Box<mpq_class> known_result(3);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(x - y <= 7);
  known_result.add_constraint(x <= 6);

  bool ok = (Box<mpq_class>(box1) == known_result);

  print_constraints(box1, "*** box1.bounded_affine_image(x, x + 4, x + 4) ***");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);

  TBox box1(3);
  box1.add_constraint(x <= 2);
  box1.add_constraint(x - y <= 3);
  box1.add_constraint(y <= 2);

  print_constraints(box1, "*** box1 ***");

  box1.bounded_affine_image(x, Linear_Expression(4), Linear_Expression(4));

  Box<mpq_class> known_result(3);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(x == 4);

  bool ok = (Box<mpq_class>(box1) == known_result);

  print_constraints(box1, "*** box1.bounded_affine_image(x, 4, 4) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);

  TBox box1(3);
  box1.add_constraint(x <= 2);
  box1.add_constraint(x - y <= 3);
  box1.add_constraint(y <= 2);

  print_constraints(box1, "*** box1 ***");

  Box<mpq_class> known_result(box1);

  box1.bounded_affine_image(x, x, x);

  bool ok = (Box<mpq_class>(box1) == known_result);

  print_constraints(box1, "*** box1.bounded_affine_image(x, x, x) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);

  TBox box1(3);
  box1.add_constraint(x <= 2);
  box1.add_constraint(x - y <= 3);
  box1.add_constraint(y <= 2);

  print_constraints(box1, "*** box1 ***");

  box1.bounded_affine_image(x, 2*x - 2, 2*x - 2, 2);

  Box<mpq_class> known_result(3);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(x - y <= 2);

  bool ok = (Box<mpq_class>(box1) == known_result);

  print_constraints(box1,
		    "*** box1.bounded_affine_image(x, 2*x-2, 2*x-2, 2) ***");

  return ok;
}

bool
test06() {
  Variable x(0);
  Variable y(1);

  TBox box1(3);
  box1.add_constraint(x <= 2);
  box1.add_constraint(x - y <= 3);
  box1.add_constraint(y <= 2);

  print_constraints(box1, "*** box1 ***");

  box1.bounded_affine_image(y, 2*x, 2*x, 2);

  Box<mpq_class> known_result(3);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(y - x == 0);

  bool ok = (Box<mpq_class>(box1) == known_result);

  print_constraints(box1, "*** box1.bounded_affine_image(y, 2*x, 2*x, 2) ***");

  return ok;
}

bool
test07() {
  Variable x(0);
  Variable y(1);

  TBox box(2);
  box.add_constraint(x <= 1);
  box.add_constraint(x >= 0);
  box.add_constraint(y <= 2);
  box.add_constraint(y >= -1);

  print_constraints(box, "*** box ***");

  box.bounded_affine_image(x, -2*x + y + 1, -2*x + y + 1);

  Box<mpq_class> known_result(2);
  known_result.add_constraint(x <= 3);
  known_result.add_constraint(x >= -2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);
  known_result.add_constraint(x - y <= 1);
  known_result.add_constraint(x - y >= -1);

  bool ok = (Box<mpq_class>(box) == known_result);

  print_constraints(box,
		    "*** box.bounded_affine_image(x, -2*x+y+1, -2*x+y+1) ***");

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBox box(5);
  box.add_constraint(A >= 0);
  box.add_constraint(A <= 4);
  box.add_constraint(B <= 5);
  box.add_constraint(A - B <= 0);
  box.add_constraint(B - C == 2);
  box.add_constraint(C - A <= -2);

  print_constraints(box, "*** box ***");

  box.bounded_affine_image(B, Linear_Expression(-1), D + E);

  Box<mpq_class> known_result(5);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 4);
  known_result.add_constraint(A - C == 2);
  known_result.add_constraint(C >= -2);
  known_result.add_constraint(C <= 2);
  known_result.add_constraint(B >= -1);

  bool ok = (Box<mpq_class>(box) == known_result);

  print_constraints(box, "*** box.bounded_affine_image(B, -1, D + E) ***");

  return ok;
}

bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBox box(5);
  box.add_constraint(A - B == 0);
  box.add_constraint(B <= 1);
  box.add_constraint(C - A <= 2);

  print_constraints(box, "*** box ***");

  box.bounded_affine_image(C, 3*D - E, 2*C + 1, 5);

  Box<mpq_class> known_result(5);
  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(5*C <= 7);
  known_result.add_constraint(A <= 1);

  bool ok = check_result(box, known_result, "9.54e-8", "9.54e-8", "9.54e-8");

  print_constraints(box, "*** box.bounded_affine_image(C, "
                        "3*D - E, 2*C + 1, 5) ***");

  return ok;
}

bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable F(5);

  TBox box(6);
  box.add_constraint(A <= 4);
  box.add_constraint(A >= -6);
  box.add_constraint(B == 0);

  print_constraints(box, "*** box ***");

  box.bounded_affine_image(A, -A - 3, B - C + 6*D + F);

  Box<mpq_class> known_result(6);
  known_result.add_constraint(A >= -7);
  known_result.add_constraint(B == 0);

  bool ok = (Box<mpq_class>(box) == known_result);

  print_constraints(box, "*** box.bounded_affine_image(A, "
		        "-A - 3, B - C + 6*D + F) ***");

  return ok;
}

bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBox box(5);
  box.add_constraint(A - B == 0);
  box.add_constraint(B <= 1);
  box.add_constraint(C - A <= 2);

  print_constraints(box, "*** box ***");

  box.bounded_affine_image(B, -B - 2, 7*D - E + 5, 3);

  Box<mpq_class> known_result(5);
  known_result.add_constraint(B >= -1);
  known_result.add_constraint(C - A <= 2);
  known_result.add_constraint(A <= 1);

  bool ok = (Box<mpq_class>(box) == known_result);

  print_constraints(box, "*** box.bounded_affine_image(B, "
                        "-B - 2, 7*D - E + 5, 3) ***");

  return ok;
}

bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBox box(5);
  box.add_constraint(A - B == 0);
  box.add_constraint(B <= 1);
  box.add_constraint(C - A <= 2);

  print_constraints(box, "*** box ***");

  box.bounded_affine_image(B, 3*E - 5*D + A - 3*B, 4*A -2*C + 3, -3);

  Box<mpq_class> known_result(5);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(C - A <= 2);

  bool ok = (Box<mpq_class>(box) == known_result);

  print_constraints(box, "*** box.bounded_affine_image(B, "
                        "3*E - 5*D + A - 3*B, 4*A - 2*C + 3, -3) ***");

  return ok;
}

bool
test13() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBox box(2);
  box.add_constraint(x >= y);

  try {
    // This is an incorrect use of the method
    // Box::bounded_affine_image(v, lb_expr, ub_expr, d): it is illegal
    // to apply it to an expression whose space dimension is
    // greater than the box's space dimension.
    box.bounded_affine_image(y, x, z);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test14() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBox box(2);
  box.add_constraint(x >= y);

  try {
    // This is an incorrect use of the method
    // Box::bounded_affine_image(v, lb_expr, ub_expr, d): it is illegal
    // to apply it to an expression whose space dimension is
    // greater than the box's space dimension.
    box.bounded_affine_image(y, z, x);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test15() {
  Variable x(0);
  Variable y(1);

  TBox box(2);
  box.add_constraint(x >= y);

  try {
    // This is an incorrect use of the method
    // Box::bounded_affine_image(v, lb_expr, ub_expr, d): it is illegal
    // to apply it to a expression with the denominator
    // equal to zero.
    Coefficient d = 0;
    box.bounded_affine_image(x, Linear_Expression(0), x + 1, d);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test16() {
  Variable x(0);
  Variable y(1);

  TBox box(1);
  box.add_constraint(x >= 1);

  try {
    // This is an invalid used of the method
    // Box::bounded_affine_image(v, lb_expr, ub_epxr, d): it is illegal to
    // apply the method to a variable that is not in the space of
    // the polyhedron.
    box.bounded_affine_image(y, x + 1, Linear_Expression(8));
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
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
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
END_MAIN
