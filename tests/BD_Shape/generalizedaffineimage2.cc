/* Test BD_Shape::generalized_affine_image().
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

  TBD_Shape bd(2);
  bd.add_constraint(x <= 4);
  bd.add_constraint(x >= -6);
  bd.add_constraint(y == 0);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(y, LESS_THAN_OR_EQUAL, -y + 1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x <= 4);
  known_result.add_constraint(x >= -6);
  known_result.add_constraint(y <= 1);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(y, "
                        "LESS_THAN_OR_EQUAL, -y + 1) ***");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x <= 4);
  bd.add_constraint(x >= -6);
  bd.add_constraint(y == 0);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(x, GREATER_THAN_OR_EQUAL, -x - 3);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x >= -7);
  known_result.add_constraint(y == 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(x, "
		        "GREATER_THAN_OR_EQUAL, -x - 3) ***");

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B <= 1);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(B, LESS_THAN_OR_EQUAL, 3*B + 1, 2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B <= 2);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(B, "
		        "LESS_THAN_OR_EQUAL, 3*B + 1, 2) ***");

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A == 0);
  bd.add_constraint(B >= 1);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, B - 2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B >= -1);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(B, "
		        "GREATER_THAN_OR_EQUAL, B - 2) ***");

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(B <= 1);
  bd.add_constraint(A - B == 0);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(A, GREATER_THAN_OR_EQUAL, 2*A + 3, 2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(2*B - 2*A <= -3);

  bool ok = check_result(bd, known_result, "7.63e-17", "3.82e-17", "1.91e-17");

  print_constraints(bd, "*** bd.generalized_affine_image(A, "
		        "GREATER_THAN_OR_EQUAL, 2*A + 3, 2) ***");

  return ok;
}

bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(C, LESS_THAN_OR_EQUAL, 2*C + 1, 5);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(5*C <= 7);
  known_result.add_constraint(A <= 1);

  bool ok = check_result(bd, known_result, "9.54e-8", "9.54e-8", "9.54e-8");

  print_constraints(bd, "*** bd.generalized_affine_image(C, "
                        "LESS_THAN_OR_EQUAL, 2*C + 1, 5) ***");

  return ok;
}

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);

  print_constraints(bd, "*** bd ***");

  BD_Shape<mpq_class> known_result(bd);

  bd.generalized_affine_image(C, EQUAL, 5*C - 3, 4);

  known_result.affine_image(C, 5*C - 3, 4);

  bool ok = check_result(bd, known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(C, "
		        "EQUAL, 5*C - 3, 4) ***");

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, -B - 2, 3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(B >= -1);
  known_result.add_constraint(C - A <= 2);
  known_result.add_constraint(A <= 1);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(B, "
                        "GREATER_THAN_OR_EQUAL, -B - 2, 3) ***");

  return ok;
}

bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(B, LESS_THAN_OR_EQUAL, 4*A -2*C + 3, -3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(C - A <= 2);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(B, "
                        "LESS_THAN_OR_EQUAL, 4*A - 2*C + 3, -3) ***");

  return ok;
}

bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <=2);

  print_constraints(bd, "*** bd ***");

  BD_Shape<mpq_class> known_result(bd);

  bd.generalized_affine_image(B, EQUAL, 2*A - 4*B + C + 3, 3);

  known_result.affine_image(B, 2*A - 4*B + C + 3, 3);

  bool ok = check_result(bd, known_result);

  print_constraints(bd, "*** bd.generalized_affine_image(B, "
		        "EQUAL, 2*A - 4*B + C + 3, 3) ***");

  return ok;
}

bool
test11() {
  Variable A(0);
  Variable B(1);
  Linear_Expression e1(A);
  Linear_Expression e2(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(A <= 4);
  bd.add_constraint(B <= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(e1, EQUAL, e2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A == 1);
  known_result.add_constraint(B <= 5);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result) ;

  print_constraints(bd, "*** bd.generalized_affine_image(A, EQUAL, 1) ***");

  return ok;
}

bool
test12() {
  Variable A(0);
  Variable B(1);
  Linear_Expression e1(B - 3);
  Linear_Expression e2(B + 1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(A <= 4);
  bd.add_constraint(B <= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(e1, EQUAL, e2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 4);
  known_result.add_constraint(B <= 9);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result) ;

  print_constraints(bd, "*** bd.generalized_affine_image(B-3,EQUAL, B+1) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
  NEW_TEST(test09);
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
END_MAIN
