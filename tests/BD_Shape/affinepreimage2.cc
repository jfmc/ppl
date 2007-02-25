/* Test BD_Shape::affine_preimage().
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 4*B + 6*C + 2, -2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 4*B + 6*C + 2, -2) ***");

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 2*A + 3*C + 2, 2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A <= -2);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 2*A + 3*C + 2, 2) ***");

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 2*A + 3*C + 2, 2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A <= -2);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, -3*A + C - 1, 2) ***");

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 3*A + C - 1, -2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 3*A + C - 1, -2) ***");

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, -3*A + C - 1, -2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, -3*A + C - 1, -2) ***");

  return ok;
}

bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd(4);
  bd.add_constraint(A == 2);
  bd.add_constraint(B == 0);
  bd.add_constraint(C >= -3);
  bd.add_constraint(D <= 5);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(D, 4*A - B + 2*C + 5*D - 1, 3);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A == 2);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C >= -3);
  known_result.add_constraint(5*D <= 14);

  bool ok = check_result(bd, known_result, "7.63e-7", "3.82e-7", "1.91e-7");

  print_constraints(bd, "*** bd.affine_preimage"
		        "(D, 4*A - B + 2*C + 5*D - 1, 3) ***");

  return ok;
}

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(B, -B);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(B, -B) ***");

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  BD_Shape<mpq_class> known_result(bd);

  bd.affine_preimage(B, -B, -1);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(B, -B, -1) ***");

  return ok;
}

bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(B >= 2);
  known_result.add_constraint(C >= 0);

  bd.affine_preimage(B, -3*B + 6, 3);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(B, -3*B + 6, 3) ***");

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
END_MAIN

