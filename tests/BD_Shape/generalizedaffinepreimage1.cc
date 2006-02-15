/* Test BD_Shape::generalized_affine_preimage().
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
#include <limits>

namespace {

bool
test1() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL,
				 Linear_Expression(-1));

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(2*A == 1);

  bool ok = check_result(bd, known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, -1) ***");

  return ok;
}

bool
test2() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, Linear_Expression(-1));

  BD_Shape<mpq_class> known_result(2, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -1) ***");

  return ok;
}

bool
test3() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(4*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -B+1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(4*A == 1);
  known_result.add_constraint(-B >= 4);

  bool ok = check_result(bd, known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -B+1) ***");

  return ok;
}

bool
test4() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(4*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, B+1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(4*A == 1);
  known_result.add_constraint(B >= 4);

  bool ok = check_result(bd, known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, B+1) ***");

  return ok;
}

bool
test5() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(7*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, 2*B+1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(7*A == 1);
  known_result.add_constraint(B >= 2);

  bool ok = check_result(bd, known_result, "2.13e-8", "1.25e-8", "8.52e-9");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, 2*B+1) ***");

  return ok;
}

bool
test6() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(5*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -2*B+1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(5*A == 1);
  known_result.add_constraint(-B >= 2);

  bool ok = check_result(bd, known_result, "2.69e-8", "1.72e-8", "1.20e-8");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -2*B+1) ***");

  return ok;
}

bool
test7() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, 3*A-2*B+1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(-2*B >= 3);

  bool ok = check_result(bd, known_result, "2.89e-7", "1.85e-7", "1.40e-7");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, 3*A-2*B+1) ***");

  return ok;
}

bool
test8() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(B >= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B+1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(-2*B >= 5);

  bool ok = check_result(bd, known_result, "5.27e-7", "3.53e-7", "2.59e-7");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B+1) ***");

  return ok;
}

bool
test9() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, 2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*B <= 14);
  known_result.add_constraint(3*B - 3*A <= 13);
  known_result.add_constraint(3*B - 3*C <= 7);
  known_result.add_constraint(3*C <= 7);

  bool ok = check_result(bd, known_result, "5.48e-6", "2.97e-6", "1.77e-6");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, 2) ***");

  return ok;
}

bool
test10() {
  // If the Coefficient type is not wide enough, do nothing.
  if (std::numeric_limits<Coefficient>::is_bounded
      && (std::numeric_limits<Coefficient>::min() > -203
	  || std::numeric_limits<Coefficient>::max() < 629))
    return true;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, 3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(20*B <= -203);
  known_result.add_constraint(60*A - 60*B >= 629);
  known_result.add_constraint(20*C - 20*B >= 231);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bool ok = check_result(bd, known_result, "4.48e-6", "2.36e-6", "1.36e-6");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, 3) ***");

  return ok;
}

bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*C <= 7);

  bool ok = check_result(bd, known_result, "3.68e-7", "2.41e-7", "1.79e-7");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2) ***");

  return ok;
}

bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, -3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(12*B >= -35);
  known_result.add_constraint(4*A - 4*B <= 13);
  known_result.add_constraint(4*C - 4*B <= 21);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bool ok = check_result(bd, known_result, "5.13e-6", "2.73e-6", "1.67e-6");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, -3) ***");

  return ok;
}

bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B <= 7);
  bd.add_constraint(3*C <= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*B <= 35);
  known_result.add_constraint(3*B - 3*A <= 34);
  known_result.add_constraint(3*B - 3*C <= 28);
  known_result.add_constraint(3*C <= 7);

  bool ok = check_result(bd, known_result, "7.39e-6", "4.07e-6", "2.55e-6");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2) ***");

  return ok;
}

bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B <= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(20*B <= 27);
  known_result.add_constraint(60*B - 60*A <= 61);
  known_result.add_constraint(20*C - 20*B >= 1);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bool ok = check_result(bd, known_result, "1.98e-6", "9.26e-7", "5.25e-7");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3) ***");

  return ok;
}

bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bool ok = check_result(bd, known_result, "4.26e-7", "2.44e-7", "1.79e-7");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test1);
  NEW_TEST(test2);
  NEW_TEST(test3);
  NEW_TEST(test4);
  NEW_TEST(test5);
  NEW_TEST(test6);
  NEW_TEST(test7);
  NEW_TEST(test8);
  NEW_TEST(test9);
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
  NEW_TEST(test13);
  NEW_TEST(test14);
  NEW_TEST(test15);
END_MAIN
