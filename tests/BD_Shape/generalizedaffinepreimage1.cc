/* Test BD_Shape::generalized_affine_preimage().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

void
test1() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B >= 5);

  TBD_Shape known_result(2);
  known_result.add_constraint(2*A == 1);

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL,
				 Linear_Expression(-1));

  bool ok = (bd == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, -1) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B >= 5);

  TBD_Shape known_result(2, EMPTY);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, Linear_Expression(-1));

  bool ok = (bd == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -1) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(4*A == 1);
  bd.add_constraint(B >= 5);

  TBD_Shape known_result(2);
  known_result.add_constraint(4*A == 1);
  known_result.add_constraint(-B >= 4);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -B+1);

  bool ok = (bd == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -B+1) ***");

  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(4*A == 1);
  bd.add_constraint(B >= 5);

  TBD_Shape known_result(2);
  known_result.add_constraint(4*A == 1);
  known_result.add_constraint(B >= 4);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, B+1);

  bool ok = (bd == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, B+1) ***");

  if (!ok)
    exit(1);
}

void
test5() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(7*A == 1);
  bd.add_constraint(B >= 5);

  TBD_Shape known_result(2);
  known_result.add_constraint(7*A == 1);
  known_result.add_constraint(B >= 2);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, 2*B+1);

  bool ok = (bd == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, 2*B+1) ***");

  if (!ok)
    exit(1);
}

void
test6() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(5*A == 1);
  bd.add_constraint(B >= 5);

  TBD_Shape known_result(2);
  known_result.add_constraint(5*A == 1);
  known_result.add_constraint(-B >= 2);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -2*B+1);

  bool ok = (bd == known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -2*B+1) ***");

  if (!ok)
    exit(1);
}

void
test7() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(B >= 5);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(-2*B >= 3);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, 3*A-2*B+1);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, 3*A-2*B+1) ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 1);
  }

  if (!ok)
    exit(1);
}

void
test8() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(B >= 5);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(-2*B >= 5);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B+1);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B+1) ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 1);
  }

  if (!ok)
    exit(1);
}

void
test9() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*B <= 14);
  known_result.add_constraint(3*B - 3*A <= 13);
  known_result.add_constraint(3*B - 3*C <= 7);
  known_result.add_constraint(3*C <= 7);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, 2);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, 2) ***");

  print_constraints(known_result, "*** known_result ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 8);
  }

  if (!ok)
    exit(1);
}

void
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(20*B <= -203);
  known_result.add_constraint(60*A - 60*B >= 629);
  known_result.add_constraint(20*C - 20*B >= 231);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, 3);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, 3) ***");

  print_constraints(known_result, "*** known_result ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 9);
  }

  if (!ok)
    exit(1);
}

void
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*C <= 7);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2) ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 1);
  }

  if (!ok)
    exit(1);
}

void
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(12*B >= -35);
  known_result.add_constraint(4*A - 4*B <= 13);
  known_result.add_constraint(4*C - 4*B <= 21);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bd.generalized_affine_preimage(B, LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, -3);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "LESS_THAN_OR_EQUAL, -3*A-2*B-7*C+1, -3) ***");

  print_constraints(known_result, "*** known_result ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 12);
  }

  if (!ok)
    exit(1);
}

void
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B <= 7);
  bd.add_constraint(3*C <= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*B <= 35);
  known_result.add_constraint(3*B - 3*A <= 34);
  known_result.add_constraint(3*B - 3*C <= 28);
  known_result.add_constraint(3*C <= 7);

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2);

  TBD_Shape T_known_result(known_result);

  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, -3*A-2*B+7*C+1, -2) ***");

  print_constraints(known_result, "*** known_result ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 8);
  }

  if (!ok)
    exit(1);
}

void
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B <= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(20*B <= 27);
  known_result.add_constraint(60*B - 60*A <= 61);
  known_result.add_constraint(20*C - 20*B >= 1);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3);

  TBD_Shape T_known_result(known_result);

  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3) ***");

  print_constraints(known_result, "*** known_result ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 9);
  }

  if (!ok)
    exit(1);
}

void
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(3*A == 1);
  bd.add_constraint(2*B >= 7);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*A == 1);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, 3*A-2*B-7*C+1, -3) ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 1);
  }

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();
  test11();
  test12();
  test13();
  test14();
  test15();

  return 0;
}
CATCH
