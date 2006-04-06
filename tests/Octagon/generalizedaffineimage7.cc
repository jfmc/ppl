/* Test Octagon::generalized_affine_image().
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {
  Variable x(0);
  Variable y(1);
  Linear_Expression e1(x + 2);
  Linear_Expression e2(x - 2);

  TOctagon oct(2);
  oct.add_constraint(x >= 4);
  oct.add_constraint(x <= -6);
  oct.add_constraint(y == 0);

  TOctagon known_result(2, EMPTY);

  oct.generalized_affine_image(e1,LESS_THAN_OR_EQUAL, e2);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(x + 2, "
                         "LESS_THAN_OR_EQUAL, x - 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable x(0);
  Variable y(1);
  Linear_Expression e1(6);
  Linear_Expression e2(x - 4);

  TOctagon oct(2, EMPTY);
  oct.generalized_affine_image(e1, EQUAL, e2);

  TOctagon known_result(oct);


  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(6, EQUAL, x-4) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);
  Linear_Expression e1(B);
  Linear_Expression e2(1);

  TOctagon oct(2);

  oct.add_constraint(A >= 0);
  oct.add_constraint(B >= 0);

  oct.generalized_affine_image(e1, LESS_THAN_OR_EQUAL, e2);

  TOctagon known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B <= 1);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(B, "
                         "LESS_THAN_OR_EQUAL, 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);
  Linear_Expression e1(A + 5);
  Linear_Expression e2(4);

  TOctagon oct(2);

  oct.add_constraint(A == 0);
  oct.add_constraint(B >= 1);

  TOctagon known_result(oct);

  oct.generalized_affine_image(e1, EQUAL, e2);

  Linear_Expression expr(-1);
  known_result.affine_image(A, expr);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(A+5, EQUAL, 4) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable A(0);
  Variable B(1);
  Linear_Expression e1(2);
  Linear_Expression e2(B);

  TOctagon oct(2);

  oct.add_constraint(A + B == 0);
  oct.add_constraint(B <= 1);

  oct.generalized_affine_image(e1, GREATER_THAN_OR_EQUAL, e2);

  TOctagon known_result(2);
  known_result.add_constraint(A >= -1);
  known_result.add_constraint(B <= 2);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(2, "
                         "GREATER_THAN_OR_EQUAL, B) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test6() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Linear_Expression e1(C + 1);
  Linear_Expression e2(A - 3);

  TOctagon oct(3);

  oct.add_constraint(A - B == 0);
  oct.add_constraint(B <= 1);
  oct.add_constraint(C + A <= 2);

  oct.generalized_affine_image(e1, LESS_THAN_OR_EQUAL, e2);

  TOctagon known_result(3);

  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(C <= -3);
  known_result.add_constraint(A + B <= 2);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(A - C >= 4);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(C + 1, "
                         "LESS_THAN_OR_EQUAL, A - 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test7() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Linear_Expression e1(C + 1);
  Linear_Expression e2(A + 2);

  TOctagon oct(3);

  oct.add_constraint(A - B == 0);
  oct.add_constraint(B <= 1);
  oct.add_constraint(C + A <= 2);

  TOctagon known_result(oct);

  oct.generalized_affine_image(e1, EQUAL, e2);

  known_result.affine_image(C, A + 1);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(C+1,EQUAL,A+2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test8() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Linear_Expression e1(B - 1);
  Linear_Expression e2(A - 2);

  TOctagon oct(3);

  oct.add_constraint(A - B == 0);
  oct.add_constraint(B <= 1);
  oct.add_constraint(C + A <= 2);

  oct.generalized_affine_image(e1, GREATER_THAN_OR_EQUAL, e2);

  TOctagon known_result(3);
  known_result.add_constraint(B - A >= -1);
  known_result.add_constraint(C + A <= 2);
  known_result.add_constraint(A <= 1);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(B - 1, "
                         "GREATER_THAN_OR_EQUAL, A - 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test9() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Linear_Expression e1(-C + 3);
  Linear_Expression e2(-B + 4);

  TOctagon oct(3);

  oct.add_constraint(A - B == 0);
  oct.add_constraint(B <= 1);
  oct.add_constraint(C + A <= 2);

  oct.generalized_affine_image(e1, LESS_THAN_OR_EQUAL, e2);

  TOctagon known_result(3);
  known_result.add_constraint(C >= B - 1);
  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(C - A >= -1);
  known_result.add_constraint(A <= 1);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(-C + 3, "
                         "LESS_THAN_OR_EQUAL, -B + 4) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Linear_Expression e1(-C + 3);
  Linear_Expression e2(-B + 4);

  TOctagon oct(3);

  oct.add_constraint(A - B == 0);
  oct.add_constraint(B <= 1);
  oct.add_constraint(C + A <=2);

  TOctagon known_result(oct);
  known_result.add_constraint(C <= B - 1);
  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(C + A <= 1);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(B + A <= 2);

  oct.generalized_affine_image(e1, GREATER_THAN_OR_EQUAL, e2);

  bool ok = (oct == known_result);

#if NOISY
  print_constraints(oct, "*** oct.generalized_affine_image(-C + 3, "
                         "GREATER_THAN_OR_EQUAL, -B + 4) ***");
#endif

  if (!ok)
    exit(1);
}

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

  return 0;
}
CATCH
