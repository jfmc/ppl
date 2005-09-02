/* Test BD_Shape::generalized_affine_image().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {
  Variable x(0);
  Variable y(1);
 
  TBD_Shape bd(2);
  bd.add_constraint(x <= 4);
  bd.add_constraint(x >= -6);
  bd.add_constraint(y == 0);

  bd.generalized_affine_image(y, LESS_THAN_OR_EQUAL, -y + 1);

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 4);
  known_result.add_constraint(x >= -6);
  known_result.add_constraint(y <= 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(y, " 
                         "LESS_THAN_OR_EQUAL, -y + 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x <= 4);
  bd.add_constraint(x >= -6);
  bd.add_constraint(y == 0);

  bd.generalized_affine_image(x, GREATER_THAN_OR_EQUAL, -x - 3);

  TBD_Shape known_result(2);
  known_result.add_constraint(x >= -7);
  known_result.add_constraint(y == 0);
 
  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(x, GREATER_THAN_OR_EQUAL, -x - 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);

  bd.add_constraint(A >= 0);
  bd.add_constraint(B <= 1);

  bd.generalized_affine_image(B, LESS_THAN_OR_EQUAL, 3*B + 1, 2);

  TBD_Shape known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B <= 2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(B, " 
                         "LESS_THAN_OR_EQUAL, 3*B + 1, 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);

  bd.add_constraint(A == 0);
  bd.add_constraint(B >= 1);

  TBD_Shape known_result(2);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B >= -1);

  bd.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, B - 2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, B - 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);

  bd.add_constraint(B <= 1);
  bd.add_constraint(A - B == 0);
  
  bd.generalized_affine_image(A, GREATER_THAN_OR_EQUAL, 2*A + 3, 2);
  
  TBD_Shape known_result(2);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(B - A <= -1);
 
  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(A, " 
		    "GREATER_THAN_OR_EQUAL, 2*A + 3, 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test6() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);

  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);

  bd.generalized_affine_image(C, LESS_THAN_OR_EQUAL, 2*C + 1, 5);

  TBD_Shape known_result(3);

  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(C <= 2);
  known_result.add_constraint(A <= 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(C, "
                         "LESS_THAN_OR_EQUAL, 2*C + 1, 5) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test7() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);

  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);

  TBD_Shape known_result(bd);

  bd.generalized_affine_image(C, EQUAL, 5*C - 3, 4);

  known_result.affine_image(C, 5*C - 3, 4);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(C, EQUAL, 5*C - 3, 4) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test8() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  
  TBD_Shape bd(3);
  
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);
  
  bd.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, -B - 2, 3);

  TBD_Shape known_result(3);
  known_result.add_constraint(B >= -1);
  known_result.add_constraint(C - A <= 2);
  known_result.add_constraint(A <= 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(B, "
                         "GREATER_THAN_OR_EQUAL, -B - 2, 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test9() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  
  TBD_Shape bd(3);
  
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <= 2);
  
  bd.generalized_affine_image(B, LESS_THAN_OR_EQUAL, 4*A -2*C + 3, -3);

  TBD_Shape known_result(3);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(C - A <= 2);
  
  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(B, "
                         "LESS_THAN_OR_EQUAL, 4*A - 2*C + 3, -3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);

  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(C - A <=2);

  TBD_Shape known_result(bd);

  bd.generalized_affine_image(B, EQUAL, 2*A - 4*B + C + 3, 3);

  known_result.affine_image(B, 2*A - 4*B + C + 3, 3);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(B, "
		         "EQUAL, 2*A - 4*B + C + 3, 3) ***");
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
