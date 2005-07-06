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

  TBD_Shape bd(2);
  bd.add_constraint(x == 4);
  bd.add_constraint(y <= 0);

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(y <= 0);
 
  bd.generalized_affine_image(3*x + 2, LESS_THAN_OR_EQUAL, 2*x - 3);
  
  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd,
		    "*** bd.generalized_affine_image(3*x + 2, "
		    "LESS_THAN_OR_EQUAL, 2*x - 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x - y <= 7);
  bd.add_constraint(y >= 1);

  TBD_Shape known_result(bd);
  known_result.affine_image(x, Linear_Expression(10), 3); 

  bd.generalized_affine_image(Linear_Expression(6), EQUAL, 3*x - 4);
 
  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd,
		    "*** bd.generalized_affine_image(6, EQUAL, 3*x - 4) ***");
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
  bd.add_constraint(B >= 0);

  bd.generalized_affine_image(2*B + 3*A,
			      LESS_THAN_OR_EQUAL, Linear_Expression(1));
 
  TBD_Shape known_result(2);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(3*A <= 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(3*A + 2*B, "
                        "LESS_THAN_OR_EQUAL, 1) ***");
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

  TBD_Shape known_result(bd);

  bd.generalized_affine_image(-2*A + 5, EQUAL, -4*B);

  known_result.affine_image(A, 4*B + 5, 2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd,
		    "*** bd.generalized_affine_image(-2*A + 5, "
		    "EQUAL, -4*B) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A - B == 0);
  bd.add_constraint(B <= 1);

  bd.generalized_affine_image(A + 2*B - 5, GREATER_THAN_OR_EQUAL, 3*B);
 
  TBD_Shape known_result(2);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(A - B >= 1);

   bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(A + 2*B - 5, "
                        "GREATER_THAN_OR_EQUAL, 3*B) ***");
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
 
  bd.generalized_affine_image(2*B + C + 1, LESS_THAN_OR_EQUAL, A - 3*B + 2*C);

  TBD_Shape known_result(3);

  known_result.add_constraint(A <= 1);
  known_result.add_constraint(C - A <= 2);
 
  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd,
		    "*** bd.generalized_affine_image(2*B + C + 1, "
		    "LESS_THAN_OR_EQUAL, A - 3*B + 2*C) ***");
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

  bd.generalized_affine_image(2*B + C + 1,
			      GREATER_THAN_OR_EQUAL, A - 3*B + 2*C);

  TBD_Shape known_result(3);

  known_result.add_constraint(A <= 1);
  known_result.add_constraint(A - C >= -2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(2*B + C + 1, "
		        "GREATER_THAN_OR_EQUAL, A - 3*B + 2*C) ***");
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

  bd.generalized_affine_image(-2*A - B - 1,
			      GREATER_THAN_OR_EQUAL, 3*A + B + 4*C - 2);
  
  TBD_Shape known_result(3);
  known_result.add_constraint(B - C >= -2);
  known_result.add_constraint(B <= 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(-2*A - B - 1, "
                        "GREATER_THAN_OR_EQUAL, 3*A + B + 4*C - 2) ***");
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

  bd.generalized_affine_image(-2*C + 3, LESS_THAN_OR_EQUAL, -3*B + 4);
  
  TBD_Shape known_result(3);
  known_result.add_constraint(A - B == 0);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(A <= 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image(-2*C + 3, "
                        "LESS_THAN_OR_EQUAL, -3*B + 4) ***");
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
  bd.add_constraint(C + A <=2);

  TBD_Shape known_result(3, EMPTY);
 
  bd.generalized_affine_image(Linear_Expression(3),
			      GREATER_THAN_OR_EQUAL,
			      Linear_Expression(4));

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd,
		    "*** bd.generalized_affine_image(3, "
		    "GREATER_THAN_OR_EQUAL, 4) ***");
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
