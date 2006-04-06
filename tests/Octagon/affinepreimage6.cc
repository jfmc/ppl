/* Test Octagon::affine_preimage().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A >= 2);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(B + C - 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(B, expr);

  TOctagon known_result(3);
  known_result.add_constraint(A >= 2);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(B, B + C - 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A >= 2);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(-B + C - 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(B, expr);

  TOctagon known_result(3);
  known_result.add_constraint(A >= 2);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(B, -B + C - 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A >= 2);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(A - C + 3);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(A, expr);

  TOctagon known_result(3);
  known_result.add_constraint(A >= -1);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(A, A - C + 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A >= 2);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(-A - C + 3);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(A, expr);

  TOctagon known_result(3);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(A, -A - C + 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A <= -1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(B - C + 3);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(A, expr);

  TOctagon known_result(3);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(A, B - C + 3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test6() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A <= -1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(2*B - C - 1);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(B, expr);

  TOctagon known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(2*B >= 1);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(B, 2*B - C - 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test7() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A <= -1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(2*B - 2*C - 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(B, expr, 2);

  TOctagon known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(B >= 1);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(B, 2*B - 2*C - 2, 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test8() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A <= -1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(2*B - 2*C - 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(B, expr, -2);

  TOctagon known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(B, 2*B - 2*C - 2, -2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test9() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A <= -1);
  oc.add_constraint(B <= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(4*B + 6*C + 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(C, expr, 2);

  TOctagon known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(6*C >= -2);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(C, 4*B + 6*C + 2, 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(A <= -1);
  oc.add_constraint(B <= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(4*B + 6*C + 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(C, expr, -2);

  TOctagon known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(B <= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(C, 4*B + 6*C + 2, -2) ***");
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
