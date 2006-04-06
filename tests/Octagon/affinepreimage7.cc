/* Test Octagon::affine_preimage().
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

  oc.affine_preimage(A, expr, -2);

  TOctagon known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(A, 4*B + 6*C + 2, -2) ***");
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
  oc.add_constraint(A <= -1);
  oc.add_constraint(B <= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(-2*A + 3*C + 2);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(A, expr, 2);

  TOctagon known_result(3);
  known_result.add_constraint(A >= 2);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(A, -2*A + 3*C + 2, 2) ***");
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
  oc.add_constraint(A <= -1);
  oc.add_constraint(B <= 0);
  oc.add_constraint(C >= 0);

  Linear_Expression expr(3*A + C - 1);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(A, expr, 2);

  TOctagon known_result(3);
  known_result.add_constraint(A <= 0);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(A, 3*A + C - 1, 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TOctagon oc(4);
  oc.add_constraint(A == 2);
  oc.add_constraint(B == 0);
  oc.add_constraint(C >= -3);
  oc.add_constraint(D <= 5);

  Linear_Expression expr(4*A - B + 2*C + 5*D - 1);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_preimage(D, expr, 3);

  TOctagon known_result(4);
  known_result.add_constraint(A == 2);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C >= -3);
  known_result.add_constraint(D <= 3);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_preimage(D, 4*A - B + 2*C + 5*D - 1, 3) ***");
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

  return 0;
}
CATCH


