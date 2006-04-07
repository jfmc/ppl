/* Test Octagon::affine_image(): we apply this function with
   the denominator different from 1.
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
#define NOISY 1
#endif

static void
tests1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(C >= 1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(A + B >= 2);

  Linear_Expression expr(C + B);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_image(A, expr, 1);

  Octagon<mpq_class> known_result(3);
  known_result.add_constraint(A >= 1);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 1);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_image(A, C + B, 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
tests2() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(C <= 1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(A + B >= 2);

  Linear_Expression expr(C + B);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_image(A, expr, 1);

  Octagon<mpq_class> known_result(3);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C <= 1);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_image(A, C + B, 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
tests3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(C <= 1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(A + B >= 2);
  oc.add_constraint(A >= 2);

  Linear_Expression expr(-A);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_image(A, expr, 1);

  Octagon<mpq_class> known_result(3);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C <= 1);
  known_result.add_constraint(A <= -2);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_image(A, -A, 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
tests4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc(3);
  oc.add_constraint(C <= 1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(A + B <= 2);
  oc.add_constraint(-A + B <= 1);
  oc.add_constraint(A >= 2);

  Linear_Expression expr(-A);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif

  oc.affine_image(A, expr, 1);

  Octagon<mpq_class> known_result(3);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C <= 1);
  known_result.add_constraint(A <= -2);
  known_result.add_constraint(-A + B <= 2);
  known_result.add_constraint(A + B <= 1);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_image(A, -A, 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
tests5() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TOctagon oc(4);
  oc.add_constraint(C <= 1);
  oc.add_constraint(B >= 0);
  oc.add_constraint(A + C <= 3);
  oc.add_constraint(A <= 2);
  oc.add_constraint(A >= 1);
  oc.add_constraint(D >= 1);
  oc.add_constraint(D <= 2);

  Linear_Expression expr(-A + 2*D);

#if NOISY
  print_constraints(oc, "*** oc ***");
#endif
  C_Polyhedron ph(oc.constraints());
  ph.affine_image(A, expr, 1);

  oc.affine_image(A, expr, 1);

  Octagon<mpq_class> known_result(4);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C <= 1);
  known_result.add_constraint(D >= 1);
  known_result.add_constraint(D <= 2);
  known_result.add_constraint(A <= 3);
  known_result.add_constraint(A >= 0);

  bool ok = (oc == known_result);

#if NOISY
  print_constraints(oc, "*** oc.affine_image(A, -A + D, 1) ***");
  print_constraints(ph, "*** ph.affine_image(A, -A + D, 1) ***");
#endif

  if (!ok)
    exit(1);
}



int
main() TRY {
//   tests1();
//   tests2();
//   tests3();
//   tests4();
  tests5();


  return 0;
}
CATCH
