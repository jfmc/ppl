/* Test Polyhedron::expand_dimension().
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);
Variable E(4);

// Test with a universe polyhedron.
static void
test1() {
  C_Polyhedron ph1(3);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 1);

  C_Polyhedron known_result(4);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(A, 1) ***");
#endif

  if (!ok)
    exit(1);
}

// Test with an empty polyhedron.
static void
test2() {
  C_Polyhedron ph1(3, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(B, 1);

  C_Polyhedron known_result(4, C_Polyhedron::EMPTY);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(B, 1) ***");
#endif

  if (!ok)
    exit(1);
}

// Test trivial expansion.
static void
test3() {
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A + B <= 2);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 0);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A + B <= 2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(A, 0) ***");
#endif

  if (!ok)
    exit(1);
}

// Test with given generators.
static void
test4() {
  C_Polyhedron ph1(2, C_Polyhedron::EMPTY);
  ph1.add_generator(point(A));
  ph1.add_generator(point(A + B));
  ph1.add_generator(point(B));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 1);

  C_Polyhedron known_result(3, C_Polyhedron::EMPTY);
  known_result.add_generator(point(A + C));
  known_result.add_generator(point(A + B));
  known_result.add_generator(point(A + B + C));
  known_result.add_generator(point(B));
  known_result.add_generator(point(B + C));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "***  After ph1.expand_dimension(A, 1) ***");
#endif

  if (!ok)
    exit(1);
}

// Test with given constraints.
static void
test5() {
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A + B <= 2);
#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 1);

  C_Polyhedron known_result(3);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A + B <= 2);
  known_result.add_constraint(C >= 0);
  known_result.add_constraint(C + B <= 2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(A, 1) ***");
#endif

  if (!ok)
    exit(1);
}

// Test using constraints expanding 2 dimensions.
static void
test6() {
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A + B <= 2);
#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 2);

  C_Polyhedron known_result(4);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A + B <= 2);
  known_result.add_constraint(C >= 0);
  known_result.add_constraint(C + B <= 2);
  known_result.add_constraint(D >= 0);
  known_result.add_constraint(D + B <= 2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(A, 2) ***");
#endif

  if (!ok)
    exit(1);
}

// Test using constraints for NNC polyhedron.
static void
test7() {

  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A - B > 2);
  ph1.add_constraint(A + 2*B < 6);
  ph1.add_constraint(B < 6);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(B, 2);

  NNC_Polyhedron known_result(4);
  known_result.add_constraint(A - B > 2);
  known_result.add_constraint(A + 2*B < 6);
  known_result.add_constraint(B < 6);
  known_result.add_constraint(A - C > 2);
  known_result.add_constraint(A + 2*C < 6);
  known_result.add_constraint(C < 6);
  known_result.add_constraint(A - D > 2);
  known_result.add_constraint(A + 2*D < 6);
  known_result.add_constraint(D < 6);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(B, 2) ***");
#endif

  if (!ok)
    exit(1);
}

// Test using constraints with equality constraint.
static void
test8() {
  C_Polyhedron ph1(3);
  ph1.add_constraint(A <= 1);
  ph1.add_constraint(C == 1);
  ph1.add_constraint(A + B >= 1);
  ph1.add_constraint(B <= 1);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 1);
  ph1.expand_dimension(C, 1);

  C_Polyhedron known_result(5);
  known_result.add_constraint(A <= 1);
  known_result.add_constraint(A + B >= 1);
  known_result.add_constraint(C == 1);
  known_result.add_constraint(E == 1);
  known_result.add_constraint(B <= 1);
  known_result.add_constraint(D <= 1);
  known_result.add_constraint(D + B >= 1);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "***  After ph1.expand_dimension(A, 1); ph1.expand_dimension(C, 1) ***");
#endif

  if (!ok)
    exit(1);
}

// Test using generators for NNC polyhedron.
static void
test9() {
  NNC_Polyhedron ph1(2, NNC_Polyhedron::EMPTY);
  ph1.add_generator(point(A));
  ph1.add_generator(closure_point(A + B));
  ph1.add_generator(ray(A - B));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 2);

  NNC_Polyhedron known_result(4, NNC_Polyhedron::EMPTY);
  known_result.add_generator(point(A + C + D));
  known_result.add_generator(ray(A -B + C + D));
  known_result.add_generator(closure_point(A + C + 2*D));
  known_result.add_generator(closure_point(A + 2*C + D));
  known_result.add_generator(closure_point(A + 2*C + 2*D));
  known_result.add_generator(closure_point(A + B + C + D));
  known_result.add_generator(closure_point(2*A + C + D));
  known_result.add_generator(closure_point(2*A + C + 2*D));
  known_result.add_generator(closure_point(2*A + 2*C + D));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "***  After ph1.expand_dimension(A, 2) ***");
#endif

  if (!ok)
    exit(1);
}

// Test as given in GopanDMDRS04 on page 519.
static void
test10() {
  C_Polyhedron ph1(2, C_Polyhedron::EMPTY);
  ph1.add_generator(point(A + 2*B));
  ph1.add_generator(point(A + 3*B));
  ph1.add_generator(point(A + 4*B));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(B, 1);

  C_Polyhedron known_result(3, C_Polyhedron::EMPTY);
  known_result.add_generator(point(A + 2*B + 2*C));
  known_result.add_generator(point(A + 2*B + 3*C));
  known_result.add_generator(point(A + 2*B + 4*C));
  known_result.add_generator(point(A + 3*B + 2*C));
  known_result.add_generator(point(A + 3*B + 3*C));
  known_result.add_generator(point(A + 3*B + 4*C));
  known_result.add_generator(point(A + 4*B + 2*C));
  known_result.add_generator(point(A + 4*B + 3*C));
  known_result.add_generator(point(A + 4*B + 4*C));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "***  After ph1.expand_dimension(A, 2) ***");
#endif

  if (!ok)
    exit(1);
}

int
main() TRY {
  set_handlers();

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
