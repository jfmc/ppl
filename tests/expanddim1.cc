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
#define NOISY 1
#endif

Variable A(0);
Variable B(1);
Variable C(2);

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

// To be written.
static void
test3() {
}

// Test with given generators.
static void
test4() {
  C_Polyhedron ph1(2, C_Polyhedron::EMPTY);
  ph1.add_generator(point(A));
  ph1.add_generator(point(A + B));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.expand_dimension(A, 1);

  C_Polyhedron known_result = ph1;

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

  C_Polyhedron known_result = ph1;

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_dimension(A, 1) ***");
  print_generators(ph1, "***  After ph1.expand_dimension(A, 1) ***");
#endif

  if (!ok)
    exit(1);
}

// To be written.
static void
test6() {
}

// To be written.
static void
test7() {
}

// To be written.
static void
test8() {
}

// To be written.
static void
test9() {
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
  return 0;
}
CATCH
