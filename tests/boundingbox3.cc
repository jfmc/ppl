/* Test Polyhedron::bounding_box().
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

// This is a non-bounded closed polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
void test0() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test0 ph ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test0 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);
}

// This is a non-bounded closed polyhedron consisting of the +ve quadrant.
void test1() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test1 ph ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test1 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);
}

// This is a bounded closed polyhedron;
void test2() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test2 ph ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test2 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);
}

// This is a unbounded closed polyhedron in 4D but bounded in 2D;
void test3() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph(4);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test3 ph ***");
#endif

  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test3 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);
}

// This is a universal, 2-dimensional closed polyhedron.
void test4() {
  C_Polyhedron ph(2);

#if NOISY
  print_generators(ph, "*** test4 ph ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test4 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);
}

// This is an zero-dimensional closed polyhedron.
void test5() {
  C_Polyhedron ph;

#if NOISY
  print_generators(ph, "*** test5 ph ***");
#endif

  BoundingBox box(0);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test5 known_ph ***");
#endif

   if (ph != known_ph)
     exit(1);
}

// This is an empty closed polyhedron.
void test6() {
  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** test6 ph ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test6 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is a bounded closed polyhedron that is a single point;
void test7() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

#if NOISY
  print_generators(ph, "*** test7 ph ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test7 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is a unit square closed polyhedron
void test8() {
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);

  C_Polyhedron ph(cs);

#if NOISY
  print_generators(ph, "*** test8 ph generators ***");
#endif

  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test8 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is a unbounded closed polyhedron in 4D but bounded in 2D
void test9() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph(4);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test9 ph ***");
#endif

  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test9 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);
}

// Constructs a polyhedron x >= 0, x <= 1/2, y >= 0
// from the corresponding box.
void test10() {
  BoundingBox box(2);

  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 2);
  box.raise_lower_bound(1, true, 0, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

#if NOISY
  print_generators(ph, "*** test12 ph ***");
#endif

  Variable x(0);
  Variable y(1);
  C_Polyhedron known_ph(2);
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(2*x <= 1);
  known_ph.add_constraint(y >= 0);

#if NOISY
  print_generators(known_ph, "*** test12 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

int
main() {
  set_handlers();

  test0();
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
