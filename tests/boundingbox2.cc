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
#define C_TESTS 1
#define NNC_TESTS 1

// This is a non-bounded C polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
void test_c0() {
#if C_TESTS
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test_c0 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c0 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);

  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

// This is a non-bounded NNC polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
void test_nnc0() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test_nnc0 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc0 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
 
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// This is a non-bounded C polyhedron consisting of the +ve quadrant.
void test_c1() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test_c1 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c1 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

// This is a non-bounded NNC polyhedron consisting of the +ve quadrant.
void test_nnc1() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test_nnc1 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc1 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}
 
// This is a bounded C polyhedron;
void test_c2() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test_c2 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c2 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}
 
// This is a bounded NNC polyhedron;
void test_nnc2() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test_nnc2 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc2 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
 
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}
 
// This is a unbounded C polyhedron in 4D but bounded in 2D;
void test_c3() {
#if C_TESTS
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
  print_generators(ph, "*** test_c3 ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c3 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
 
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}
 
// This is a unbounded NNC polyhedron in 4D but bounded in 2D;
void test_nnc3() {
#if NNC_TESTS
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test_nnc3 ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc3 known_ph ***");
#endif
   known_ph.intersection_assign_and_minimize(ph);
 
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// This is a universal, 2-dimensional C polyhedron. 
void test_c4() {
#if C_TESTS
  C_Polyhedron ph(2);

#if NOISY
  print_generators(ph, "*** test_c4 ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c4 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

  // This is a universal, 2-dimensional NNC polyhedron. 
void test_nnc4() {
#if NNC_TESTS
  NNC_Polyhedron ph(2);

#if NOISY
  print_generators(ph, "*** test_nnc4 ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc4 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// This is an zero-dimensional C polyhedron. 
void test_c5() {
#if C_TESTS
  C_Polyhedron ph;

#if NOISY
  print_generators(ph, "*** test_c5 ph ***");
#endif  
  
  BoundingBox box(0);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c5 known_ph ***");
#endif
  
   if (ph != known_ph)
     exit(1);

#endif // C_TESTS
}

// This is an zero-dimensional NNC polyhedron. 
void test_nnc5() {
#if NNC_TESTS
  NNC_Polyhedron ph;

#if NOISY
  print_generators(ph, "*** test_nnc5 ph ***");
#endif  
  
  BoundingBox box(0);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc5 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// This is an empty C polyhedron. 
void test_c6() {
#if C_TESTS
  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** test_c6 ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c6 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

// This is an empty NNC polyhedron. 
void test_nnc6() {
#if NNC_TESTS
  NNC_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_generators(ph, "*** test_nnc6 ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc6 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// This is a bounded C polyhedron that is a single point;
void test_c7() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

#if NOISY
  print_generators(ph, "*** test_c7 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c7 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

// This is a bounded NNC polyhedron that is a single point;
void test_nnc7() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

#if NOISY
  print_generators(ph, "*** test_nnc7 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc7 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// This is a unit square C polyhedron
void test_c8() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  C_Polyhedron ph(cs);

#if NOISY
  print_generators(ph, "*** test_c8 ph generators ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c8 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

// This is a unit square NNC polyhedron
void test_nnc8() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  NNC_Polyhedron ph(cs);

#if NOISY
  print_generators(ph, "*** test_nnc8 ph generators ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc8 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}
 
// This is a unbounded C polyhedron in 4D but bounded in 2D
void test_c9() {
#if C_TESTS
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
  print_generators(ph, "*** test_c9 ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  C_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_c9 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}

// This is a unbounded NNC polyhedron in 4D but bounded in 2D
// with strict inequality and closure points at the lower bound.
void test_nnc9() {
#if NNC_TESTS
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x +y > 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test_nnc9 ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc9 known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}
 
// This is a bounded NNC polyhedron with strict inequalities 
// causing upper and lower bounds of the box to be open.
void test_nnc10() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x < 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test_nnc10 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc known_ph ***");
#endif
  known_ph.intersection_assign_and_minimize(ph);
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}
 
// An empty polyhedron in 2D defined using strict constraints.
void test_nnc11() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x > 0);
  ph.add_constraint(x < 0);
  ph.add_constraint(y > 0);
  ph.add_constraint(y < 0);

#if NOISY
  print_generators(ph, "*** test_nnc11 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  NNC_Polyhedron known_ph(box, From_Bounding_Box());

#if NOISY
  print_generators(known_ph, "*** test_nnc11 known_ph ***");
#endif
  
  if (ph != known_ph)
    exit(1);

#endif // NNC_TESTS
}

// Constructs a polyhedron x > 0, x < 1/2, y > 0 from the corresponding box.
void test_c12() {
#if C_TESTS
  BoundingBox box(2);

  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 2);
  box.raise_lower_bound(1, true, 0, 1);
  
  C_Polyhedron ph(box, From_Bounding_Box());

#if NOISY
  print_generators(ph, "*** test_c12 ph ***");
#endif
  
  Variable x(0);
  Variable y(1);
  C_Polyhedron known_ph(2);
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(2*x <= 1);
  known_ph.add_constraint(y >= 0);

#if NOISY
  print_generators(known_ph, "*** test_c12 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);

#endif // C_TESTS
}


int
main() {

  test_c0();
  test_c1();
  test_c2();
  test_c3();
  test_c4();
  test_c5();
  test_c6();
  test_c7();
  test_c8();
  test_c9();
  test_c12();
  test_nnc0();
  test_nnc1();
  test_nnc2();
  test_nnc3();
  test_nnc4();
  test_nnc5();
  test_nnc6();
  test_nnc7();
  test_nnc8();
  test_nnc9();
  test_nnc10();
  test_nnc11();

  return 0;
}
