/* Testing Polyhedron::bounding_box().
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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"
#include <iostream>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 1
#define C_TESTS 1
#define NNC_TESTS 1

// This is a non-bounded C polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
void test0() {
#if C_TESTS
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test0 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test0 ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);

   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}

  // This is a non-bounded NNC polyhedron consisting of the line x = y.
  // The bounding box is the xy plane - the universal polyhedron.
void test0a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test0a ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test0a ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
 
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

  // This is a non-bounded C polyhedron consisting of the +ve quadrant.
void test1() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test1 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test1 ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}

  // This is a non-bounded NNC polyhedron consisting of the +ve quadrant.
void test1a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test1a ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test1a ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}
 
  // This is a bounded C polyhedron;
void test2() {
#if C_TESTS
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

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test2 ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}
 
  // This is a bounded NNC polyhedron;
void test2a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test2a ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test2a ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
 
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

  // This is a bounded C polyhedron that is a single point;
void test2b() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

#if NOISY
  print_generators(ph, "*** test2b ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test3b ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}
 
  // This is a unbounded C polyhedron in 4D but bounded in 2D;
void test3() {
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
  print_generators(ph, "*** test3 ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test3 ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
 
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}
 
  // This is a unbounded NNC polyhedron in 4D but bounded in 2D;
void test3a() {
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
  print_generators(ph, "*** test3a ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test3a ph1 ***");
#endif
   ph1.intersection_assign_and_minimize(ph);
 
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

  // This is a universal, 2-dimensional C polyhedron. 
void test4() {
#if C_TESTS
  C_Polyhedron ph(2);

#if NOISY
  print_generators(ph, "*** test4 ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test4 ph1 ***");
#endif
   ph1.intersection_assign_and_minimize(ph);
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}

  // This is a universal, 2-dimensional NNC polyhedron. 
void test4a() {
#if NNC_TESTS
  NNC_Polyhedron ph(2);

#if NOISY
  print_generators(ph, "*** test4a ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test4a ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

  // This is an zero-dimensional C polyhedron. 
void test5() {
#if C_TESTS
  C_Polyhedron ph;

#if NOISY
  print_generators(ph, "*** test5 ph ***");
#endif  
  
  BoundingBox box(0);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test5 ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}

  // This is an zero-dimensional NNC polyhedron. 
void test5a() {
#if TEST5a
  NNC_Polyhedron ph;

#if NOISY
  print_generators(ph, "*** test5a ph ***");
#endif  
  
  BoundingBox box(0);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test5a ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

  // This is an empty C polyhedron. 
void test6() {
#if C_TESTS
  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** test6 ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test6 ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}

  // This is an empty NNC polyhedron. 
void test6a() {
#if NNC_TESTS
  NNC_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_generators(ph, "*** test6a ph ***");
#endif  
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test6a ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

// This is a unit square C polyhedron
void test10() {
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
  print_generators(ph, "*** test10 ph generators ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  C_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test10 ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //C_TESTS
}

// This is a unit square NNC polyhedron
void test10a() {
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
  print_generators(ph, "*** test10a ph generators ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test10a ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}
 
  // This is a unbounded NNC polyhedron in 4D but bounded in 2D
  // with strict inequality and closure points at the lower bound.
void test11() {
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
  print_generators(ph, "*** test11 ph ***");
#endif
  
  BoundingBox box(4);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test11 ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}

 
  // This is a bounded NNC polyhedron with strict inequalities 
  // causing upper and lower bounds of the box to be open.
void test12() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x < 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test12 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test12 ph1 ***");
#endif
  ph1.intersection_assign_and_minimize(ph);
  
   if ((ph != ph1))
     exit(1);

#endif //NNC_TESTS
}
 
void test20() {
  BoundingBox box(2);

  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 2);
  box.raise_lower_bound(1, true, 0, 1);

  //#if NOISY
  //box.print_box("*** test0 box ***");
  //#endif
  
  From_Bounding_Box dummy;

  C_Polyhedron ph(dummy, box);

#if NOISY
  print_generators(ph, "*** test20 ph ***");
#endif
  
  Variable x(0);
  Variable y(1);
  C_Polyhedron known_ph(2);
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(2*x <= 1);
  known_ph.add_constraint(y >= 0);

#if NOISY
  print_generators(known_ph, "*** test20 known_ph ***");
#endif

   if ((ph != known_ph))
     exit(1);

}

void test21() {
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x > 0);
  ph.add_constraint(x < 0);
  ph.add_constraint(y > 0);
  ph.add_constraint(y < 0);

#if NOISY
  print_generators(ph, "*** test21 ph ***");
#endif
  
  BoundingBox box(2);
  ph.shrink_bounding_box(box);

  From_Bounding_Box dummy;

  NNC_Polyhedron ph1(dummy, box);

#if NOISY
  print_generators(ph1, "*** test21 ph1 ***");
#endif
  
   if ((ph != ph1))
     exit(1);

}


int
main() {


  test0();
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test10();
  test0a();
  test1a();
  test2a();
  test3a();
  test4a();
  test5a();
  test6a();
  test10a();
  test11();
  test12();
  test20();
  test21();
  return 0;
}
