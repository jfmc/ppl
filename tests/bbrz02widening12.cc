/* Test Polyhedron::BBRZ02_widening_assign(): the third technique
   of this function is tested.
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

void
test1() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(A));
  gs1.insert(ray(A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(A));
  gs2.insert(ray(A + 2*B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test2() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-A));
  gs1.insert(ray(-A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-A));
  gs2.insert(ray(-A + 2*B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 0);
  known_result.add_constraint(B >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test3() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-A));
  gs1.insert(ray(-A - B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-A));
  gs2.insert(ray(-A - 2*B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 0);
  known_result.add_constraint(B <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test4() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(A));
  gs1.insert(ray(A - B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(A));
  gs2.insert(ray(A - 2*B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test5() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(B));
  gs1.insert(ray(A + 2*B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(B));
  gs2.insert(ray(A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test6() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(B));
  gs1.insert(ray(-A + 2*B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(B));
  gs2.insert(ray(-A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 0);
  known_result.add_constraint(B >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test7() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-B));
  gs1.insert(ray(-A - 2*B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-B));
  gs2.insert(ray(-A - B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 0);
  known_result.add_constraint(B <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test8() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-B));
  gs1.insert(ray(A - 2*B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-B));
  gs2.insert(ray(A - B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test9() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(A));
  gs1.insert(ray(A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(A));
  gs2.insert(ray(-A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(B >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test10() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(B));
  gs1.insert(ray(-A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(B));
  gs2.insert(ray(-A - B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test11() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-A));
  gs1.insert(ray(-A - B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-A));
  gs2.insert(ray(A - B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(B <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test12() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-B));
  gs1.insert(ray(A - B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-B));
  gs2.insert(ray(A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test13() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-A));
  gs1.insert(ray(-A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-A));
  gs2.insert(ray(A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(B >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test14() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(-B));
  gs1.insert(ray(-A - B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(-B));
  gs2.insert(ray(-A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test15() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(A));
  gs1.insert(ray(A - B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(A));
  gs2.insert(ray(-A - B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(B <= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

void
test16() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(B));
  gs1.insert(ray(A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point());
  gs2.insert(ray(B));
  gs2.insert(ray(A - B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.BBRZ02_widening_assign(ph1);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);

  bool equal = (ph2 == known_result);

#if NOISY
  print_generators(ph2, "*** After ph2.BBRZ02_widening_assign(ph1) ***");
#endif

  if (!equal)
    exit(1);
}

int
main() {

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
  test11();
  test12();
  test13();
  test14();
  test15();

  return 0;
}
