/* Some incorrect uses of the functions of PPL.
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
error1() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x - y > 0);
  cs.insert(x >= 0);
  
  try {
    // This is an invalid use of the constructor of a polyhedron:
    // it is illegal to built a closed polyhedron starting from
    // a system of constraints that contains strict-inequalities.
    C_Polyhedron ph(cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error2() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  
  GenSys gs;
  gs.insert(closure_point(x + y));
  gs.insert(point(x + y));
  gs.insert(ray(x));
  gs.insert(ray(y));
  
  try {
    // This is an invalid use of the constructor of a polyhedron:
    // it is illegal to built a closed polyhedron starting from
    // a system of generators that contains closure points.
    C_Polyhedron ph(gs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error3() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3);
  
  try {
    // This is an invalid use of the function add_constraint(c): it is
    // illegal to insert a strict-inequality into a system of
    // constraints of a closed polyhedron.
    ph.add_constraint(x - y > 0);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_constraint: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error4() {
  set_handlers();
  
  C_Polyhedron ph(3, C_Polyhedron::EMPTY);

  try {
    // This is an incorrect use of the function add_generator(g): it
    // is illegal to insert a closure-point into a system of
    // generators of a closed polyhedron.
    ph.add_generator(closure_point(LinExpression(2)));
  }
  catch(invalid_argument& e) {
# if NOISY
    cout << "invalid_generator: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error5() {
  set_handlers();
  
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3);
  ph.add_constraint(x >= 2);
  ph.add_constraint(y >= 2);

  ConSys cs;
  cs.insert(x == y);
  cs.insert(x < 5);

  try {
    // This is an incorrect use of the function
    // add_constraints_and_minimize(cs): it is illegal to add a system of
    // constraints that contains strict-inequalities to a closed polyhedron.
    ph.add_constraints_and_minimize(cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error6() {
  set_handlers();
  
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3);
  ConSys cs;
  cs.insert(x > 3);
  cs.insert(x > y);

  try {
    // This is an invalid use of the function add_constraints(cs):
    // it is illegal to add a system of constraints that constains
    // strict-inequalities to a closed polyhedron.
    ph.add_constraints(cs);
  }
  catch(invalid_argument& e) {
#if NOISY   
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error7() {
  set_handlers();
  
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3);

  ConSys cs;
  cs.insert(x > 2);
  cs.insert(x == y);
  NNC_Polyhedron qh(cs);

  try {
    // This is an incorrect use of the method concatenate_assign(): it
    // is illegal to apply this method to a closed polyhedron with a
    // NNC Polyhedron.
    ph.concatenate_assign(qh);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error8() {
  set_handlers();
  
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3);
  GenSys gs;
  gs.insert(point());
  gs.insert(closure_point(-x));
  gs.insert(ray(x));
  gs.insert(ray(y));

  try {
    // This is an incorrect use of the function
    // add_generators_and_minimize(gs): it is illegal to add a
    // system of generators that constains closure-points to a closed
    // polyhedron.
    ph.add_generators_and_minimize(gs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error9() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3);
  GenSys gs;
  gs.insert(point(x));
  gs.insert(closure_point());
  gs.insert(ray(x+y));

  try {
    // This is an incorrect use of the function add_generators(gs): it is
    // illegal to add a system of generators that contains closure-points
    // to a closed polyhedron.
    ph.add_generators(gs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error10() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  
  NNC_Polyhedron ph1(3);
  ph1.add_constraint(x >= 5);
  ph1.add_constraint(y > x - 3);

  try {
    // It is illegal to built a closed polyhedron starting from
    // the system of constraints of a polyhedron that is not closed.
    C_Polyhedron ph2(ph1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedron: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}


void
error11() {
  set_handlers();
  
  Variable A(0);
  Variable B(1);
  
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 2);

  NNC_Polyhedron ph2(2);
  ph2.add_constraint(A - B > 0);
  ph2.add_constraint(A >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif
  try {
    // This is an invalid use of the function
    // `intersection_assign_and_minimize': it is illegal to apply
    // to a closed polyhedron and a non-closed polyhedron.
    ph1.intersection_assign_and_minimize(ph2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error12() {
  set_handlers();
  
  Variable A(0);
  Variable B(1);
  
  C_Polyhedron ph1(2);
  ph1.add_constraint(B >= 2);

  NNC_Polyhedron ph2(2);
  ph2.add_constraint(A - B > 0);
  ph2.add_constraint(B >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif
  try {
    // This is an invalid use of the function
    // `intersection_assign': it is illegal to apply this function
    // to a closed polyhedron and a non-closed polyhedron.
    ph1.intersection_assign(ph2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}


void
error13() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  GenSys gs1;
  gs1.insert(point());
  gs1.insert(point(3*A));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point(B));
  gs2.insert(closure_point());
  gs2.insert(closure_point(3*B));
  NNC_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif
  
   try {
     // This is an invalid use of the function
     // `poly_hull_assign_and_minimize': it is illegal to apply
     // this function to a closed polyhedron and a
     // non-closed polyhedron.
    ph1.poly_hull_assign_and_minimize(ph2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error14() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  GenSys gs1;
  gs1.insert(point());
  gs1.insert(point(3*B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point(2*A));
  gs2.insert(closure_point());
  gs2.insert(closure_point(3*A));
  NNC_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif
  
   try {
    // This is an invalid use of the function
    // `poly_hull_assign': it is illegal to apply this function
    // to a closed polyhedron and a non-closed polyhedron.
    ph1.poly_hull_assign(ph2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error15() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A <= 4);
  ph1.add_constraint(B >= 0);
  ph1.add_constraint(B <= 4);

  NNC_Polyhedron ph2(2);
  ph2.add_constraint(A >= 2);
  ph2.add_constraint(A <= 6);
  ph2.add_constraint(B >= 0);
  ph2.add_constraint(B <= 4);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif

  try {
    // This is an invalid use of the function
    // `poly_difference_assign': it is illegal to apply this function
    // to a closed polyhedron and a non-closed polyhedron.
    ph1.poly_difference_assign(ph2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error16() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A <= 2);
  ph1.add_constraint(A - B >= 0);

  NNC_Polyhedron ph2(2);
  ph2.add_constraint(A >= 0);
  ph2.add_constraint(A <= 4);
  ph2.add_constraint(A - B >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif

  try {
    // This is an invalid use of the function
    // `H79_widening_assign': it is illegal to apply this function
    // to a closed polyhedron and a non-closed polyhedron.
    ph2.H79_widening_assign(ph1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}    

void
error17() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A <= 2);
  ph1.add_constraint(A - B >= 0);

  C_Polyhedron ph2(2);
  ph2.add_constraint(A >= 0);
  ph2.add_constraint(A <= 4);
  ph2.add_constraint(A - B >= 0);

  ConSys cs;
  cs.insert(A <= 8);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
  print_constraints(cs, "*** cs ***");
#endif

  try {
    // This is an invalid use of the function
    // `limited_H79_widening_assign': it is illegal to
    // apply this function to a closed polyhedron and
    // a non-closed polyhedron.
    ph2.limited_H79_widening_assign(ph1, cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}    

void
error18() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A <= 2);
  ph1.add_constraint(A - B >= 0);

  C_Polyhedron ph2(2);
  ph2.add_constraint(A >= 0);
  ph2.add_constraint(A <= 4);
  ph2.add_constraint(A - B >= 0);

  ConSys cs;
  cs.insert(A < 8);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
  print_constraints(cs, "*** cs ***");
#endif

  try {
    // This is an invalid use of the function
    // `limited_H79_widening_assign': it is illegal to
    // apply this function to two closed polyhedra and
    // to a non-closed system of constraints.
    ph2.limited_H79_widening_assign(ph1, cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}    

void
error19() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(B >= 0);
  ph1.add_constraint(A + B <= 1);

  GenSys gs;
  gs.insert(point(2*A + 2*B));
  gs.insert(ray(A + B));
  NNC_Polyhedron ph2(gs);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

 try {
    // This is an invalid use of the function
    // `time_elapse_assign': it is illegal to
    // apply this function to a closed polyhedron and
    // a non-closed polyhedron.
    ph1.time_elapse_assign(ph2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}
  
void
error20() {
  set_handlers();

  Variable A(0);

  C_Polyhedron ph1(1);
  ph1.add_constraint(A >= 5);

  NNC_Polyhedron ph2(1);
  ph2.add_constraint(A > 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif
  
  try {
    // This is an invalid use of the `operator<=':
    // it is illegal to apply this function to a
    // closed polyhedron and a non-closed polyhedron.
    ph1 <= ph2;
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error21() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  
  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A - B > 0);

  const ConSys cs = ph1.constraints();

#if NOISY
  print_constraints(cs, "*** cs ***");
#endif
  
  try {
    // This is an incorrect use of the function
    // C_Polyhedron::C_Polyhedron(cs): it is illegal to built a
    // closed polyhedron starting from a system of constraints
    // that contains strict inequalities.
    C_Polyhedron ph2(cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}   

void
error22() {
  set_handlers();

  Variable A(0);

  GenSys gs1;
  gs1.insert(point(3*A));
  gs1.insert(closure_point(2*A));
  gs1.insert(ray(A));

  NNC_Polyhedron ph1(gs1);

  const GenSys gs2 = ph1.generators();

  try {
    // This is an incorrect use of the function
    // `C_Polyhedron(const GenSys)': it is illegal to built
    // a closed polyhedron starting from a constant non-closed
    // system of generators.
    C_Polyhedron ph2(gs2);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error23() {
  set_handlers();

  NNC_Polyhedron ph(0, NNC_Polyhedron::EMPTY);

  try {
    // This is an incorrect use of the function
    // `add_generator(g)': it is illegal add a closure point
    // to a zero-dimensional and empty non-closed polyhedron.
    ph.add_generator(closure_point());
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_generator: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error24() {
  set_handlers();
  
  NNC_Polyhedron ph(0, NNC_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  GenSys gs;
  gs.insert(closure_point());

  try {
    // This is an invalid used of the function
    // `add_generators_and_minimize(gs)': it is illegal to
    // add a system of generators that does not contain points
    // to an empty zero-dimensional polyhedron.
    ph.add_generators_and_minimize(gs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error25() {
  set_handlers();
  
  NNC_Polyhedron ph(0, NNC_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  GenSys gs;
  gs.insert(closure_point());

  try {
    // This is an invalid used of the function
    // `add_generators(gs)': it is illegal to
    // add a system of generators that does not contain points
    // to an empty zero-dimensional polyhedron.
    ph.add_generators(gs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error26() {
  set_handlers();

  Variable A(0);

  NNC_Polyhedron ph1(1);
  ph1.add_constraint(A > 5);

  C_Polyhedron ph2(1);
  ph2.add_constraint(A >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif
  
  try {
    // This is an invalid use of the `operator<=':
    // it is illegal to apply this function to a
    // closed polyhedron and a non-closed polyhedron.
    ph1 <= ph2;
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error27() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A <= 2);
  ph1.add_constraint(A - B >= 0);

  NNC_Polyhedron ph2(2);
  ph2.add_constraint(A >= 0);
  ph2.add_constraint(A <= 4);
  ph2.add_constraint(A - B >= 0);

  ConSys cs;
  cs.insert(A < 8);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
  print_constraints(cs, "*** cs ***");
#endif

  try {
    // This is an invalid use of the function
    // `limited_H79_widening_assign': it is illegal to
    // apply this function to a non-closed polyhedron,
    // a non-closed polyhedron and a system of
    // constraints that contains strict inequalities.
    ph2.limited_H79_widening_assign(ph1, cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error28() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(A <= 2);
  ph1.add_constraint(B >= 0);
  ph1.add_constraint(A - B >= 0);

  NNC_Polyhedron ph2(2);
  ph2.add_constraint(A < 5);
  ph2.add_constraint(B >= 0);
  ph2.add_constraint(A - B >= 0);

  try {
    // This is an invalid use of the function
    // `BBRZ02_widening_assign': it is illegal to
    // apply this function to a non-closed polyhedron and
    // a non-closed polyhedron.
    ph2.BBRZ02_widening_assign(ph1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error29() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A < 2);
  ph1.add_constraint(B > 0);
  ph1.add_constraint(A - B > 0);

  try {
    // This is an invalid use of the function
    // `C_Polyhedron(NNC_Polyhedron&)': it is illegal to
    // built a closed polyhedron starting from a 
    // non-closed polyhedron.
    C_Polyhedron ph2(ph1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedron: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

int
main() {
  
  error1();
  error2();
  error3();
  error4();
  error5();
  error6();
  error7();
  error8();
  error9();
  error11();
  error12();
  error13();
  error14();
  error15();
  error16();
  error17();
  error18();
  error19();
  error20();
  error21();
  error22();
  error23();
  error24();
  error25();
  error26();
  error27();
  error28();
  error29();

  return 0;
}
