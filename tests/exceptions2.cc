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

#include "ppl_install.hh"
#include "ehandlers.hh"
#include <stdexcept>
#include <iostream>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

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
  
  try {
    // This is an incorrect use of the function
    // add_dimensions_and_constraints(cs): it is illegal to apply
    // this function to a closed polyhedron with a system of constraints
    // that contains strict-inequalities.
    ph.add_dimensions_and_constraints(cs);
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
  error10();

  return 0;
}
