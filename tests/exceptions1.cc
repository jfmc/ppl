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
  Variable z(2);
  
  GenSys gs;
  try {
    // This is an incorrect use of the function Generator::point(expr, d):
    // it is illegal to built a point with the denominator
    // equal to zero.
    gs.insert(point(x + y + z, 0));
  }
  
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
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
  gs.insert(point(x + y));
  gs.insert(ray(x + 0*y));
  gs.insert(ray(0*x + y));
  C_Polyhedron ph(gs);
  LinExpression coeff1 = x + y + 1;
  try {
    // This is an incorrect use of function
    // C_Polyhedron::affine_image(v, expr,d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    Integer d = 0;
    ph.affine_image(x, coeff1, d);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_denominator: " << e.what() << endl << endl;
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

  C_Polyhedron ph1;
  GenSys gs;
  gs.insert(point(x + y));
  C_Polyhedron ph2(gs);
  try {
    // This is an incorrect use of function
    // C_Polyhedron::poly_hull_assign(p): it is illegal to use
    // it with two polyhedra of different dimensions.
    ph1.poly_hull_assign_and_minimize(ph2);
  }
  catch (std::invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch(...) {
    exit(1);
  }
}

void
error4() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  GenSys gs;
  gs.insert(line(x + y + z));

  try {
    // This is an incorrect use of the function
    // C_Polyhedron::C_Polyhedron(gs): it is illegal to built a
    // polyhedron starting from a system of generators that does not
    // contain a point.
    C_Polyhedron ph(gs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
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
  Variable z(2);
  
  GenSys gs;
  gs.insert(point(0*x + 1*y +2*z));
  C_Polyhedron ph(gs);

  set<Variable> to_be_removed;
  to_be_removed.insert(z);

  ph.remove_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // This is an incorrect use use of function
    // C_Polyhedron::remove_dimensions(to_be_remove).
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    ph.remove_dimensions(to_be_removed);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
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

  C_Polyhedron ph(1);
  ph.add_constraint(x >= 1);

  try {
    // This is an invalid used of the function
    // C_Polyhedron::affine_image(v, expr, d): it is illegal to
    // apply this function to a variable that is not in the space of
    // the polyhedron.
    ph.affine_image(y, x + 1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_variable: " << e.what() << endl << endl;
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
  Variable z(2);
  
  C_Polyhedron ph(2);
  ph.add_constraint(x >= 1);
  ph.add_constraint(y >= 1);
  
  try {
    // This is an invalid used of the function
    // C_Polyhedron::affine_image(v, expr, d): it is illegal to
    // use a variable in the expression that does not apper in the
    // space of the polyhedron.
    ph.affine_image(y, x + z + 1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_expression: " << e.what() << endl << endl;
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

  C_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  LinExpression coeff = x + y + 1;
  try {
    // This is an incorrect use of the function
    // C_Polyhedron::affine_preimage(v, expr, d): it is illegal
    // to apply to a polyhedron an expression with the denominator
    // equal to zero.
    Integer d = 0;
    ph.affine_preimage(x, coeff, d);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_denominator: " << e.what() << endl << endl;
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
  Variable z(2);

  GenSys gs;
  gs.insert(point());
  gs.insert(ray(x + y));
  gs.insert(ray(x));

  C_Polyhedron ph(gs);
  try {
    // This is an invalid used of the function
    // C_Polyhedron::affine_image(v, expr, d): it is illegal apply
    // the transformation to a variable that is not in the space
    // of the polyhedron.
    ph.affine_preimage(z, x + 1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_variable: " << e.what() << endl << endl;
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
  Variable z(2);

  GenSys gs;
  gs.insert(point());
  gs.insert(point(x));
  gs.insert(line(x + y));

  C_Polyhedron ph(gs);
  try {
    // This is an invalid used of the function
    // C_Polyhedron::affine_preimage(v, expr, d): it is illegal to
    // apply to a polyhedron an expression that contains a variable that
    // is not in the space of the polyhedron.
    ph.affine_preimage(y, x + z + 1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_expression: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error11() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(x >= y);

  C_Polyhedron ph2(3);

  try {
    // This is an invalid use of function
    // C_Polyhedron::intersection_assign_and_minimze(ph2): it is illegal
    // to apply this funcition to two polyhedra of different dimensions.
    ph1.intersection_assign_and_minimize(ph2);
  }
  catch (invalid_argument& e) {
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

  C_Polyhedron ph1(7);

  C_Polyhedron ph2(15);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::intersection_assign(ph2): it is illegal to apply
    // this function to two polyhedron of different dimensions.
    ph1.intersection_assign(ph2);
  }
  catch (invalid_argument& e) {
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

  Variable w(4);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators_and_minimize(gs): it is illegal
    // to add a system of generator that is not dimensional compatible
    // with the polyhedron.
    GenSys gs;
    gs.insert(point(w));
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
error14() {
  set_handlers();

  C_Polyhedron ph(5);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::remove_higher_dimensions(n): it is illegal to erase
    // a variable that is not in the space of the polyhedron.
    ph.remove_higher_dimensions(7);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_variable: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error15() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_constraints_and_minimze(cs): it is illegal to
    // add a system of constraints that is not dimensional incompatible
    // with the polyhedron.
    ConSys cs;
    cs.insert(x - y >= 0);
    ph.add_constraints_and_minimize(cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error16() {
  set_handlers();

  Variable y(1);

  C_Polyhedron ph(1);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_constraint(c): it is illegal to insert a
    // constraints that contains a variable that is not in the space
    // of the polyhedron.
    ph.add_constraint(y >= 0);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_constraint: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error17() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  
  C_Polyhedron ph(1);
  
  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_constraints(cs): it is illegal to add a system
    // of constraints that is dimensional incompatible with the
    // polyhedron.
    ConSys cs;
    cs.insert(x - y == 0);
    ph.add_constraints(cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error18() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  GenSys gs1;
  gs1.insert(point());
  gs1.insert(ray(x));

  C_Polyhedron ph1(gs1);
  
  GenSys gs2;
  gs2.insert(point(x));
  gs2.insert(ray(x + y));

  C_Polyhedron ph2(gs2);
  
  try {
    // This is an invalid use of the function
    // C_Polyhedron::poly_hull_assign(ph2): it is illegal to apply
    // this function to two polyhedra with different dimensions.
    ph1.poly_hull_assign(ph2);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error19() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function C_Polyhedron::add_generator(g):
    // it is illegal to insert a generator that is dimensional
    // incompatible with the polyhedron.
    ph.add_generator(point(x + y));
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_generator: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error20() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators(gs): it is illegal to a system of
    // generators that is dimensional incompatible with the
    // polyhedron.
    GenSys gs;
    gs.insert(point());
    gs.insert(line(x + y));
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
error21() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  GenSys gs;
  gs.insert(ray(x + y));
  gs.insert(point());

  C_Polyhedron ph(gs);
  try {
    // This is an invalid use of the function C_Polyhedron::relation_with(c):
    // it is illegal to use a constraints that is dimensional
    // incompatible with the polyhedron.
    Constraint c(z >= 0);
    ph.relation_with(c);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_constraint: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error22() {
  set_handlers();

  Variable z(2);

  C_Polyhedron ph(2);

  try {
    // This is an invalid use of the function
    // C_Polyhedronn::relation_with(g): it is illegal to apply this
    // function to a generator that is not dimensional compatible with
    // the polyhedron.
    Generator g(point(z));
    ph.relation_with(g);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_generator: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error23() {
  set_handlers();

  C_Polyhedron ph1(5);
  C_Polyhedron ph2(10);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::H79_widening_assign(ph2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    ph2.H79_widening_assign(ph1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl; 
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error24() {
  set_handlers();

  Variable y(1);

  C_Polyhedron ph1(1);
  C_Polyhedron ph2(2);

  ConSys cs;
  cs.insert(y <= 9);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::limited_H79_widening_assign(ph2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimensional compatible.
    ph2.limited_H79_widening_assign(ph1, cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error25() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  C_Polyhedron ph1(2);
  ph1.add_constraint(x - y >= 0);
  ph1.add_constraint(x >= 0);
  ph1.add_constraint(x <= 2);

  C_Polyhedron ph2(2);
  ph2.add_constraint(x - y >= 0);
  ph2.add_constraint(x >= 0);
  ph2.add_constraint(x <= 5);

  ConSys cs;
  cs.insert(z <= 5);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::limited_H79_widening_assign(ph, cs): it is
    // illegal to apply this function to a system of constraints that
    // is not dimensional compatible with the two polyhedra.
    ph2.limited_H79_widening_assign(ph1, cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error26() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1(3);
  ph1.add_constraint(x - y >= 0);

  C_Polyhedron ph2(2);
  ph2.add_constraint(x - y == 0);

  try {
    // This is an invalid use of operator <=: it is illegal to apply
    // this function to two polyhedra that are not dimensional compatible.
    ph1 <= ph2;
  }
  catch (invalid_argument& e) {
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
  Variable x(0);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of function C_Polyhedron::add_generator(g):
    // it is illegal to insert a generator that is not dimensional
    // comaptible with the polyhedron..
    Generator g(ray(x));
    ph.add_generator(g);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_generator: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error28() {
  set_handlers();
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators(gs): it is illegal to add a system
    // of generators with no points to an empty polyhedron.
    GenSys gs;
    gs.insert(ray(x + y));
    gs.insert(ray(x - y));
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
error29() {
  set_handlers();
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators_and_minimize(gs): it is illegal
    // to apply this function with a system of generators with no
    // points to an empty polyhedron.
    GenSys gs;
    gs.insert(line(x));
    gs.insert(line(y));
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
error30() {
  
  C_Polyhedron ph1(3);
  C_Polyhedron ph2(5);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::poly_difference_assign(ph2): it is impossibile to apply
    // this function to two polyhedra of different dimensions.
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
error31() {
  set_handlers();

  C_Polyhedron ph1(3);
  C_Polyhedron ph2(8);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::time_elapse_assign(p): it is illegal to use
    // it with two polyhedra of different dimensions.
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
error32() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  GenSys gs1;
  gs1.insert(ray(A));
  gs1.insert(line(B));
  
  const GenSys gs2 = gs1;

#if NOISY
  print_generators(gs2, "*** gs2 ***");
#endif
  
  try {
    // This is an incorrect use of the function
    // `C_Polyhedron::C_Polyhedron(gs)': it is illegal to built a
    // closed polyhedron starting from a constant system of 
    // generators that does not contain points.
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
error33() {
  set_handlers();

  Variable A(0);

  C_Polyhedron ph1(2, C_Polyhedron::EMPTY);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif
  
  try {
    // This is an incorrect use of the function
    // `add_generator(g)': it is illegal to add a
    // ray to an empty polyhedron.
    ph1.add_generator(ray(A));
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
error34() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(1);
  ph.add_constraint(A >= 3);
  ph.add_constraint(A <= 5);

  try {
    // This is an invalid used of the function
    // `C_Polyhedron::bounds_from_above(v, expr, d)': it is illegal to
    // use a variable in the expression that does not apper in the
    // space of the polyhedron.
    ph.bounds_from_above(A + B);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_expression: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error35() {
  set_handlers();
  
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  GenSys gs;
  gs.insert(line(A));
  gs.insert(ray(B));

  try {
    // This is an invalid used of the function
    // `add_generators_and_minimize(gs)': it is illegal to
    // add a system of generators that does not contain points
    // to an empty polyhedron.
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
error36() {
  set_handlers();
  
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  GenSys gs;
  gs.insert(ray(A));
  gs.insert(ray(B));

  try {
    // This is an invalid used of the function
    // `add_generators(gs)': it is illegal to
    // add a system of generators that does not contain points
    // to an empty polyhedron.
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
error37() {
  set_handlers();

  C_Polyhedron ph1(5);
  C_Polyhedron ph2(10);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::BBRZ02_widening_assign(ph2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    ph2.BBRZ02_widening_assign(ph1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_polyhedra: " << e.what() << endl << endl; 
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error38() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A - B >= 0);
  
  try {
    // This is an incorrect use of function
    // C_Polyhedron::generalized_affine_image(v, r, expr, d): it is illegal
    // applying the function with a linear expression with the denominator
    // equal to zero.
    Integer d = 0;
    ph.generalized_affine_image(B, PPL_GE, B + 2, d);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_denominator: " << e.what() << endl << endl;
#endif
  }
 catch (...) {
    exit(1);
  }
}

void
error39() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(1);
  ph.add_constraint(A >= 0);
  
  try {
    // This is an incorrect use of function
    // C_Polyhedron::generalized_affine_image(v, r, expr, d): it is illegal to
    // use a variable in the expression that does not apper in the polyhedron.
    ph.generalized_affine_image(A, PPL_GE, B);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_expression: " << e.what() << endl << endl;
#endif
  }
 catch (...) {
    exit(1);
  }
}

void
error40() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(1);
  ph.add_constraint(A >= 1);

  try {
    // This is an invalid used of the function
    // C_Polyhedron::generalized_affine_image(v, expr, d): it is illegal to
    // apply this function to a variable that is not in the space of
    // the polyhedron.
    ph.generalized_affine_image(B, PPL_LE, A + 1);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_variable: " << e.what() << endl << endl;
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
  error30();
  error31();
  error32();
  error33();
  error34();
  error35();
  error36();
  error37();
  error38();
  error39();
  error40();

  return 0;
}


