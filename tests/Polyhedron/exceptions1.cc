/* Test that the right exceptions are thrown in case of incorrect uses.
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

namespace {

void
error1() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Generator_System gs;
  try {
    // This is an incorrect use of the function Generator::point(expr, d):
    // it is illegal to build a point with the denominator
    // equal to zero.
    gs.insert(point(x + y + z, 0));

    // It is an error if the exception is not thrown.
    exit(1);
  }

  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    // It is an error if the wrong exception is thrown.
    exit(1);
  }
}

void
error2() {
  Variable x(0);
  Variable y(1);

  Generator_System gs;
  gs.insert(point(x + y));
  gs.insert(ray(x + 0*y));
  gs.insert(ray(0*x + y));
  C_Polyhedron ph(gs);
  Linear_Expression coeff1 = x + y + 1;
  try {
    // This is an incorrect use of function
    // C_Polyhedron::affine_image(v, expr,d): it is illegal applying
    // the function with a linear expression with the denominator equal to
    // zero.
    Coefficient d = 0;
    ph.affine_image(x, coeff1, d);
    exit(1);
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
error3() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1;
  Generator_System gs;
  gs.insert(point(x + y));
  C_Polyhedron ph2(gs);
  try {
    // This is an incorrect use of function
    // C_Polyhedron::poly_hull_assign(p): it is illegal to use
    // it with two polyhedra of different dimensions.
    ph1.poly_hull_assign_and_minimize(ph2);
    exit(1);
  }
  catch (invalid_argument& e) {
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
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Generator_System gs;
  gs.insert(line(x + y + z));

  try {
    // This is an incorrect use of the function
    // C_Polyhedron::C_Polyhedron(gs): it is illegal to build a
    // polyhedron starting from a system of generators that does not
    // contain a point.
    C_Polyhedron ph(gs);
    exit(1);
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
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Generator_System gs;
  gs.insert(point(0*x + 1*y +2*z));
  C_Polyhedron ph(gs);

  Variables_Set to_be_removed;
  to_be_removed.insert(z);

  ph.remove_space_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // This is an incorrect use use of function
    // C_Polyhedron::remove_space_dimensions(to_be_remove).
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    ph.remove_space_dimensions(to_be_removed);
    exit(1);
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
    exit(1);
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
error7() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  C_Polyhedron ph(2);
  ph.add_constraint(x >= 1);
  ph.add_constraint(y >= 1);

  try {
    // This is an invalid used of the function
    // C_Polyhedron::affine_image(v, expr, d): it is illegal to
    // use a variable in the expression that does not appear in the
    // space of the polyhedron.
    ph.affine_image(y, x + z + 1);
    exit(1);
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
error8() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  Linear_Expression coeff = x + y + 1;
  try {
    // This is an incorrect use of the function
    // C_Polyhedron::affine_preimage(v, expr, d): it is illegal
    // to apply to a polyhedron an expression with the denominator
    // equal to zero.
    Coefficient d = 0;
    ph.affine_preimage(x, coeff, d);
    exit(1);
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
error9() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Generator_System gs;
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
    exit(1);
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
error10() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Generator_System gs;
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
    exit(1);
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
error11() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(x >= y);

  C_Polyhedron ph2(3);

  try {
    // This is an invalid use of function
    // C_Polyhedron::intersection_assign_and_minimize(ph2): it is illegal
    // to apply this function to two polyhedra of different dimensions.
    ph1.intersection_assign_and_minimize(ph2);
    exit(1);
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
error12() {
  C_Polyhedron ph1(7);

  C_Polyhedron ph2(15);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::intersection_assign(ph2): it is illegal to apply
    // this function to two polyhedron of different dimensions.
    ph1.intersection_assign(ph2);
    exit(1);
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
error13() {
  Variable w(4);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators_and_minimize(gs): it is illegal
    // to add a system of generator that is not dimension-compatible
    // with the polyhedron.
    Generator_System gs;
    gs.insert(point(w));
    ph.add_generators_and_minimize(gs);
    exit(1);
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
error14() {
  C_Polyhedron ph(5);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::remove_higher_space_dimensions(n): it is illegal to
    // erase a variable that is not in the space of the polyhedron.
    ph.remove_higher_space_dimensions(7);
    exit(1);
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
error15() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_constraints_and_minimize(cs): it is illegal to
    // add a system of constraints that is not dimensional incompatible
    // with the polyhedron.
    Constraint_System cs;
    cs.insert(x - y >= 0);
    ph.add_constraints_and_minimize(cs);
    exit(1);
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
error16() {
  Variable y(1);

  C_Polyhedron ph(1);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_constraint(c): it is illegal to insert a
    // constraints that contains a variable that is not in the space
    // of the polyhedron.
    ph.add_constraint(y >= 0);
    exit(1);
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
error17() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_constraints(cs): it is illegal to add a system
    // of constraints that is dimensional incompatible with the
    // polyhedron.
    Constraint_System cs;
    cs.insert(x - y == 0);
    ph.add_constraints(cs);
    exit(1);
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
error18() {
  Variable x(0);
  Variable y(1);

  Generator_System gs1;
  gs1.insert(point());
  gs1.insert(ray(x));

  C_Polyhedron ph1(gs1);

  Generator_System gs2;
  gs2.insert(point(x));
  gs2.insert(ray(x + y));

  C_Polyhedron ph2(gs2);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::poly_hull_assign(ph2): it is illegal to apply
    // this function to two polyhedra with different dimensions.
    ph1.poly_hull_assign(ph2);
    exit(1);
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
error19() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function C_Polyhedron::add_generator(g):
    // it is illegal to insert a generator that is dimensional
    // incompatible with the polyhedron.
    ph.add_generator(point(x + y));
    exit(1);
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
error20() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(1, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators(gs): it is illegal to a system of
    // generators that is dimensional incompatible with the
    // polyhedron.
    Generator_System gs;
    gs.insert(point());
    gs.insert(line(x + y));
    ph.add_generators(gs);
    exit(1);
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
error21() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Generator_System gs;
  gs.insert(ray(x + y));
  gs.insert(point());

  C_Polyhedron ph(gs);
  try {
    // This is an invalid use of the function C_Polyhedron::relation_with(c):
    // it is illegal to use a constraints that is dimensional
    // incompatible with the polyhedron.
    Constraint c(z >= 0);
    ph.relation_with(c);
    exit(1);
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
error22() {
  Variable z(2);

  C_Polyhedron ph(2);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::relation_with(g): it is illegal to apply this
    // function to a generator that is not dimension-compatible with
    // the polyhedron.
    Generator g(point(z));
    ph.relation_with(g);
    exit(1);
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
error23() {
  C_Polyhedron ph1(5);
  C_Polyhedron ph2(10);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::H79_widening_assign(ph2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    ph2.H79_widening_assign(ph1);
    exit(1);
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
error24() {
  Variable y(1);

  C_Polyhedron ph1(1);
  C_Polyhedron ph2(2);

  Constraint_System cs;
  cs.insert(y <= 9);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::limited_H79_extrapolation_assign(ph2, cs): it is
    // illegal to apply this function to two polyhedra that are not
    // dimension-compatible.
    ph2.limited_H79_extrapolation_assign(ph1, cs);
    exit(1);
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
error25() {
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

  Constraint_System cs;
  cs.insert(z <= 5);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::limited_H79_extrapolation_assign(ph, cs): it is
    // illegal to apply this function to a system of constraints that
    // is not dimension-compatible with the two polyhedra.
    ph2.limited_H79_extrapolation_assign(ph1, cs);
    exit(1);
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
error26() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1(3);
  ph1.add_constraint(x - y >= 0);

  C_Polyhedron ph2(2);
  ph2.add_constraint(x - y == 0);

  try {
    // This is an invalid use of Polyhedron::contains(): it is
    // illegal to apply this method to two polyhedra that are not
    // dimension-compatible.
    ph1.contains(ph2);
    exit(1);
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
error27() {
  Variable x(0);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of method
    // C_Polyhedron::add_generator(g): it is illegal to insert a
    // generator that is not dimension-compatible with the
    // polyhedron.
    Generator g(ray(x));
    ph.add_generator(g);
    exit(1);
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
error28() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(3, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators(gs): it is illegal to add a system
    // of generators with no points to an empty polyhedron.
    Generator_System gs;
    gs.insert(ray(x + y));
    gs.insert(ray(x - y));
    ph.add_generators(gs);
    exit(1);
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
error29() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::add_generators_and_minimize(gs): it is illegal
    // to apply this function with a system of generators with no
    // points to an empty polyhedron.
    Generator_System gs;
    gs.insert(line(x));
    gs.insert(line(y));
    ph.add_generators_and_minimize(gs);
    exit(1);
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
error30() {

  C_Polyhedron ph1(3);
  C_Polyhedron ph2(5);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::poly_difference_assign(ph2): it is impossible to apply
    // this function to two polyhedra of different dimensions.
    ph1.poly_difference_assign(ph2);
    exit(1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error31() {
  C_Polyhedron ph1(3);
  C_Polyhedron ph2(8);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::time_elapse_assign(p): it is illegal to use
    // it with two polyhedra of different dimensions.
    ph1.time_elapse_assign(ph2);
    exit(1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error32() {
  Variable A(0);
  Variable B(1);

  Generator_System gs1;
  gs1.insert(ray(A));
  gs1.insert(line(B));

  const Generator_System gs2 = gs1;

#if NOISY
  print_generators(gs2, "*** gs2 ***");
#endif

  try {
    // This is an incorrect use of the function
    // `C_Polyhedron::C_Polyhedron(gs)': it is illegal to build a
    // closed polyhedron starting from a constant system of
    // generators that does not contain points.
    C_Polyhedron ph2(gs2);
    exit(1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error33() {
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
    exit(1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error34() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(1);
  ph.add_constraint(A >= 3);
  ph.add_constraint(A <= 5);

  try {
    // This is an invalid used of the function
    // `C_Polyhedron::bounds_from_above(v, expr, d)': it is illegal to
    // use a variable in the expression that does not appear in the
    // space of the polyhedron.
    ph.bounds_from_above(A + B);
    exit(1);
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
error35() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  Generator_System gs;
  gs.insert(line(A));
  gs.insert(ray(B));

  try {
    // This is an invalid used of the function
    // `add_generators_and_minimize(gs)': it is illegal to
    // add a system of generators that does not contain points
    // to an empty polyhedron.
    ph.add_generators_and_minimize(gs);
    exit(1);
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
error36() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  Generator_System gs;
  gs.insert(ray(A));
  gs.insert(ray(B));

  try {
    // This is an invalid used of the function
    // `add_generators(gs)': it is illegal to
    // add a system of generators that does not contain points
    // to an empty polyhedron.
    ph.add_generators(gs);
    exit(1);
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
error37() {
  C_Polyhedron ph1(5);
  C_Polyhedron ph2(10);

  try {
    // This is an invalid use of the function
    // C_Polyhedron::BHRZ03_widening_assign(ph2): it is illegal to apply
    // this function to two polyhedra that are not dimensional
    // compatible.
    ph2.BHRZ03_widening_assign(ph1);
    exit(1);
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
error38() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A - B >= 0);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::generalized_affine_image(v, r, expr, d): it is illegal
    // applying the function with a linear expression with the denominator
    // equal to zero.
    Coefficient d = 0;
    ph.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, B + 2, d);
    exit(1);
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
error39() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(1);
  ph.add_constraint(A >= 0);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::generalized_affine_image(v, r, expr, d): it is illegal to
    // use a variable in the expression that does not appear in the polyhedron.
    ph.generalized_affine_image(A, GREATER_THAN_OR_EQUAL, B);
    exit(1);
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
error40() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(1);
  ph.add_constraint(A >= 1);

  try {
    // This is an invalid used of the function
    // C_Polyhedron::generalized_affine_image(v, r, expr, d): it is illegal to
    // apply this function to a variable that is not in the space of
    // the polyhedron.
    ph.generalized_affine_image(B, LESS_THAN_OR_EQUAL, A + 1);
    exit(1);
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
error41() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 0);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::generalized_affine_image(lhs, r, rhs):
    // it is illegal to use a variable in the `rhs' expression that
    // does not appear in the polyhedron.
    ph.generalized_affine_image(A + B, GREATER_THAN_OR_EQUAL, B + C);
    exit(1);
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
error42() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 1);

  try {
    // This is an incorrect use of function
    // C_Polyhedron::generalized_affine_image(lhs, r, rhs):
    // it is illegal to use a variable in the `lhs' expression that
    // does not appear in the polyhedron.
    ph.generalized_affine_image(B + C, LESS_THAN_OR_EQUAL, A + 1);
    exit(1);
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
error43() {
  Generator_System gs;
  Linear_Expression e;
  try {
    // This is an incorrect use of function
    // Generator::ray(e):
    // the origin can not be a ray.
    gs.insert(ray(e));
    exit(1);
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
error44() {
  Generator_System gs;
  Linear_Expression e;
  try {
    // This is an incorrect use of function
    // Generator::line(e):
    // the origin can not be a line.
    gs.insert(line(e));
    exit(1);
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
error45() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator g = point(3*A - 2*B);
  try {
    // This is an incorrect use of function
    // Generator::coefficient(v):
    // it is impossible to compute the coefficient
    // of a variable that is not in the space of the
    // generator.
    g.coefficient(C);
    exit(1);
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
error46() {
  Variable A(0);
  Variable B(1);

  Generator g = line(3*A - 2*B);
  try {
    // This is an incorrect use of method Generator::divisor(): it is
    // illegal to ask for the divisor of a line.
    g.divisor();
    exit(1);
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
error47() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint c(2*A - 3*B <= 2);
  try {
    // This is an incorrect use of function
    // Constraint::coefficient(v):
    // it is impossible to compute the coefficient
    // of a variable that is not in the space of the
    // constraint.
    c.coefficient(C);
    exit(1);
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

} // namespace

int
main() TRY {
  set_handlers();

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
  error41();
  error42();
  error43();
  error44();
  error45();
  error46();
  error47();

  return 0;
}
CATCH
