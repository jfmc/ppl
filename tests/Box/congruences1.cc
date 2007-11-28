/* Test Box::Box(const Generator_System&).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

#include "files.hh"
#include <string>
#include <fstream>

using std::string;
using std::fstream;
using std::ios_base;

namespace {

// Universe Box constructed from empty congruences
bool
test01() {
  Congruence_System cgs;
  TBox box(cgs);

  Rational_Box known_result(0);

  bool ok = (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// Box constructed from non-empty congruences and add_congruences()
bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Congruence_System cgs;
  cgs.insert(A + B %= 0);
  cgs.insert((1*A + 2*B + 3*C + 4*D %= 0) / 0);
  cgs.insert((2*A + 3*B + 4*C + 5*D %= 1) / 0);
  TBox box(cgs);

  Rational_Box known_result(4);
  known_result.add_congruences(cgs);

  bool ok = (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// Box constructed from non-empty congruences
bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert(B %= 3);
  cgs.insert(B %= 0);
  cgs.insert(C %= 7);

  TBox box(cgs);

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);

  bool ok = (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_congruences_and_minimize()
bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert(B %= 3);
  cgs.insert(B %= 0);
  cgs.insert(C %= 7);

  TBox box(3);
  bool ok = box.add_congruences_and_minimize(cgs);

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);

  ok = ok && (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_recycled_congruences()
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Congruence_System cgs;
  cgs.insert(A + B %= 0);
  cgs.insert((1*A + 2*B + 3*C + 4*D %= 0) / 0);
  cgs.insert((2*A + 3*B + 4*C + 5*D %= 1) / 0);
  TBox box(4);
  box.add_recycled_congruences(cgs);

  Rational_Box known_result(4);
  known_result.add_constraint(1*A + 2*B + 3*C + 4*D == 0);
  known_result.add_constraint(2*A + 3*B + 4*C + 5*D == 1);

  bool ok = (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_recycled_congruences_and_minimize()
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert(B %= 3);
  cgs.insert(B %= 0);
  cgs.insert(C %= 7);

  TBox box(3);
  bool ok = box.add_recycled_congruences_and_minimize(cgs);

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);

  ok = ok && (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// Box constructed from non-empty congruences; congruences().
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert(B %= 3);
  cgs.insert(B %= 0);
  cgs.insert(C %= 7);

  TBox box(cgs);

  TBox box1(box.congruences());

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(box, "*** box ***");
  print_constraints(box1, "*** box1(box.congruences()) ***");

  return ok;
}

// Box constructed from non-empty congruences.
bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert((B %= 3) / 0);
  // This inconsistent equality is ignored when congruences
  // are added to the box.
  cgs.insert((A + B %= 0) / 0);
  cgs.insert(C %= 7);

  TBox box(cgs);

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);
  known_result.add_constraint(B == 3);

  bool ok = (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_congruences_and_minimize for inconsistent equality congruences
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert((B %= 3) / 0);
  cgs.insert((A %= 0) / 0);
  cgs.insert(C %= 7);

  TBox box(3);
  bool ok = !box.add_congruences_and_minimize(cgs);

  Rational_Box known_result(3, EMPTY);

  ok = ok && (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_congruence()
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBox box(4);
  box.add_congruence((1*A + 2*B + 3*C + 4*D %= 0) / 0);
  box.add_congruence((2*A + 3*B + 4*C + 5*D %= 1) / 0);

  Rational_Box known_result(4);

  bool ok = (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_congruence_and_minimize()
bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBox box(3);
  bool ok = box.add_congruence_and_minimize((A %= 7) / 0);
  ok = ok && box.add_congruence_and_minimize((A %= 2) / 3);
  // Inconsistency in the two non-relational additions should
  // not be detected.
  ok = ok && box.add_congruence_and_minimize((A + B %= 2) / 0);
  ok = ok && box.add_congruence_and_minimize((A + B %= 3) / 0);

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);

  ok = ok && (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");

  return ok;
}

// add_congruence_and_minimize()
bool
test12() {
  Variable A(0);

  TBox box(1);
  bool ok = box.add_congruence_and_minimize((A %= 7) / 0);
  ok = ok && !box.add_congruence_and_minimize((A %= 2) / 0);

  Rational_Box known_result(1, EMPTY);

  ok = ok && (Rational_Box(box) == known_result);

  print_constraints(box, "*** box ***");
  print_constraints(known_result, "*** known_result ***");

  return ok;
}


// Non-empty box; congruences().
bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert(B %= 3);
  cgs.insert(B %= 0);
  cgs.insert(C %= 7);

  TBox box(cgs);

  TBox box1(box.congruences());

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(box1, "*** box1(box.congruences()) ***");

  return ok;
}

// Non-empty Box; minimized_congruences().
bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert((B %= 3) / 0);
  cgs.insert((C %= 3) / 5);

  TBox box(cgs);
  TBox box1(box.minimized_congruences());

  Rational_Box known_result(3);
  known_result.add_constraint(A == 7);
  known_result.add_constraint(B == 3);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(box1, "*** box1(box.congruences()) ***");

  return ok;
}

// Empty Box; minimized_congruences().
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 0);
  cgs.insert((B %= 3) / 0);
  cgs.insert((A %= 0) / 0);
  cgs.insert(C %= 7);

  TBox box(3);
  box.add_congruences(cgs);

  TBox box1(box.minimized_congruences());

  Rational_Box known_result(3, EMPTY);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(box1, "*** box1(box.congruences()) ***");

  return ok;
}

// Zero dimension universe; congruences()
bool
test16() {
  TBox box(0);
  TBox box1(box.congruences());

  Rational_Box known_result(0);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(box1, "*** box1(box.congruences()) ***");

  return ok;
}

// Zero dimension empty; congruences()
bool
test17() {
  TBox box(0, EMPTY);
  TBox box1(box.congruences());

  Rational_Box known_result(0, EMPTY);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(box1, "*** box1(box.congruences()) ***");

  return ok;
}

bool
test18() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBox box1(2);

  try {
    // This is an invalid use of method
    // Box::add_congruence: it is illegal
    // to add a congruence with bigger dimension.
    box1.add_congruence(x %= 0);
    box1.add_congruence(y - x + z %= 0);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test19() {
  Variable x(0);
  Variable y(1);

  TBox box(1);

  try {
    // This is an invalid use of the method
    // Box::add_congruences(cs): it is illegal to
    // add a system of congruences that is dimensional incompatible
    // with the box.
    Congruence_System cgs;
    cgs.insert(x - y %= 0);
    box.add_congruences(cgs);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test20() {
  Variable y(1);

  TBox box(1);

  try {
    // This is an invalid use of the method
    // Box::add_congruence(c): it is illegal to insert a
    // congruence that contains a variable that is not in the space
    // of the box.
    box.add_congruence((y %= 0) / 0);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST(test18);
  DO_TEST(test19);
  DO_TEST(test20);
END_MAIN
