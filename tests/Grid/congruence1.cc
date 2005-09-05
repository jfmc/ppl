/* Test Congruence.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Congruence>

class Test_Congruence : public Congruence {
public:
  Test_Congruence(Congruence cg) : Congruence(cg) {}
  Test_Congruence(Constraint c) : Congruence(c) {}
  void strong_normalize() { Congruence::strong_normalize(); }
};

// Negative inhomogeneous term.

static void
test1() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test1:" << endl;

  Test_Congruence a((x + 2*y + 3*z %= 5) / 7);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((x %= 5 - 3*z - 2*y) / 7);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Positive inhomogeneous term.

static void
test2() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test2:" << endl;

  Test_Congruence a((x + 2*y + 3*z %= -5) / 7);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((x %= -5 - 3*z - 2*y) / 7);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Common factors and reducible positive inhomogeneous term.

static void
test3() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test3:" << endl;

  Test_Congruence a((16*x + 2*y + 8*z + 64 %= 0) / 4);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((16*x + 2*y %= - 64 - 8*z) / 4);
  b.strong_normalize();
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Negative first coefficient.

static void
test4() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test4:" << endl;

  Test_Congruence a((- x + 2*y + 3*z %= 5) / 7);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((- x %= - 2*y + 5 - 3*z) / 7);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Constructed with only the %= operator.

static void
test5() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test5:" << endl;

  Test_Congruence c(x + 4*y + 3*z %= 5);
  Test_Congruence a(c);
  //Test_Congruence a = (x + 4*y + 3*z %= 5);
  //Test_Congruence a(x + 4*y + 3*z %= 5);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b(x + 4*y %= 5 - 3*z);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Equality congruence (a modulus of 0).

static void
test6() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test6:" << endl;

  Test_Congruence a((3*x + 24*y + 3*z %= -19) / 0);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((3*x + 24*y %= -19 - 3*z) / 0);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Constructed from a Constraint with the `/' operator.

static void
test7() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test7:" << endl;

  Test_Congruence a((x + 4*y + 3*z == 17) / 3);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((x + 4*y == 17 - 3*z) / 3);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Constructed from a Constraint.

static void
test8() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test8:" << endl;

  Test_Congruence a(x + 4*y + 3*z == 17);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b(x + 4*y == 17 - 3*z);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Set modulus with `/='.

static void
test9() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test9:" << endl;

  Test_Congruence a(x + 4*y + 3*z == 17);
  a /= 3;
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b(x + 4*y == 17 - 3*z);
  b /= 3;
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Use is_trivial_true.

static void
test10() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test10:" << endl;

  Test_Congruence a(0*x + 0*y + 0*z %= 17);
  if (find_variation(a))
    exit(1);
  if (!a.is_trivial_true()) {
    nout << "is_trivial_true should have returned true." << endl;
    exit(1);
  }

  a = Test_Congruence((0*x + 0*y + 0*z %= 0) / 3);
  if (find_variation(a))
    exit(1);
  if (!a.is_trivial_true()) {
    nout << "is_trivial_true should have returned true." << endl;
    exit(1);
  }
}

// Use is_trivial_false.

static void
test11() {
  Variable x(0);
  Variable y(1);

  nout << "test11:" << endl;

  Test_Congruence a(0*x + 0*y %= 17);
  a /= 0;
  if (find_variation(a))
    exit(1);
  if (!a.is_trivial_false()) {
    nout << "is_trivial_false should have returned true." << endl;
    exit(1);
  }

  a = Test_Congruence((0*x + 0*y + 3 %= 0) / 0);
  a.strong_normalize();
  if (find_variation(a))
    exit(1);
  if (!a.is_trivial_false()) {
    nout << "is_trivial_false should have returned true." << endl;
    exit(1);
  }
}

// Negative moduli.

static void
test12() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test12:" << endl;

  Test_Congruence a((x + 4*y + 3*z %= -4) / -3);
  a.strong_normalize();
  if (find_variation(a))
    exit(1);

  Test_Congruence b((x + 4*y %= -1 - 3*z) / -3);
  if (find_variation(b))
    exit(1);
  b.strong_normalize();

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Negative modulus and negative first coefficient.

static void
test13() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);
  Variable x3(3);
  Variable x4(4);

  nout << "test13:" << endl;

  Test_Congruence a((-x0 + 4*x1 + 3*x2 + 17*x3 + 2*x4 %= -4) / -3);
  if (find_variation(a))
    exit(1);
  a.strong_normalize();

  Test_Congruence b((-x0 + 4*x1 %= - 3*x2 - 17*x3 - 2*x4 - 4) / -3);
  b.strong_normalize();
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Create from empty linear expression.

static void
test14() {
  nout << "test14:" << endl;

  Linear_Expression le;
  Test_Congruence a(le %= le);
  a.strong_normalize();
  if (find_variation(a))
    exit(1);

  Test_Congruence b(le %= 0);
  b.strong_normalize();
  if (find_variation(b))
    exit(1);

  // FIX should these be equal?

  if (a == b)
    return;

  nout << "Test_Congruences a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
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
  test10();
  test11();
  test12();
  test13();
  test14();

  return 0;
}
CATCH
