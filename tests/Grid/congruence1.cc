/* Test class Congruence.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

class Test_Congruence : public Congruence {
public:
  Test_Congruence(Congruence cg) : Congruence(cg) {}
  Test_Congruence(Constraint c) : Congruence(c) {}
  void strong_normalize() { Congruence::strong_normalize(); }
  void normalize() { Congruence::normalize(); }
};

// Negative inhomogeneous term.

static bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((A + 2*B + 3*C %= 5) / 7);
  a.strong_normalize();

  Test_Congruence b((A %= 5 - 3*C - 2*B) / 7);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Positive inhomogeneous term.

static bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((A + 2*B + 3*C %= -5) / 7);
  a.strong_normalize();

  Test_Congruence b((A %= -5 - 3*C - 2*B) / 7);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Common factors and reducible positive inhomogeneous term.

static bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((16*A + 2*B + 8*C + 64 %= 0) / 4);
  a.strong_normalize();

  Test_Congruence b((16*A + 2*B %= - 64 - 8*C) / 4);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Negative first coefficient.

static bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((- A + 2*B + 3*C %= 5) / 7);
  a.strong_normalize();

  Test_Congruence b((- A %= - 2*B + 5 - 3*C) / 7);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Constructed with only the %= operator.

static bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence c(A + 4*B + 3*C %= 5);
  Test_Congruence a(c);
  //Test_Congruence a = (A + 4*B + 3*C %= 5);
  //Test_Congruence a(A + 4*B + 3*C %= 5);
  a.strong_normalize();

  Test_Congruence b(A + 4*B %= 5 - 3*C);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Equality congruence (a modulus of 0).

static bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((3*A + 24*B + 3*C %= -19) / 0);
  a.strong_normalize();

  Test_Congruence b((3*A + 24*B %= -19 - 3*C) / 0);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Constructed from a Constraint with the `/' operator.

static bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((A + 4*B + 3*C == 17) / 3);
  a.strong_normalize();

  Test_Congruence b((A + 4*B == 17 - 3*C) / 3);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Constructed from a Constraint.

static bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a(A + 4*B + 3*C == 17);
  a.strong_normalize();

  Test_Congruence b(A + 4*B == 17 - 3*C);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Set modulus with `/='.

static bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a(A + 4*B + 3*C == 17);
  a /= 3;
  a.strong_normalize();

  Test_Congruence b(A + 4*B == 17 - 3*C);
  b /= 3;
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// is_trivial_true and is_trivial_false.

static bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a(0*A + 0*B + 0*C %= 17);

  bool ok = (a.is_trivial_true()) && (!a.is_trivial_false());

  print_congruence(a, "*** a(0*A + 0*B + 0*C %= 17) ***");

  a = Test_Congruence((0*A + 0*B + 0*C %= 0) / 3);
  ok &= a.is_trivial_true()
    && !a.is_trivial_false();

  a = Test_Congruence((0*A + 0*B + 8 %= 0) / 4);
  ok &= a.is_trivial_true()
    && !a.is_trivial_false();

  print_congruence(a, "*** a = Test_Congruence((0*A + 0*B + 8 %= 0) / 4) ***");

  a = Test_Congruence(0*A + 0*B %= 17);
  a /= 0;
  ok &= !a.is_trivial_true()
    && a.is_trivial_false();

  print_congruence(a, "*** a = Test_Congruence(0*A + 0*B %= 17) ***");

  a = Test_Congruence((0*A + 0*B + 3 %= 0) / 0);
  a.strong_normalize();
  ok &= !a.is_trivial_true()
    && a.is_trivial_false();

  print_congruence(a, "*** a = Test_Congruence((0*A + 0*B + 3 %= 0) / 0) ***");

  a = Test_Congruence((0*A + 0*B + 4 %= 0) / 3);
  a.strong_normalize();
  ok &= !a.is_trivial_true()
    && a.is_trivial_false();

  print_congruence(a, "*** a = Test_Congruence((0*A + 0*B + 4 %= 0) / 3) ***");

  return ok;
}

// Negative moduli.

static bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence a((A + 4*B + 3*C %= -4) / -3);
  a.strong_normalize();

  Test_Congruence b((A + 4*B %= -1 - 3*C) / -3);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Negative modulus and negative first coefficient.

static bool
test12() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);
  Variable x3(3);
  Variable x4(4);

  Test_Congruence a((-x0 + 4*x1 + 3*x2 + 17*x3 + 2*x4 %= -4) / -3);
  a.strong_normalize();

  Test_Congruence b((-x0 + 4*x1 %= - 3*x2 - 17*x3 - 2*x4 - 4) / -3);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Create from empty linear expression.

static bool
test13() {
  Linear_Expression le;
  Test_Congruence a(le %= le);
  a.strong_normalize();

  Test_Congruence b(le %= 0);
  b.strong_normalize();

  bool ok (a == b);

  print_congruence(a, "*** a ***");
  print_congruence(b, "*** b ***");

  return ok;
}

// Space dimension exception.

static bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(2);

  try {
    gr.add_congruence(A + C %= 0);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
  NEW_TEST(test09);
  NEW_TEST(test10);
  NEW_TEST(test11);
  NEW_TEST(test12);
  NEW_TEST(test13);
  NEW_TEST(test14);
END_MAIN
