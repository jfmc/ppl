/* Tests on individual congruences.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

static void
check_ok(Congruence a) {
  if (!a.OK()) {
    nout << "a.OK() failed\nASCII dump: " << endl;
    a.ascii_dump(nout);
    exit(1);
  }
}

/* Negative inhomogeneous term.  */

static void
test1() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test1" << endl;

  stringstream dump;

  Congruence a((x + 2*y + 3*z %= 5) / 7);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "2 1 2 3 m 7\n");

  Congruence b((x %= 5 - 3*z - 2*y) / 7);
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "2 1 2 3 m 7\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Positive inhomogeneous term.  */

static void
test2() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test2" << endl;

  stringstream dump;

  Congruence a((x + 2*y + 3*z %= -5) / 7);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "5 1 2 3 m 7\n");

  Congruence b((x %= -5 - 3*z - 2*y) / 7);
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "5 1 2 3 m 7\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Common factors and reducible positive inhomogeneous term.  */

static void
test3() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test3" << endl;

  stringstream dump;

  Congruence a((16*x + 2*y + 8*z + 64 %= 0) / 4);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "0 8 1 4 m 2\n");

  Congruence b((16*x + 2*y %= - 64 - 8*z) / 4);
  nout << b << endl;
  b.strong_normalize();
  check_ok(b);
  dump.str("");
  b.ascii_dump(dump);
  check_dump(dump, "0 8 1 4 m 2\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Negative first coefficient.  */

static void
test4() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test4" << endl;

  stringstream dump;

  Congruence a((- x + 2*y + 3*z %= 5) / 7);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "5 1 -2 -3 m 7\n");

  Congruence b((- x %= - 2*y + 5 - 3*z) / 7);
  nout << b << endl;
  check_ok(b);
  b.strong_normalize();
  dump.str("");
  b.ascii_dump(dump);
  check_dump(dump, "5 1 -2 -3 m 7\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Constructed with only the %= operator.  */

static void
test5() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test5" << endl;

  stringstream dump;

  Congruence c(x + 4*y + 3*z %= 5);
  Congruence a(c);
  //Congruence a = (x + 4*y + 3*z %= 5);
  //Congruence a(x + 4*y + 3*z %= 5);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "0 1 4 3 m 1\n");

  Congruence b(x + 4*y %= 5 - 3*z);
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "0 1 4 3 m 1\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Equality congruence (a modulus of 0).  */

static void
test6() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test6" << endl;

  stringstream dump;

  Congruence a((3*x + 24*y + 3*z %= -19) / 0);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "19 3 24 3 m 0\n");

  Congruence b((3*x + 24*y %= -19 - 3*z) / 0);
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "19 3 24 3 m 0\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Constructed from a Constraint.  */

static void
test7() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test7" << endl;

  stringstream dump;

  Congruence a((x + 4*y + 3*z == 17) / 3);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "1 1 4 3 m 3\n");

  Congruence b((x + 4*y == 17 - 3*z) / 3);
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "1 1 4 3 m 3\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Constructed from a Constraint, using only ==.  */

static void
test8() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test8" << endl;

  stringstream dump;

  Congruence a(x + 4*y + 3*z == 17);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "0 1 4 3 m 1\n");

  Congruence b(x + 4*y == 17 - 3*z);
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "0 1 4 3 m 1\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Set modulus with `/='.  */

static void
test9() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test9" << endl;

  stringstream dump;

  Congruence a(x + 4*y + 3*z == 17);
  a /= 3;
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "1 1 4 3 m 3\n");

  Congruence b(x + 4*y == 17 - 3*z);
  b /= 3;
  nout << b << endl;
  check_ok(b);
  dump.str("");
  b.strong_normalize();
  b.ascii_dump(dump);
  check_dump(dump, "1 1 4 3 m 3\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Use is_trivial_true.  */

static void
test10() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test10" << endl;

  stringstream dump;

  Congruence a(0*x + 0*y + 0*z %= 17);
  nout << a << endl;
  check_ok(a);
  a.ascii_dump(dump);
  check_dump(dump, "-17 0 0 0 m 1\n");
  if (!a.is_trivial_true()) {
    nout << "is_trivial_true should have returned true." << endl;
    exit(1);
  }

  a = Congruence((0*x + 0*y + 0*z %= 0) / 3);
  nout << a << endl;
  check_ok(a);
  dump.str("");
  a.ascii_dump(dump);
  check_dump(dump, "0 0 0 0 m 3\n");
  if (!a.is_trivial_true()) {
    nout << "is_trivial_true should have returned true." << endl;
    exit(1);
  }
}

/* Use is_trivial_false.  */

static void
test11() {
  Variable x(0);
  Variable y(1);

  nout << "test11" << endl;

  stringstream dump;

  Congruence a(0*x + 0*y %= 17);
  a /= 0;
  nout << a << endl;
  check_ok(a);
  a.ascii_dump(dump);
  check_dump(dump, "-17 0 0 m 0\n");
  if (!a.is_trivial_false()) {
    nout << "is_trivial_false should have returned true." << endl;
    exit(1);
  }

  a = Congruence((0*x + 0*y + 3 %= 0) / 0);
  nout << a << endl;
  a.strong_normalize();
  check_ok(a);
  dump.str("");
  a.ascii_dump(dump);
  check_dump(dump, "3 0 0 m 0\n");
  if (!a.is_trivial_false()) {
    nout << "is_trivial_false should have returned true." << endl;
    exit(1);
  }
}

/* Negative modulus.  */

static void
test12() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  nout << "test12" << endl;

  stringstream dump;

  Congruence a((x + 4*y + 3*z %= -4) / -3);
  nout << a << endl;
  a.strong_normalize();
  check_ok(a);
  a.ascii_dump(dump);
  check_dump(dump, "1 1 4 3 m 3\n");

  Congruence b((x + 4*y %= -1 - 3*z) / -3);
  nout << b << endl;
  check_ok(b);
  b.strong_normalize();
  dump.str("");
  b.ascii_dump(dump);
  check_dump(dump, "1 1 4 3 m 3\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Negative modulus and negative first coefficient.  */

static void
test13() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);
  Variable x3(3);
  Variable x4(4);

  nout << "test13" << endl;

  stringstream dump;

  Congruence a((-x0 + 4*x1 + 3*x2 + 17*x3 + 2*x4 %= -4) / -3);
  nout << a << endl;
  check_ok(a);
  a.strong_normalize();
  a.ascii_dump(dump);
  check_dump(dump, "2 1 -4 -3 -17 -2 m 3\n");

  Congruence b((-x0 + 4*x1 %= - 3*x2 - 17*x3 - 2*x4 - 4) / -3);
  nout << b << endl;
  b.strong_normalize();
  check_ok(b);
  dump.str("");
  b.ascii_dump(dump);
  check_dump(dump, "2 1 -4 -3 -17 -2 m 3\n");

  if (a != b) {
    nout << "Congruences should be equal." << endl;
    exit(1);
  }
}

/* Create from empty linear expression.  */

static void
test14() {
  nout << "test14" << endl;

  stringstream dump;

  Linear_Expression le;
  Congruence a(le %= le);
  nout << a << endl;
  a.strong_normalize();
  check_ok(a);
  a.ascii_dump(dump);
  check_dump(dump, "0 m 1\n");

  a = (le %= 0);
  nout << a << endl;
  a.strong_normalize();
  check_ok(a);
  dump.str("");
  a.ascii_dump(dump);
  check_dump(dump, "0 m 1\n");
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
