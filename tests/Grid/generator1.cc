/* Test class Grid_Generator.
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

// Point.
static bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(grid_point(A + 2*B + 3*C));

  Grid_Generator b(grid_point(3*C + A + 2*B));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Point with divisor.
static bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(grid_point(A + 2*B + 3*C, 5));

  Grid_Generator b(grid_point(15*C + 5*A + 10*B, 25));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Line.
static bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(grid_line(A + 2*B + 3*C));

  Grid_Generator b(grid_line(15*C + 5*A + 10*B));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Parameter.
static bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(parameter(A + 2*B + 3*C));

  Grid_Generator b(parameter(2*B + 2*A - A + 3*C));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Parameter with divisor.
static bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(parameter(A + 2*B + 3*C, 4));

  Grid_Generator b(parameter(6*B + 3*A + 9*C, 12));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Negative first coefficient.
static bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(grid_point(- A + 2*B + 3*C, 4));

  Grid_Generator b(grid_point(6*B - 3*A + 9*C, 12));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Construction from Generator.
static bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(grid_point(- A + 2*B + 3*C, 4));

  Grid_Generator b(grid_point(6*B - 3*A + 9*C, 12));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Construction from reference to Generator.
static bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator g = grid_point(- A + 2*B + 3*C, 4);
  Grid_Generator& g_ref = g;

  Grid_Generator a(g_ref);

  Grid_Generator b(grid_point(6*B - 3*A + 9*C, 12));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Create from empty linear expression.
static bool
test09() {
  Linear_Expression le;
  Grid_Generator a(grid_point(le));

  Grid_Generator b(grid_point(le));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Parameter with negative divisor.
static bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(parameter(-A - 2*B - 3*C, -4));

  Grid_Generator b(parameter(6*B + 3*A + 9*C, 12));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Point with negative divisor.
static bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator a(grid_point(A + 2*B + 3*C, 5));

  Grid_Generator b(grid_point(-15*C - 5*A - 10*B, -25));

  bool ok = (a == b);

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");

  return ok;
}

// Parameter with zero divisor.
static bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  try {
    Grid_Generator a(parameter(-A - 2*B - 3*C, 0));
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Point with zero divisor.
static bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  try {
    Grid_Generator a(grid_point(A + 2*B + 3*C, 0));
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Line.
static bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  try {
    Grid_Generator a(grid_line(0*A + 0*B + 0*C));
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// is_equivalent_to() and is_equal_to(): generators have different types
static bool
test15() {
  Variable A(0);

  Grid_Generator a(grid_point(A));
  Grid_Generator b(grid_point(A, 2));
  Grid_Generator c(parameter(A));
  Grid_Generator d(grid_line(A));
  Grid_Generator e(grid_point(2*A, 4));
  Grid_Generator f(parameter(2*A, 2));

  bool ok = (!c.is_equivalent_to(d));
  ok &= (!a.is_equivalent_to(b));
  ok &= (!c.is_equal_to(d));
  ok &= (!a.is_equal_to(b));
  ok &= (b.is_equal_to(e));
  ok &= (!c.is_equal_to(f));

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");
  print_generator(c, "*** c ***");
  print_generator(d, "*** d ***");
  print_generator(e, "*** e ***");
  print_generator(f, "*** f ***");

  return ok;
}

// is_equivalent_to(): generators have different space dimensions
static bool
test16() {
  Variable A(0);
  Variable B(1);

  Grid_Generator a(grid_point(A));
  Grid_Generator b(grid_point(B));
  Grid_Generator c(parameter(A));
  Grid_Generator d(parameter(B));
  Grid_Generator e(grid_line(B));
  Grid_Generator f(grid_line(A));

  bool ok = (!a.is_equivalent_to(b));
  ok &= (!c.is_equivalent_to(d));
  ok &= (!e.is_equivalent_to(f));

  print_generator(a, "*** a ***");
  print_generator(b, "*** b ***");
  print_generator(c, "*** c ***");
  print_generator(d, "*** d ***");
  print_generator(e, "*** e ***");
  print_generator(f, "*** f ***");

  return ok;
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
END_MAIN
