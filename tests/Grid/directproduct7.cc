/* Test Grid::congruences().
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
site: http://www.cs.unipr.it/ppl/ .

Tests bounds_from_above(), bounds_from_below(), maximize() and minimize()
for the product domains */

#include "ppl_test.hh"

namespace {


// FIXME: Test both Direct_Product and Open_Product.
#define OPEN_PRODUCT
// FIXME: Also test the other combination (Product<Ph, Grid>).
#define GRID_IS_D1
// FIXME: Also test with C_Polyhedron.
#define PH_IS_NNC

#ifdef PH_IS_NNC
#define DO_TEST_NNC(test) DO_TEST(test)
#else
#define NNC_Polyhedron C_Polyhedron
#define DO_TEST_NNC(test)
#endif

#ifdef OPEN_PRODUCT
#ifdef GRID_IS_D1
typedef Open_Product<Grid, NNC_Polyhedron> Product;
#else
typedef Open_Product<NNC_Polyhedron, Grid> Product;
#endif
#else
#ifdef GRID_IS_D1
typedef Direct_Product<Grid, NNC_Polyhedron> Product;
#else
typedef Direct_Product<NNC_Polyhedron, Grid> Product;
#endif
#endif

// Empty.
bool
test01() {
  Product dp(7, EMPTY);

  bool ok = (dp.bounds_from_above(Linear_Expression(0))
	     && dp.bounds_from_below(Linear_Expression(0)));

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Zero dimension empty.
bool
test02() {
  Product dp(0, EMPTY);

  bool ok = (dp.bounds_from_above(Linear_Expression(3))
	     && dp.bounds_from_below(Linear_Expression(3)));

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Zero dimension universe.
bool
test03() {
  Product dp(0);

  bool ok = (dp.bounds_from_above(Linear_Expression(1))
	     && dp.bounds_from_below(Linear_Expression(1)));

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Point.
bool
test04() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A == 1);
  dp.add_constraint(3*B == 2);

  Linear_Expression le = A + B;
  bool ok = dp.bounds_from_above(le)
    && dp.bounds_from_below(le);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// only one component is bounded.
bool
test05() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A + B >= 1);
  dp.add_constraint(A + B <= 1);
  dp.add_congruence(3*B %= 2);

  Linear_Expression le = A + B;
  bool ok = dp.bounds_from_above(le)
    && dp.bounds_from_below(le);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Line and neither component is bounded.
bool
test06() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(B == 1);

  Linear_Expression le = 2*A - B;

  bool ok = !dp.bounds_from_above(le)
    && !dp.bounds_from_below(le);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Empty. maximize() and minimize()
bool
test07() {
  Product dp(7, EMPTY);

  Coefficient extr_n, extr_d;
  bool dummy;

  bool ok = (!dp.maximize(Linear_Expression(0), extr_n, extr_d, dummy));

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Zero dimension empty.
bool
test08() {
  Product dp(0, EMPTY);

  Coefficient extr_n, extr_d;
  bool dummy;

  bool ok = (!dp.maximize(Linear_Expression(0), extr_n, extr_d, dummy));

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Zero dimension universe.
bool
test09() {
  Product dp(0);

  Coefficient extr_n, extr_d;
  bool dummy;

  bool ok = dp.maximize(Linear_Expression(0), extr_n, extr_d, dummy);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Point.
bool
test10() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A == 1);
  dp.add_constraint(3*B == 2);

  Linear_Expression le = A + B;

  Coefficient max_n, max_d, min_n, min_d;

  bool max, min;

  bool ok = dp.maximize(le, max_n, max_d, max)
    && dp.minimize(le, min_n, min_d, min);

  ok &= max && min && max_n == 5 && max_d == 3 && min_n == 5 && min_d == 3;

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// only one component is bounded.
bool
test11() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A + B >= 1);
  dp.add_constraint(A + B <= 1);
  dp.add_congruence(3*B %= 2);
;
  Linear_Expression le = A + B;

  Coefficient max_n, max_d, min_n, min_d;

  bool max, min;

  bool ok = dp.maximize(le, max_n, max_d, max)
    && dp.minimize(le, min_n, min_d, min);

  ok &= max && min && max_n == 1 && max_d == 1 && min_n == 1 && min_d == 1;

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Line and neither component is bounded.
bool
test12() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(B == 1);

  Linear_Expression le = 2*A - B;

  Coefficient extr_n, extr_d;
  bool max, min;

  bool ok = !dp.maximize(le, extr_n, extr_d, max)
    && !dp.minimize(le, extr_n, extr_d, min);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// only one component is strictly bounded.
bool
test13() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A + B > 0);
  dp.add_constraint(A + B < 1);
  dp.add_congruence(3*B %= 2);
;
  Linear_Expression le = A + B;

  Coefficient max_n, max_d, min_n, min_d;

  bool max, min;

  bool ok = dp.maximize(le, max_n, max_d, max)
    && dp.minimize(le, min_n, min_d, min);

  ok &= !max && !min && max_n == 1 && max_d == 1 && min_n == 0 && min_d == 1;

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Non-empty product. bounded_affine_image/3
bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((A ==  0) / 0);
  dp.add_congruence((B ==  0) / 0);
  dp.add_congruence((C == -2) / 0);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  dp.bounded_affine_image(A, 7-B, B+3);

  Product known_dp(3);
  known_dp.add_constraint(C == -2);
  known_dp.add_constraint(B == 0);
  known_dp.add_constraint(A <= 3);
  known_dp.add_constraint(A + B >= 7);

  bool ok = (dp == known_dp);


#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Empty grid. bounded_affine_image/3
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_constraint(A ==  0);
  dp.add_constraint(A ==  1);
  dp.add_constraint(C == -2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  dp.bounded_affine_image(A, 7-B, B+3);

  Product known_dp(3, EMPTY);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Non-empty product. bounded_affine_preimage/3
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((A ==  0) / 0);
  dp.add_congruence((B ==  0) / 0);
  dp.add_congruence((C == -2) / 0);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  dp.bounded_affine_preimage(A, 7-B, B+3);

  Constraint_System cs;
  cs.insert(C == -2);
  cs.insert(B == 0);
  cs.insert(C >= 3);

  Product known_dp(3);
  known_dp.add_constraints(cs);

  bool ok = (dp == known_dp);


#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  return ok;
}

// Empty grid. bounded_affine_preimage/3
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_constraint(A ==  0);
  dp.add_constraint(A ==  1);
  dp.add_constraint(C == -2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

  dp.bounded_affine_preimage(A, 7-B, B+3);

  Product known_dp(3, EMPTY);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(),
     "*** dp.domain1() ***");
  print_congruences(dp.domain2(),
     "*** dp.domain2() ***");
#endif

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
  DO_TEST(test17);
END_MAIN
