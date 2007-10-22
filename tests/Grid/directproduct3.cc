/* Test Direct_Product<NNC_Polyhedron, Grid>.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

// FIXME: Test both Direct_Product and Open_Product.
// #define OPEN_PRODUCT
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

namespace {

// contains()
bool
test01() {
  Variable A(0);

  Product dp(1);
  dp.add_constraint(A < 3);
  dp.add_congruence((A %= 3) / 2);

  Product dp2(1);
  dp2.add_constraint(A < 3);
  dp2.add_congruence(A %= 3);

  bool ok = !dp.contains(dp2);

  return ok;
}

// contains()
bool
test02() {
  Variable A(0);

  Product dp(1);
  dp.add_congruence(A %= 3);
  dp.add_constraint(A < 3);

  Product dp2(1);
  dp2.add_congruence((A %= 3) / 2);
  dp2.add_constraint(A < 2);

  bool ok = dp.contains(dp2);

  return ok;
}

// contains(), due to intersection.
bool
test03() {
  Variable A(0);

  Product dp(1);
  dp.add_congruence((A == 0) / 0);

  Product dp2(1);
  dp2.add_constraint(A < 2);
  dp2.add_constraint(A > -1);
  dp2.add_congruence((A %= 0) / 3);

  bool ok = !/* FIX */ dp.contains(dp2);

  return ok;
}

// strictly_contains()
bool
test04() {
  Variable A(0);

  Product dp(1);
  dp.add_constraint(A < 1);
  dp.add_congruence(A %= 3);

  Product dp2(1);
  dp2.add_constraint(A < 2);
  dp2.add_congruence(A %= 3);

  bool ok = !dp.strictly_contains(dp2);

  return ok;
}

// strictly_contains()
bool
test05() {
  Variable A(0);

  Product dp(1);
  dp.add_constraint(A < 3);
  dp.add_congruence(A %= 3);

  Product dp2(1);
  dp2.add_constraint(A < 2);
  dp2.add_congruence(A %= 3);

  bool ok = !dp.strictly_contains(dp2);

  return ok;
}

// strictly_contains()
bool
test06() {
  Variable A(0);

  Product dp(1);
  dp.add_constraint(A < 3);
  dp.add_congruence(A %= 3);

  Product dp2(1);
  dp2.add_constraint(A < 2);
  dp2.add_congruence((A %= 3) / 2);

  bool ok = dp.strictly_contains(dp2);

  return ok;
}

// strictly_contains(), due to intersection.
bool
test07() {
  Variable A(0);

  Product dp(1);
  dp.add_congruence(A %= 3);

  Product dp2(1);
  dp2.add_congruence((A %= 3) / 2);

  bool ok = !/* FIX */ dp.strictly_contains(dp2);

  return ok;
}

// strictly_contains(), due to intersection.
bool
test08() {
  Variable A(0);

  Product dp(1);
  dp.add_congruence((A %= 0) / 6);

  Product dp2(1);
  dp2.add_constraint(A < 2);
  dp2.add_constraint(A > -1);
  dp2.add_congruence((A %= 0) / 3);

  bool ok = !/*FIX*/ dp.strictly_contains(dp2);

  return ok;
}

// FIXME: Waiting for covering box methods, details in
//        Direct_Product.defs.hh.
#if 0
// get_covering_box(box), via grid.
bool
test09() {
  Variable A(0);

  Rational_Box box(1);

  Product dp(1);
  dp.add_congruence((A %= 0) / 3);

  dp.get_covering_box(box);

  Rational_Box known_box(1);
  known_box.add_constraint(A >= 0);
  known_box.add_constraint(A <= 3);

  bool ok = (box == known_box);

  return ok;
}

// get_covering_box(box), via polyhedron.
bool
test10() {
  Variable B(1);

  Rational_Box box(1);

  Product dp(2);
  dp.add_constraint(B < 3);
  dp.add_constraint(B > 0);

  dp.get_covering_box(box);

  Rational_Box known_box(1);
  known_box.add_constraint(B == 0 /* FIX */);

  bool ok = (box == known_box);

  return ok;
}

// get_covering_box(box), via intersection.
bool
test11() {
  Variable A(0);
  Variable B(1);

  Rational_Box box(2);

  Product dp(2);
  dp.add_constraint(B <= 0);
  dp.add_constraint(B >= 0);
  dp.add_congruence(A - B %= 0);

  dp.get_covering_box(box);

  Rational_Box known_box(2);
  known_box.add_constraint(A >= 0);
  known_box.add_constraint(A <= 1);

  bool ok = !/* FIX */ (box == known_box);

  return ok;
}
#endif

// intersection_assign()
bool
test12() {
  Variable A(0);
  Variable B(1);

  Product dp1(3);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  Product dp2(3);
  dp2.add_constraint(A <= 0);
  dp2.add_congruence((A %= 0) / 7);

  dp1.intersection_assign(dp2);

  Product known_dp(3);
  known_dp.add_congruence((A %= 0) / 14);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(A <= 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// upper_bound_assign(dp2)
bool
test13() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs(A == 9);

  Product dp1(cs);

  Product dp2(1);
  dp2.add_constraint(A == 19);

  dp1.upper_bound_assign(dp2);

  Product known_dp(1);
  known_dp.add_constraint(A >= 9);
  known_dp.add_constraint(A <= 19);
  known_dp.add_congruence((A %= 9) / 10);

  bool ok = (dp1 == known_dp);

  return ok;
}

// upper_bound_assign_if_exact()
bool
test14() {
  Variable A(0);
  Variable B(1);

  Product dp1(3);
  dp1.add_constraint(B == 0);

  Product dp2(3);
  dp2.add_constraint(B == 0);
  dp2.add_constraint(A == 12);
  dp2.add_constraint(A == 16);

  dp1.upper_bound_assign_if_exact(dp2);

  Product known_dp(3);
  known_dp.add_constraint(B == 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// difference_assign()
bool
test15() {
  Variable A(0);
  Variable B(1);

  Product dp1(3);
  dp1.add_constraint(A > 0);
  dp1.add_congruence((A - B %= 0) / 2);

  Product dp2(3);
  dp2.add_constraint(A > 3);
  dp2.add_congruence((A - B %= 0) / 4);

  dp1.difference_assign(dp2);

  Product known_dp(3);
  known_dp.add_constraint(A > 0);
  known_dp.add_constraint(A <= 3);
  known_dp.add_congruence((A - B %= 2) / 4);

  bool ok = (dp1 == known_dp);

  return ok;
}

// add_space_dimensions_and_embed()
bool
test16() {
  Variable A(0);
  Variable B(1);

  Product dp1(2);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  dp1.add_space_dimensions_and_embed(3);

  Product known_dp(5);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(A >= 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// add_space_dimensions_and_project()
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp1(2);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  dp1.add_space_dimensions_and_project(1);

  Product known_dp(3);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(C == 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// concatenate_assign()
bool
test18() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Product dp1(2);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  Product dp2(2);
  dp2.add_constraint(A < 1);
  dp2.add_constraint(B > 0);

  dp1.concatenate_assign(dp2);

  Product known_dp(4);
  known_dp.add_constraint(A >= 0);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(C < 1);
  known_dp.add_constraint(D > 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// remove_space_dimensions()
bool
test19() {
  Variable A(0);
  Variable C(2);
  Variable D(3);

  Product dp(4);
  dp.add_constraint(A >= 0);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A - C %= 0) / 2);

  Variables_Set vars;
  vars.insert(C);
  vars.insert(D);

  dp.remove_space_dimensions(vars);

  Product known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_congruence((A %= 0) / 2);

  bool ok = (dp == known_dp);

  return ok;
}

// remove_higher_space_dimensions()
bool
test20() {
  Variable A(0);
  Variable C(2);
  Variable D(3);

  Product dp(4);
  dp.add_constraint(A >= 0);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A - C %= 0) / 2);

  dp.remove_higher_space_dimensions(2);

  Product known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_congruence((A %= 0) / 2);

  bool ok = (dp == known_dp);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST_NNC(test01);
  DO_TEST_NNC(test02);
  DO_TEST_NNC(test03);
  DO_TEST_NNC(test04);
  DO_TEST_NNC(test05);
  DO_TEST_NNC(test06);
  DO_TEST(test07);
  DO_TEST_NNC(test08);
#if 0
  DO_TEST(test09);
  //DO_TEST(test10);
  DO_TEST(test11);
#endif
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST_NNC(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST_NNC(test18);
  DO_TEST(test19);
  DO_TEST(test20);
END_MAIN
