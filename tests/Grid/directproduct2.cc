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

// is_empty() where both domain objects have points.
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence(A %= 9);
  dp.add_congruence(B + C %= 3);

  bool ok = !dp.is_empty();

  return ok;
}

// is_empty() where one domain object is empty.
bool
test02() {
  Variable A(0);

  Product dp(3);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A %= 1) / 2);

  bool ok = dp.is_empty();

  return ok;
}

// is_empty() where both domain objects are empty.
bool
test03() {
  Variable A(0);

  Product dp(3);
  dp.add_constraint(A == 1);
  dp.add_constraint(A == 3);

  bool ok = dp.is_empty();

  return ok;
}

// is_universe() where both domain objects are empty.
bool
test04() {
  Product dp(3, EMPTY);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where one domain object is universe.
bool
test05() {
  Variable A(0);

  Product dp(3);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A %= 1) / 2);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where both domain objects are universe.
bool
test06() {
  Product dp(3);

  bool ok = dp.is_universe();

  return ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// open.
bool
test07() {
  Variable A(0);

  Product dp(3);
  dp.add_constraint(A < 3);
  dp.add_congruence((A %= 0) / 3);

  bool ok = !dp.is_topologically_closed();

  return ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// closed.
bool
test08() {
  Variable A(0);

  Product dp(3);
  dp.add_constraint(A <= 3);
  dp.add_congruence((A %= 0) / 3);

  bool ok = dp.is_topologically_closed();

  return ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// open and the intersection is closed.
bool
test09() {
  Variable A(0);

  Product dp(3);
  dp.add_constraint(A < 3);
  dp.add_congruence((A %= 0) / 4);

  bool ok = !/* FIX */ dp.is_topologically_closed();

  return ok;
}

// is_disjoint_from(dp), due to the Polyhedra.
bool
test10() {
  Variable B(1);

  Product dp1(12);
  dp1.add_constraint(B < 3);

  Product dp2(12);
  dp2.add_constraint(B > 3);

  bool ok = dp1.is_disjoint_from(dp2);

  return ok;
}

// is_disjoint_from(dp), due to the Grids.
bool
test11() {
  Variable A(0);

  Product dp1(3);
  dp1.add_congruence((A %= 0) / 7);

  Product dp2(3);
  dp2.add_congruence((A %= 1) / 7);

  bool ok = dp1.is_disjoint_from(dp2);

  return ok;
}

// is_disjoint_from(dp), due to either.
bool
test12() {
  Variable A(0);

  Product dp1(3);
  dp1.add_constraint(A < 3);
  dp1.add_congruence((A %= 0) / 7);

  Product dp2(3);
  dp2.add_constraint(A > 3);
  dp2.add_congruence((A %= 1) / 7);

  bool ok = dp1.is_disjoint_from(dp2);

  return ok;
}

// is_disjoint_from(dp), due to both.
bool
test13() {
  Variable A(0);

  Product dp1(3);
  dp1.add_constraint(A < 6);
  dp1.add_congruence((A %= 1) / 7);

  Product dp2(3);
  dp2.add_constraint(A > 3);
  dp2.add_congruence((A %= 1) / 14);

  bool ok = !/* FIX */ dp1.is_disjoint_from(dp2);

  return ok;
}

// is_disjoint_from(dp), due to the intersection of the entire direct
// products (i.e. the dp1 and dp2 polyhedron components intersect, as
// do the grid components).
bool
test14() {
  Variable A(0);
  Variable B(1);

  Product dp1(2);
  dp1.add_constraint(A <= 4);
  dp1.add_constraint(A >= 0);
  dp1.add_constraint(A - B <= 0);
  dp1.add_constraint(A - B >= 2);
  dp1.add_congruence((A %= 0) / 2);
  dp1.add_congruence((A %= 0) / 4);

  Product dp2(2);
  dp2.add_constraint(A <= 4);
  dp2.add_constraint(A <= 0);
  dp2.add_constraint(A + B >= 4);
  dp2.add_constraint(A + B <= 6);
  // Same grid as dp1.
  dp2.add_congruence((A %= 0) / 2);
  dp2.add_congruence((A %= 0) / 4);

  bool ok = dp1.is_disjoint_from(dp2);

  return ok;
}

// is_discrete(), due to grid.
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_constraint(A == 1);
  dp.add_constraint(B == 2);
  dp.add_constraint(C <= 3);
  dp.add_congruence((C %= 0) / 3);

  bool ok = dp.is_discrete();

  return ok;
}

// is_discrete(), due to polyhedron.
bool
test16() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A <= 3);
  dp.add_constraint(A >= 3);
  dp.add_constraint(B == 0);

  bool ok = dp.is_discrete();

  return ok;
}

// is_discrete(), due to intersection.
bool
test17() {
  Variable A(0);
  Variable B(1);

  Product dp(3, EMPTY);
  dp.add_grid_generator(grid_point());
  dp.add_grid_generator(grid_line(A + B));
  dp.add_generator(point());
  dp.add_generator(line(A));

  bool ok = dp.is_discrete();

  return ok;
}

// is_bounded(), due to polyhedron.
bool
test18() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A > 1);
  dp.add_constraint(A < 4);
  dp.add_constraint(B > 1);
  dp.add_constraint(B < 4);
  dp.add_congruence((A %= 1) / 3);

  bool ok = dp.is_bounded();

  return ok;
}

// is_bounded(), due to grid.
bool
test19() {
  Variable A(0);

  Product dp(2, EMPTY);
  dp.add_generator(point());
  dp.add_generator(ray(-A));
  dp.add_grid_generator(grid_point());

  bool ok = dp.is_bounded();

  return ok;
}

// is_bounded(), due to intersection.
bool
test20() {
  Variable A(0);
  Variable B(1);

  Product dp(2, EMPTY);
  dp.add_grid_generator(grid_point());
  dp.add_grid_generator(grid_line(A + B));
  dp.add_generator(point());
  dp.add_generator(line(A));

#ifndef OPEN_PRODUCT
  bool ok = !dp.is_bounded();
#else
  bool ok = dp.is_bounded();
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
  DO_TEST_NNC(test07);
  DO_TEST(test08);
  DO_TEST_NNC(test09);
  DO_TEST_NNC(test10);
  DO_TEST(test11);
  DO_TEST_NNC(test12);
  DO_TEST_NNC(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
#ifdef OPEN_PRODUCT
  DO_TEST(test17);
#endif
  DO_TEST_NNC(test18);
  DO_TEST(test19);
  DO_TEST(test20);
END_MAIN
