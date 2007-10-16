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
site: http://www.cs.unipr.it/ppl/ . */

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

// A point in both domains.
bool
test01() {
  Variable A(0);

  Product dp(1);

  dp.add_constraint(A == 7);

  Constraint_System cs = dp.constraints();

  Product dp1(cs);

  bool ok = (dp1 == dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  return ok;
}

// A point specified by inequalities.
bool
test02() {
  Variable A(0);

  Product dp(1);
  dp.add_constraint(A >= 7);
  dp.add_constraint(A <= 7);

  Constraint_System cs = dp.constraints();

  Product dp1(cs);

  bool ok = (dp1 == dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
  print_congruences(dp1.domain1(), "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(), "*** dp1.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
  print_constraints(dp1.domain1(), "*** dp1.domain1() ***");
  print_congruences(dp1.domain2(), "*** dp1.domain2() ***");
#endif

  return ok;
}

// A point in both domains.
bool
test03() {
  Variable A(0);

  Product dp(1);

  dp.add_constraint(A == 7);

  Constraint_System cs = dp.minimized_constraints();

  Product dp1(cs);

  bool ok = (dp1 == dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  return ok;
}

// A point specified by inequalities.
bool
test04() {
  Variable A(0);

  Product dp(1);
  dp.add_constraint(A >= 7);
  dp.add_constraint(A <= 7);

  Constraint_System cs = dp.minimized_constraints();

  Product dp1(cs);

  bool ok = (dp1 == dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
  print_congruences(dp1.domain1(), "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(), "*** dp1.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
  print_constraints(dp1.domain1(), "*** dp1.domain1() ***");
  print_congruences(dp1.domain2(), "*** dp1.domain2() ***");
#endif

  return ok;
}

// add_constraints
bool
test05() {

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B == 0);

  Product dp(2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  dp.add_constraints(cs);

  Product known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(B == 0);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.add_constraints(cs).domain1() ***");
  print_constraints(dp.domain2(), "*** dp.add_constraints(cs).domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.add_constraints(cs).domain1() ***");
  print_congruences(dp.domain2(), "*** dp.add_constraints(cs).domain2() ***");
#endif

  return ok;
}

// add_recycled_constraints
bool
test06() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B <= 0);

  Product dp(2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  dp.add_recycled_constraints(cs);

  Product known_dp(2);
  known_dp.add_constraint(A + B <= 0);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.add_recycled_constraints(cs).domain1() ***");
  print_constraints(dp.domain2(),
    "*** dp.add_recycled_constraints(cs).domain2() ***");
#else
  print_constraints(dp.domain1(),
    "*** dp.add_recycled_constraints(cs).domain1() ***");
  print_congruences(dp.domain2(),
    "*** dp.add_recycled_constraints(cs).domain2() ***");
#endif

  return ok;
}

// add_constraints_and_minimize
bool
test07() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A + B == 0);

  Product dp(2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  dp.add_constraints_and_minimize(cs);

  Product known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(A + B == 0);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.add_constraints_and_minimize(cs).domain1() ***");
  print_constraints(dp.domain2(),
    "*** dp.add_constraints_and_minimize(cs).domain2() ***");
#else
  print_constraints(dp.domain1(),
    "*** dp.add_constraints_and_minimize(cs).domain1() ***");
  print_congruences(dp.domain2(),
    "*** dp.add_constraints_and_minimize(cs).domain2() ***");
#endif

  return ok;
}

// add_recycled_constraints_and_minimize
bool
test08() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(B >= 0);
  cs.insert(A - B == 0);

  Product dp(2);

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

  dp.add_recycled_constraints_and_minimize(cs);

  Product known_dp(2);
  known_dp.add_constraint(B >= 0);
  known_dp.add_constraint(A - B == 0);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.add_recycled_constraints_and_minimize(cs).domain1() ***");
  print_constraints(dp.domain2(),
    "*** dp.add_recycled_constraints_and_minimize(cs).domain2() ***");
#else
  print_constraints(dp.domain1(),
    "*** dp.add_recycled_constraints_and_minimize(cs).domain1() ***");
  print_congruences(dp.domain2(),
    "*** dp.add_recycled_constraints_and_minimize(cs).domain2() ***");
#endif

  return ok;
}
// add_congruences
bool
test09() {

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);
  cgs.insert((B == 0) / 2);

  Product dp(2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  dp.add_congruences(cgs);

  Product known_dp(2);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_congruence((B == 0) / 2);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.add_congruences(cgs).domain1() ***");
  print_constraints(dp.domain2(), "*** dp.add_congruences(cgs).domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.add_congruences(cgs).domain1() ***");
  print_congruences(dp.domain2(), "*** dp.add_congruences(cgs).domain2() ***");
#endif

  return ok;
}

#if (0)
// add_recycled_congruences
bool
test10() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A + B %= 0) / 2);

  Product dp(2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  dp.add_recycled_congruences(cgs);

  Product known_dp(2);
  known_dp.add_congruence((A + B %= 0) / 2);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.add_recycled_congruences(cgs).domain1() ***");
  print_constraints(dp.domain2(),
    "*** dp.add_recycled_congruences(cgs).domain2() ***");
#else
  print_constraints(dp.domain1(),
    "*** dp.add_recycled_congruences(cgs).domain1() ***");
  print_congruences(dp.domain2(),
    "*** dp.add_recycled_congruences(cgs).domain2() ***");
#endif

  return ok;
}

// add_congruences_and_minimize
bool
test11() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);
  cgs.insert(A + B == 0);

  Product dp(2);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(), "*** dp.domain1() ***");
  print_constraints(dp.domain2(), "*** dp.domain2() ***");
#else
  print_constraints(dp.domain1(), "*** dp.domain1() ***");
  print_congruences(dp.domain2(), "*** dp.domain2() ***");
#endif

  dp.add_congruences_and_minimize(cgs);

  Product known_dp(2);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_congruence(A + B == 0);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.add_congruences_and_minimize(cgs).domain1() ***");
  print_constraints(dp.domain2(),
    "*** dp.add_congruences_and_minimize(cgs).domain2() ***");
#else
  print_constraints(dp.domain1(),
    "*** dp.add_congruences_and_minimize(cgs).domain1() ***");
  print_congruences(dp.domain2(),
    "*** dp.add_congruences_and_minimize(cgs).domain2() ***");
#endif

  return ok;
}

// add_recycled_congruences_and_minimize
bool
test12() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((B %= 0) / 2);
  cgs.insert(A - B == 0);

  Product dp(2);

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

  dp.add_recycled_congruences_and_minimize(cgs);

  Product known_dp(2);
  known_dp.add_congruence((B %= 0) / 2);
  known_dp.add_congruence(A - B == 0);

  bool ok = (dp == known_dp);

#ifdef GRID_IS_D1
  print_congruences(dp.domain1(),
    "*** dp.add_recycled_congruences_and_minimize(cgs).domain1() ***");
  print_constraints(dp.domain2(),
    "*** dp.add_recycled_congruences_and_minimize(cgs).domain2() ***");
#else
  print_constraints(dp.domain1(),
    "*** dp.add_recycled_congruences_and_minimize(cgs).domain1() ***");
  print_congruences(dp.domain2(),
    "*** dp.add_recycled_congruences_and_minimize(cgs).domain2() ***");
#endif

  return ok;
}

#endif

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
#if 0
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
#endif
END_MAIN
