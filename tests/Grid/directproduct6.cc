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

// relation_with a generator
bool
test13() {
  Variable A(0);
  Variable B(1);

  Generator pnt(point(A + B));

  Product dp(2);

  bool ok = Poly_Gen_Relation::subsumes() == dp.relation_with(pnt);

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

// relation_with a constraint
bool
test14() {
  Variable A(0);
  Variable B(1);

  Constraint c(A == 2);

  Product dp(2);

  bool ok = Poly_Con_Relation::strictly_intersects() == dp.relation_with(c);

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

// Empty product; relation_with a constraint.
bool
test15() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A == 1);
  dp.add_congruence((A %= 2) / 0);

  bool ok = (dp.relation_with(B == 0)
	     == (Poly_Con_Relation::is_included()
		 && Poly_Con_Relation::is_disjoint()
	         && Poly_Con_Relation::saturates())
	     && dp.relation_with(B >= 0)
	     == (Poly_Con_Relation::is_included()
		 && Poly_Con_Relation::is_disjoint()
	         && Poly_Con_Relation::saturates()));

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

// A product in 3D; relation_with a constraint.
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3, EMPTY);
  dp.add_grid_generator(grid_point(A + B + C));
  dp.add_grid_generator(grid_line(A - 2*B + 3*C));
  dp.add_grid_generator(parameter(A - B, 3));
  dp.add_generator(point(A + B + C));
  dp.add_generator(line(A - 2*B + 3*C));
  dp.add_generator(ray(A - B));

  bool okdp1 = (dp.domain1().relation_with(2*A + B >= 3)
	     == Poly_Con_Relation::strictly_intersects());

  bool okdp2 = (dp.domain2().relation_with(2*A + B >= 3)
	       == Poly_Con_Relation::is_included());

  bool ok = (okdp1 && okdp2
             && dp.relation_with(A + B + C == 0)
	     == Poly_Con_Relation::strictly_intersects()
	     && dp.relation_with(A + B == 0)
	     == Poly_Con_Relation::strictly_intersects()
	     && dp.relation_with(A == 0)
	     == Poly_Con_Relation::strictly_intersects()
	     && dp.relation_with(Linear_Expression(0) == 0)
	     == (Poly_Con_Relation::is_included()
		 && Poly_Con_Relation::saturates())
	     && dp.relation_with(2*A + B >= 3)
	     == Poly_Con_Relation::is_included()
	     && dp.relation_with(3*A + 3*B + C >= 7)
	     == (Poly_Con_Relation::is_included()
		 && Poly_Con_Relation::saturates()));

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
#if 0
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
#endif
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
END_MAIN
