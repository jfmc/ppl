/* Test Direct_Product<NNC_Polyhedron, Grid>.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

// FIXME: Spread these tests into many files.

using namespace Parma_Polyhedra_Library::IO_Operators;

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

namespace {

// Product()
bool
test01() {
  Product dp1;

  Product dp2(0, UNIVERSE);

  bool ok = (dp1 == dp2);

  return ok;
}

// Product(dims, type)
bool
test02() {
  Product dp1(3);

  Product dp2(3, EMPTY);

  bool ok = (dp1 != dp2);

  return ok;
}

// Product(ccgs), add_congruence(cg)
bool
test03() {
  Variable A(0);

  const Congruence_System cgs((A %= 0) / 4);

  Product dp1(cgs);

  Product dp2(1);
  dp2.add_congruence((A %= 0) / 4);

  bool ok = (dp1 == dp2);

  return ok;
}

// Product(cgs), domain1(), domain2()
bool
test04() {
  Variable A(0);

  Congruence_System cgs((A %= 0) / 4);

  Product dp(cgs);

  NNC_Polyhedron known_ph(1);

  Grid known_gr(1);
  known_gr.add_congruence((A %= 0) / 4);

#ifdef GRID_IS_D1
  bool ok = (dp.domain1() == known_gr
	     && dp.domain2() == known_ph);
#else
  bool ok = (dp.domain1() == known_ph
	     && dp.domain2() == known_gr);
#endif

  return ok;
}

// Product(ccs), add_constraint(cc)
bool
test05() {
  Variable A(0);

  const Constraint_System cs(A >= 0);

  Product dp1(cs);

  Product dp2(1);
  dp2.add_constraint(static_cast<const Constraint>(A >= 0));

  bool ok = (dp1 == dp2);

  return ok;
}

// Product(cs)
bool
test06() {
  Variable A(0);

  Constraint_System cs(A == 9);

  Product dp1(cs);

  Product dp2(1);
  dp2.add_constraint(A == 9);

  Grid known_gr(1);
  known_gr.add_congruence(A == 9);

#ifdef GRID_IS_D1
  bool ok = (dp1 == dp2 && dp1.domain1() == known_gr);
#else
  bool ok = (dp1 == dp2 && dp1.domain2() == known_gr);
#endif

  return ok;
}

// Product(cggs), add_grid_generator(g)
bool
test07() {
  Variable A(0);
  Variable B(1);

  const Grid_Generator_System gs(grid_point(A + B));

  Product dp(gs);

  Grid known_gr(2, EMPTY);
  known_gr.add_grid_generator(grid_point(A + B));

#ifdef GRID_IS_D1
  bool ok = (dp.domain1() == known_gr);
#else
  bool ok = (dp.domain2() == known_gr);
#endif

  return ok;
}

// Product(ggs)
bool
test08() {
  Variable A(0);
  Variable C(2);

  Grid_Generator_System gs(grid_point(A + 7*C));

  Product dp(gs);

  Grid known_gr(3, EMPTY);
  known_gr.add_grid_generator(grid_point(A + 7*C));

#ifdef GRID_IS_D1
  bool ok = (dp.domain1() == known_gr);
#else
  bool ok = (dp.domain2() == known_gr);
#endif

  return ok;
}

// Product(bounding_box)
bool
test09() {
  Variable B(1);

  Rational_Box box(2);
  box.raise_lower_bound(1, true, 2, 3);
  box.lower_upper_bound(1, true, 2, 3);

  Product dp(box);

  Grid known_gr(2);
  known_gr.add_congruence(3*B == 2);

  NNC_Polyhedron known_ph(2);
  known_ph.add_constraint(3*B == 2);

#ifdef GRID_IS_D1
  bool ok = (dp.domain1() == known_gr && dp.domain2() == known_ph);
#else
  bool ok = (dp.domain1() == known_ph && dp.domain2() == known_gr);
#endif

  return ok;
}

// FIXME: Waiting for covering box methods, details in
//        Direct_Product.defs.hh.
#if 0
// Product(covering_box)
bool
test10() {
  Variable B(1);

  Rational_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 2, 3);
  box.lower_upper_bound(1, true, 3, 3);

  Product dp(box, From_Covering_Box());

  Grid known_gr(2);
  known_gr.add_congruence(3*B %= 0);

  NNC_Polyhedron known_ph(2);

#ifdef GRID_IS_D1
  bool ok = (dp.domain1() == known_gr && dp.domain2() == known_ph);
#else
  bool ok = (dp.domain1() == known_ph && dp.domain2() == known_gr);
#endif

  return ok;
}
#endif

// operator=
bool
test11() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs(A + B <= 9);

  Product dp1(cs);
  dp1.add_congruence((A %= 9) / 19);

  Product dp2 = dp1;

  bool ok = (dp1 == dp2);

  return ok;
}

// space_dimension()
bool
test12() {
  Variable A(0);
  Variable E(4);

  Constraint_System cs(A + E < 9);

  Product dp(cs);

  bool ok = (dp.space_dimension() == 5);

  return ok;
}

// Copy constructor.
bool
test13() {
  Variable A(0);
  Variable B(2);

  Constraint_System cs(A - B == 0);

  Product dp1(cs);

  Product dp2(dp1);

  bool ok = (dp1 == dp2);

  return ok;
}


// affine_dimension()
bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_constraint(A - C <= 9);
  dp.add_constraint(A - C >= 9);
  dp.add_constraint(B == 2);

  bool ok = (dp.affine_dimension() == 1
#ifdef GRID_IS_D1
	     && dp.domain1().affine_dimension() == 2
	     && dp.domain2().affine_dimension() == 1
#else
	     && dp.domain1().affine_dimension() == 1
	     && dp.domain2().affine_dimension() == 2
#endif
	     );

  if (ok) {
    dp.add_constraint(C == 4);
    dp.add_generator(ray(B));
    dp.add_generator(point(A + C));
    ok &= (dp.affine_dimension() == 1
#ifdef GRID_IS_D1
	   && dp.domain1().affine_dimension() == 1
	   && dp.domain2().affine_dimension() == 2
#else
	   && dp.domain1().affine_dimension() == 2
	   && dp.domain2().affine_dimension() == 1
#endif
	   );
  }

  return ok;
}

// congruences()
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence(A %= 9);
  dp.add_congruence(B + C %= 3);

#ifdef GRID_IS_D1
  Congruence_System cgs;
  cgs.insert(B + C %= 0);
  cgs.insert(A %= 0);

  Grid known_gr(cgs);
#else
  Grid known_gr(3);
#endif

  Grid gr(dp.congruences());

  bool ok = gr == known_gr;

  return ok;
}

// minimized_congruences()
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence(B + C %= 3);
  dp.add_constraint(A >= 9);
  dp.add_constraint(A <= 9);

  Congruence_System cgs;
#ifdef GRID_IS_D1
  cgs.insert(B + C %= 3);
#endif
  cgs.insert((A + 0*C %= 9) / 0);

  Grid known_gr(cgs);

  Grid gr(dp.minimized_congruences());

  bool ok = gr == known_gr;

  return ok;
}

// constraints()
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((B + C %= 3) / 0);
  dp.add_constraint(A > 9);
  dp.add_constraint(A <= 11);

  NNC_Polyhedron ph(dp.space_dimension());
  ph.add_constraints(dp.constraints());

  NNC_Polyhedron known_ph(dp.space_dimension());
  known_ph.add_constraint(B + C == 3);
#ifndef GRID_IS_D1
  known_ph.add_constraint(A <= 11);
  known_ph.add_constraint(A > 9);
#endif

  bool ok = (ph == known_ph);

  return ok;
}

// minimized_constraints()
bool
test18() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((B + C %= 3) / 0);
  dp.add_constraint(A > 9);
  dp.add_constraint(A <= 11);

  NNC_Polyhedron ph(dp.space_dimension());
  ph.add_constraints(dp.constraints());

  NNC_Polyhedron known_ph(dp.space_dimension());
  known_ph.add_constraint(B + C == 3);
#ifndef GRID_IS_D1
  known_ph.add_constraint(A > 9);
  known_ph.add_constraint(A <= 11);
#endif

  bool ok = (ph == known_ph);

  return ok;
}

// generators()
bool
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((B + C %= 3) / 0);
  dp.add_constraint(A > 9);
  dp.add_constraint(A <= 11);

  NNC_Polyhedron ph(dp.space_dimension());
  ph.add_generators(dp.generators());

  NNC_Polyhedron known_ph(dp.space_dimension());
  known_ph.add_generator(closure_point(9*A + 3*B));
  known_ph.add_generator(point(11*A + 3*B));
  known_ph.add_generator(line(B + C));

  bool ok = (ph == known_ph);

  return ok;
}

// minimized_generators()
bool
test20() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((B + C %= 3) / 0);
  dp.add_constraint(A > 9);
  dp.add_constraint(A <= 11);

  NNC_Polyhedron ph(dp.space_dimension());
  ph.add_generators(dp.generators());

  NNC_Polyhedron known_ph(dp.space_dimension());
  known_ph.add_generator(line(B - C));
  known_ph.add_generator(closure_point(9*A + 3*B));
  known_ph.add_generator(point(10*A + 3*B));
  known_ph.add_generator(point(11*A + 3*B));

  // Maybe this should check that the generators are minimized.

  bool ok = (ph == known_ph);

  return ok;
}

// is_empty() where both domain objects have points.
bool
test21() {
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
test22() {
  Variable A(0);

  Product dp(3);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A %= 1) / 2);

  bool ok = dp.is_empty();

  return ok;
}

// is_empty() where both domain objects are empty.
bool
test23() {
  Variable A(0);

  Product dp(3);
  dp.add_constraint(A == 1);
  dp.add_constraint(A == 3);

  bool ok = dp.is_empty();

  return ok;
}

// is_universe() where both domain objects are empty.
bool
test24() {
  Product dp(3, EMPTY);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where one domain object is universe.
bool
test25() {
  Variable A(0);

  Product dp(3);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A %= 1) / 2);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where both domain objects are universe.
bool
test26() {
  Product dp(3);

  bool ok = dp.is_universe();

  return ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// open.
bool
test27() {
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
test28() {
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
test29() {
  Variable A(0);

  Product dp(3);
  dp.add_constraint(A < 3);
  dp.add_congruence((A %= 0) / 4);

  bool ok = !/* FIX */ dp.is_topologically_closed();

  return ok;
}

// is_disjoint_from(dp), due to the Polyhedra.
bool
test30() {
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
test31() {
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
test32() {
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
test33() {
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
test34() {
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
test35() {
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
test36() {
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
test37() {
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
test38() {
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
test39() {
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
test40() {
  Variable A(0);
  Variable B(1);

  Product dp(2, EMPTY);
  dp.add_grid_generator(grid_point());
  dp.add_grid_generator(grid_line(A + B));
  dp.add_generator(point());
  dp.add_generator(line(A));

  bool ok = dp.is_bounded();

  return ok;
}

// contains()
bool
test41() {
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
test42() {
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
test43() {
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
test44() {
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
test45() {
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
test46() {
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
test47() {
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
test48() {
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
test51() {
  Variable A(0);

  Rational_Box box(1);

  Product dp(1);
  dp.add_congruence((A %= 0) / 3);

  dp.get_covering_box(box);

  Rational_Box known_box(1);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 3, 1);

  bool ok = (box == known_box);

  return ok;
}

// get_covering_box(box), via polyhedron.
bool
test52() {
  Variable B(1);

  Rational_Box box(1);

  Product dp(2);
  dp.add_constraint(B < 3);
  dp.add_constraint(B > 0);

  dp.get_covering_box(box);

  Rational_Box known_box(1);
  known_box.raise_lower_bound(1, true, 0 /* FIX */, 1);
  known_box.lower_upper_bound(1, true, 0 /* FIX */, 1);

  bool ok = (box == known_box);

  return ok;
}

// get_covering_box(box), via intersection.
bool
test53() {
  Variable A(0);
  Variable B(1);

  Rational_Box box(2);

  Product dp(2);
  dp.add_constraint(B <= 0);
  dp.add_constraint(B >= 0);
  dp.add_congruence(A - B %= 0);

  dp.get_covering_box(box);

  Rational_Box known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);

  bool ok = !/* FIX */ (box == known_box);

  return ok;
}
#endif

// intersection_assign()
bool
test54() {
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
test55() {
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
test56() {
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
test57() {
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
test58() {
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
test59() {
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
test60() {
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
test61() {
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
test62() {
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

// map_space_dimensions()
bool
test63() {
  Variable A(0);
  Variable B(1);

  Product dp(2);
  dp.add_constraint(A >= 0);
  dp.add_congruence((A - B %= 0) / 2);

  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);

  dp.map_space_dimensions(function);

  Product known_dp(2);
  known_dp.add_constraint(B >= 0);
  known_dp.add_congruence((B - A %= 0) / 2);

  bool ok = (dp == known_dp);

  return ok;
}

// expand_space_dimension()
bool
test64() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Product dp(3);
  dp.add_congruence((A + B %= 2) / 7);
  dp.add_constraint(A >= 0);

  dp.expand_space_dimension(A, 1);

  Product known_dp(4);
  known_dp.add_congruence((A + B %= 2) / 7);
  known_dp.add_congruence((D + B %= 2) / 7);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(D >= 0);

  bool ok = (dp == known_dp);

  return ok;
}

// fold_space_dimensions()
bool
test65() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((A %= 2) / 7);
  dp.add_congruence((B %= 2) / 14);
  dp.add_congruence((C %= 2) / 21);
  dp.add_constraint(A <= 5);
  dp.add_constraint(B <= 10);
  dp.add_constraint(C <= 0);
  dp.add_constraint(C >= 0);

  Variables_Set to_fold;
  to_fold.insert(A);
  to_fold.insert(C);

  dp.fold_space_dimensions(to_fold, B);

  Product known_dp(1);
  known_dp.add_congruence((A %= 2) / 7);
  known_dp.add_constraint(A <= 10);

  bool ok = (dp == known_dp);

  return ok;
}

// FIXME: Wait for implementation.
#if 0
// affine_image()
bool
test66() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence((B %= 2) / 14);
  dp.add_constraint(A <= 5);
  dp.add_constraint(B <= 10);

  dp.affine_image(A, B + C);

  Product known_dp(3);
  known_dp.add_congruence((B %= 2) / 14);
  known_dp.add_constraint(A - B - C == 0);
  known_dp.add_constraint(B <= 10);

  bool ok = (dp == known_dp);

  return ok;
}

// affine_preimage()
bool
test67() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_constraint(A - B == 0);
  dp.add_congruence((A %= 0) / 3);

  dp.affine_preimage(A, B);

  Product known_dp(3);
  known_dp.add_congruence((B %= 0) / 3);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(v, e, relsym, d)
bool
test68() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((A + B %= 0) / 2);
  dp.add_constraint(B >= 0);
  dp.add_constraint(A - B >= 0);

  dp.generalized_affine_image(A, EQUAL, A + 2);

  Product known_dp(3);
  known_dp.add_congruence(A %= 0);
  known_dp.add_congruence((A + B %= 0) / 2);
  known_dp.add_constraint(B >= 0);
  known_dp.add_constraint(A - B >= 2);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(v, e, d, modulus)
bool
test69() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((A + B %= 0) / 2);
  dp.add_constraint(A > 3);

  dp.generalized_affine_image(B, A + 1, 2);

  Product known_dp(3);
  known_dp.add_congruence((A - 2*B %= -1) / 2);
  known_dp.add_congruence(A %= 0);
  known_dp.add_constraint(A > 3);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(v, e, relsym, d)
bool
test70() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_constraint(A >= 0);
  dp.add_constraint(A <= 4);
  dp.add_constraint(B <= 5);
  dp.add_constraint(A <= B);
  dp.add_congruence(A %= B);

  dp.generalized_affine_preimage(B, GREATER_OR_EQUAL, A+2);

  Product known_dp(3);
  known_dp.add_constraint(0 <= A);
  known_dp.add_constraint(A <= 3);
  known_dp.add_congruence(A %= B);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(v, e, d, modulus), add_generator(),
// add_generators(), add_grid_generators()
bool
test71() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((B %= 0) / 2);

  dp.generalized_affine_preimage(B, A + B, 1, 0);

  Grid_Generator_System ggs;
  ggs.insert(grid_point());
  ggs.insert(parameter(2*B));
  ggs.insert(parameter(A + B));
  ggs.insert(grid_line(C));

  Generator_System gs;
  gs.insert(point());
  gs.insert(line(A));
  gs.insert(line(B));
  gs.insert(line(C));

  Product known_dp(3, EMPTY);
  known_dp.add_grid_generators(ggs);
  known_dp.add_generators(gs);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(lhs, relsym, rhs)
bool
test72() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_congruence(A %= 0);
  dp.add_constraint(B >= 0);
  dp.add_constraint(A - B >= 1);

  dp.generalized_affine_image(Linear_Expression(2), LESS_OR_EQUAL, A + B);

  Product known_dp(3);
  known_dp.add_congruence(A %= 0);
  known_dp.add_constraint(B >= 0);
  known_dp.add_constraint(A - B >= 1);
  known_dp.add_constraint(2 <= A + B);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(lhs, rhs, modulus), add_congruences(cgs)
bool
test73() {
  Variable A(0);
  Variable B(1);

  Congruence_System cs;
  cs.insert((A %= 0) / 1);
  cs.insert((B %= 0) / 2);

  Product dp(2);
  dp.add_congruences(cs);
  dp.add_constraint(A <= 3);

  dp.generalized_affine_image(A + 2*B, A - B, 3);

  Product known_dp(2, EMPTY);
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(grid_point(B, 2));
  known_dp.add_grid_generator(grid_line(2*A - B));
  known_dp.add_generator(point(3*A));
  known_dp.add_generator(ray(-A));
  known_dp.add_generator(line(B));

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(lhs, relsym, rhs), add_constraints(cs)
bool
test74() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A <= 4);
  cs.insert(B <= 5);
  cs.insert(A <= B);

  Product dp(3);
  dp.add_constraints(cs);
  dp.add_congruence(A %= B);

  dp.generalized_affine_preimage(1*B, GREATER_OR_EQUAL, A+2);

  Product known_dp(3);
  known_dp.add_constraint(0 <= A);
  known_dp.add_constraint(A <= 3);
  known_dp.add_congruence(A %= B);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(lhs, rhs, modulus)
bool
test75() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_constraint(A - B == 0);

  dp.generalized_affine_preimage(A - B, 2*A - 2*B, 5);

  Product known_dp(3);
  known_dp.add_congruence((2*A - 2*B %= 0) / 5);
  known_dp.add_constraint(A - B >= 0);
  known_dp.add_constraint(A - B <= 0);

  bool ok = (dp == known_dp);

  return ok;
}
#endif

// time_elapse_assign(y)
bool
test76() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp1(3, EMPTY);
  dp1.add_grid_generator(grid_point());
  dp1.add_grid_generator(grid_point(A + 2*B - 3*C, 3));
  dp1.add_generator(point(3*A));
  dp1.add_generator(ray(A));
  dp1.add_generator(point(3*B));
  dp1.add_generator(ray(B));
  dp1.add_generator(line(C));

  Product dp2(3, EMPTY);
  dp2.add_grid_generator(grid_point(3*A - B + 4*C, 7));
  dp2.add_generator(point(A + B));

  dp1.time_elapse_assign(dp2);

  Product known_dp(3, EMPTY);
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(grid_point(A + 2*B - 3*C, 3));
  known_dp.add_grid_generator(grid_point(3*A - B + 4*C, 7));
  // Same Generators as dp1.
  known_dp.add_generator(point(3*A));
  known_dp.add_generator(ray(A));
  known_dp.add_generator(point(3*B));
  known_dp.add_generator(ray(B));
  known_dp.add_generator(line(C));

  bool ok = (dp1 == known_dp);

  return ok;
}

// topological_closure_assign
bool
test77() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3, EMPTY);
  dp.add_grid_generator(grid_point());
  dp.add_grid_generator(grid_point(A + 2*B - 3*C, 3));
  dp.add_generator(point(A));
#ifdef GRID_IS_D1
  dp.domain2().constraints();
#else
  dp.domain1().constraints();
#endif
  dp.add_generator(closure_point());
  dp.add_generator(ray(A));
  dp.add_generator(ray(B));

  dp.topological_closure_assign();

  Product known_dp(3, EMPTY);
  known_dp.add_generator(point());
  known_dp.add_generator(ray(A));
  known_dp.add_generator(ray(B));
  // Add Grid_Generators as to dp.
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(grid_point(A + 2*B - 3*C, 3));

  bool ok = (dp == known_dp);

  return ok;
}

// widening_assign
bool
test78() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp_prev(3, EMPTY);
  dp_prev.add_grid_generator(grid_point());
  dp_prev.add_grid_generator(grid_point(A, 3));
  dp_prev.add_grid_generator(grid_point(2*B));
  dp_prev.add_generator(point(A));
  dp_prev.add_generator(point(2*A));
  dp_prev.add_generator(point(2*A + B));

  Product dp(dp_prev);
  dp.add_grid_generator(parameter(A, 6));
  dp.add_generator(point(2*A + 2*B));
  dp.upper_bound_assign(dp_prev);

  dp.widening_assign(dp_prev);

  Product known_dp(3, EMPTY);
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(grid_point(2*B));
  known_dp.add_grid_generator(grid_line(A));
  known_dp.add_generator(point(2*A));
  known_dp.add_generator(ray(-A));
  known_dp.add_generator(ray(B));

  bool ok = (dp == known_dp);

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
  //DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST_NNC(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
#ifdef OPEN_PRODUCT
  DO_TEST(test16);
#endif
  DO_TEST_NNC(test17);
  DO_TEST_NNC(test18);
  DO_TEST_NNC(test19);
  DO_TEST_NNC(test20);
  DO_TEST(test21);
  DO_TEST(test22);
  DO_TEST(test23);
  DO_TEST(test24);
  DO_TEST(test25);
  DO_TEST(test26);
  DO_TEST_NNC(test27);
  DO_TEST(test28);
  DO_TEST_NNC(test29);
  DO_TEST_NNC(test30);
  DO_TEST(test31);
  DO_TEST_NNC(test32);
  DO_TEST_NNC(test33);
  DO_TEST(test34);
  DO_TEST(test35);
  DO_TEST(test36);
#ifdef OPEN_PRODUCT
  DO_TEST(test37);
#endif
  DO_TEST_NNC(test38);
  DO_TEST(test39);
#ifdef OPEN_PRODUCT
  DO_TEST(test40);
#endif
  DO_TEST_NNC(test41);
  DO_TEST_NNC(test42);
  DO_TEST_NNC(test43);
  DO_TEST_NNC(test44);
  DO_TEST_NNC(test45);
  DO_TEST_NNC(test46);
  DO_TEST(test47);
  DO_TEST_NNC(test48);
#if 0
  DO_TEST(test51);
  //DO_TEST(test52);
  DO_TEST(test53);
#endif
  DO_TEST(test54);
  DO_TEST(test55);
  DO_TEST(test56);
  DO_TEST_NNC(test57);
  DO_TEST(test58);
  DO_TEST(test59);
  DO_TEST_NNC(test60);
  DO_TEST(test61);
  DO_TEST(test62);
  DO_TEST(test63);
  DO_TEST(test64);
  DO_TEST(test65);
#if 0
  DO_TEST(test66);
  DO_TEST(test67);
  DO_TEST(test68);
  DO_TEST(test69);
  DO_TEST(test70);
  DO_TEST(test71);
  DO_TEST(test72);
  DO_TEST(test73);
  DO_TEST(test74);
  DO_TEST(test75);
#endif
  DO_TEST_F8(test76);
  DO_TEST_NNC(test77);
  DO_TEST(test78);
END_MAIN
