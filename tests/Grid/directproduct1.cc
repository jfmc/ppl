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
  box.add_constraint(3*B == 2);

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
  box.add_constraint(A == 0);
  box.add_constraint(3*B >= 2);
  box.add_constraint(3*B <= 3);

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
END_MAIN
