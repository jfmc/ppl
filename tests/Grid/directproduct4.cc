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

// map_space_dimensions()
bool
test01() {
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
test02() {
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
test03() {
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
test04() {
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
test05() {
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
test06() {
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
test07() {
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
test08() {
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
test09() {
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
test10() {
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
test11() {
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
test12() {
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
test13() {
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
test14() {
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
test15() {
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
test16() {
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
#if 0
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
#endif
  DO_TEST_F8(test14);
  DO_TEST_NNC(test15);
  DO_TEST(test16);
END_MAIN
