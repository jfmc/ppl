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

// #define PH_IS_NNC
// #define PH_IS_FIRST

#ifdef PH_IS_NNC
typedef NNC_Polyhedron Poly;
#else
typedef C_Polyhedron Poly;
#endif

#ifdef PH_IS_FIRST
typedef Domain_Product<Poly, Grid>::Direct_Product Product;
#else
typedef Domain_Product<Grid, Poly>::Direct_Product Product;
#endif

namespace {

// affine_image()
bool
test01() {
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

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// affine_preimage()
bool
test02() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_constraint(A - B == 0);
  dp.add_congruence((A %= 0) / 3);

  dp.affine_preimage(A, B);

  Product known_dp(3);
  known_dp.add_congruence((B %= 0) / 3);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// generalized_affine_image(v, relsym, e, d)
bool
test03() {
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

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

#if 0
// generalized_affine_image(v, EQUAL, e, d, denom, modulus)
bool
test04() {
  Variable A(0);
  Variable B(1);

  Product dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((A + B %= 0) / 2);
  dp.add_constraint(A >= 3);

  dp.generalized_affine_image(B, EQUAL, A + 1, 2, 1);

  Product known_dp(3);
  known_dp.add_congruence((A + 2*B %= -1) / 2);
  known_dp.add_constraint(A - 2*B >= -1);
  known_dp.add_constraint(A - 2*B <= -1);
  known_dp.add_congruence(A %= 0);
  known_dp.add_constraint(A >= 3);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}
#endif

// generalized_affine_preimage(v, relsym, e, d)
bool
test05() {
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

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// generalized_affine_preimage(v, EQUAL, e, d),
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((B %= 0) / 2);

  dp.generalized_affine_preimage(B, EQUAL, A + B, 1);

  Product known_dp(3);
  known_dp.add_congruence((A + B %= 0) / 2);
  known_dp.add_congruence(A %= 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// generalized_affine_image(lhs, relsym, rhs)
bool
test07() {
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

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

#if 0
// generalized_affine_image(lhs, EQUAL, rhs, modulus),
// add_congruences(cgs)
bool
test08() {
  Variable A(0);
  Variable B(1);

  Congruence_System cs;
  cs.insert((A %= 0) / 1);
  cs.insert((B %= 0) / 2);

  Product dp(2);
  dp.add_congruences(cs);
  dp.add_constraint(A <= 3);

  dp.generalized_affine_image(A + 2*B, EQUAL, A - B, 3);

  Product known_dp(2);
  known_dp.add_congruence((A + 2*B %= 0) / 1);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}
#endif

// generalized_affine_preimage(lhs, relsym, rhs), add_constraints(cs)
bool
test09() {
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

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

#if 0
// generalized_affine_preimage(lhs, EQUAL, rhs, modulus)
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Product dp(3);
  dp.add_constraint(A - B == 0);

  dp.generalized_affine_preimage(A - B, EQUAL, 2*A - 2*B, 5);

  Product known_dp(3);
  known_dp.add_congruence((2*A - 2*B %= 0) / 5);
  known_dp.add_constraint(A - B >= 0);
  known_dp.add_constraint(A - B <= 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}
#endif

// add_constraints
bool
test11() {

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B == 0);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_constraints(cs);

  Product known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(B == 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// add_recycled_constraints
bool
test12() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A + B <= 0);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_recycled_constraints(cs);

  Product known_dp(2);
  known_dp.add_constraint(A + B <= 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// add_constraints_and_minimize
bool
test13() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A + B == 0);

  Product dp(2);

  dp.add_constraints_and_minimize(cs);

  Product known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(A + B == 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// add_recycled_constraints_and_minimize
bool
test14() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(B >= 0);
  cs.insert(A - B == 0);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_recycled_constraints_and_minimize(cs);

  Product known_dp(2);
  known_dp.add_constraint(B >= 0);
  known_dp.add_constraint(A - B == 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}
// add_congruences
bool
test15() {

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);
  cgs.insert((B == 0) / 2);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_congruences(cgs);

  Product known_dp(2);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_congruence((B == 0) / 2);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

#if (0)
// add_recycled_congruences
bool
test16() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A + B %= 0) / 2);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_recycled_congruences(cgs);

  Product known_dp(2);
  known_dp.add_congruence((A + B %= 0) / 2);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// add_congruences_and_minimize
bool
test17() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);
  cgs.insert(A + B == 0);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_congruences_and_minimize(cgs);

  Product known_dp(2);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_congruence(A + B == 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}

// add_recycled_congruences_and_minimize
bool
test18() {
  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((B %= 0) / 2);
  cgs.insert(A - B == 0);

  Product dp(2);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  dp.add_recycled_congruences_and_minimize(cgs);

  Product known_dp(2);
  known_dp.add_congruence((B %= 0) / 2);
  known_dp.add_congruence(A - B == 0);

  bool ok = (dp == known_dp);

  print_constraints(dp, "*** dp constraints ***");
  print_congruences(dp, "*** dp congruences ***");

  return ok;
}
#endif

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
//  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
//  DO_TEST(test08);
  DO_TEST(test09);
//  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
//  DO_TEST(test16);
//  DO_TEST(test17);
//  DO_TEST(test18);
END_MAIN
