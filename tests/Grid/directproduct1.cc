/* Test Direct_Product<NNC_Polyhedron, Grid>.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

// Direct_Product()
bool
test01() {
  Direct_Product<NNC_Polyhedron, Grid> dp1;

  Direct_Product<NNC_Polyhedron, Grid> dp2(0, UNIVERSE);

  bool ok = (dp1 == dp2);

  return ok;
}

// Direct_Product(dims,type)
bool
test02() {
  Direct_Product<NNC_Polyhedron, Grid> dp1(3);

  Direct_Product<NNC_Polyhedron, Grid> dp2(3, EMPTY);

  bool ok = (dp1 != dp2);

  return ok;
}

// Direct_Product(ccgs), add_congruence(cg)
bool
test03() {
  Variable A(0);

  const Congruence_System cgs((A %= 0) / 4);

  Direct_Product<NNC_Polyhedron, Grid> dp1(cgs);

  Direct_Product<NNC_Polyhedron, Grid> dp2(1);
  dp2.add_congruence((A %= 0) / 4);

  bool ok = (dp1 == dp2);

  return ok;
}

// Direct_Product(cgs), domain1(), domain2
bool
test04() {
  Variable A(0);

  Congruence_System cgs((A %= 0) / 4);

  Direct_Product<NNC_Polyhedron, Grid> dp(cgs);

  NNC_Polyhedron known_ph(1);

  Grid known_gr(1);
  known_gr.add_congruence((A %= 0) / 4);

  bool ok = (dp.domain1() == known_ph
	     && dp.domain2() == known_gr);

  nout << "*** Direct_Product<NNC_Polyhedron, Grid> dp(cgs) ***"
       << endl << dp << endl;

  return ok;
}

// Direct_Product(ccs), add_constraint(cc)
bool
test05() {
  Variable A(0);

  const Constraint_System cs(A >= 0);

  Direct_Product<NNC_Polyhedron, Grid> dp1(cs);

  Direct_Product<NNC_Polyhedron, Grid> dp2(1);
  dp2.add_constraint(static_cast<const Constraint>(A >= 0));

  bool ok = (dp1 == dp2);

  return ok;
}

// Direct_Product(cs)
bool
test06() {
  Variable A(0);

  Constraint_System cs(A == 9);

  Direct_Product<NNC_Polyhedron, Grid> dp1(cs);

  Direct_Product<NNC_Polyhedron, Grid> dp2(1);
  dp2.add_constraint(A == 9);

  Grid known_gr(1);
  known_gr.add_congruence(A == 9);

  bool ok = (dp1 == dp2 && dp1.domain2() == known_gr);

  return ok;
}

// Direct_Product(cggs)
bool
test07() {
  Variable A(0);
  Variable B(1);

  const Grid_Generator_System gs(grid_point(A + B));

  Direct_Product<NNC_Polyhedron, Grid> dp(gs);

  Grid known_gr(2, EMPTY);
  known_gr.add_grid_generator(grid_point(A + B));

  bool ok = (dp.domain2() == known_gr);

  return ok;
}

// Direct_Product(ggs)
bool
test08() {
  Variable A(0);
  Variable C(2);

  Grid_Generator_System gs(grid_point(A + 7*C));

  Direct_Product<NNC_Polyhedron, Grid> dp(gs);

  Grid known_gr(3, EMPTY);
  known_gr.add_grid_generator(grid_point(A + 7*C));

  bool ok = (dp.domain2() == known_gr);

  return ok;
}

// Direct_Product(bounding_box)
bool
test09() {
  Variable A(0);
  Variable B(1);

  Bounding_Box box(2);
  box.raise_lower_bound(1, true, 2, 3);
  box.lower_upper_bound(1, true, 2, 3);

  Direct_Product<NNC_Polyhedron, Grid> dp1(box, From_Bounding_Box());

  Grid known_gr(2);
  known_gr.add_congruence(3*B == 2);

  NNC_Polyhedron known_ph(2);
  known_ph.add_constraint(3*B == 2);

  bool ok = (dp1.domain1() == known_ph && dp1.domain2() == known_gr);

  return ok;
}

// operator=
bool
test10() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs(A + B <= 9);

  Direct_Product<NNC_Polyhedron, Grid> dp1(cs);
  dp1.add_congruence((A %= 9) / 19);

  Direct_Product<NNC_Polyhedron, Grid> dp2 = dp1;

  bool ok = (dp1 == dp2);

  return ok;
}

// space_dimension
bool
test11() {
  Variable A(0);
  Variable E(4);

  Constraint_System cs(A + E < 9);

  Direct_Product<NNC_Polyhedron, Grid> dp(cs);

  bool ok = (dp.space_dimension() == 5);

  return ok;
}

// Copy constructor
bool
test12() {
  Variable A(0);
  Variable B(2);

  Constraint_System cs(A - B == 0);

  Direct_Product<NNC_Polyhedron, Grid> dp1(cs);

  Direct_Product<NNC_Polyhedron, Grid> dp2(dp1);

  bool ok = (dp1 == dp2);

  return ok;
}

// affine_dimension
bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A - C <= 9);
  dp.add_constraint(A - C >= 9);
  dp.add_constraint(B == 2);

  bool ok = (dp.affine_dimension() == 1
	     && dp.domain1().affine_dimension() == 1
	     && dp.domain2().affine_dimension() == 2);

  return ok;
}

#if 0
// congruences()
bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(A %= 9);
  dp.add_congruence(B + C %= 3);

  Congruence_System cgs;
  cgs.insert(A %= 9);
  cgs.insert(B + C %= 3);

  bool ok = (dp.congruences() == cgs);

  return ok;
}
#endif

// is_empty() where both domain objects have points.
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(A %= 9);
  dp.add_congruence(B + C %= 3);

  bool ok = !dp.is_empty();

  return ok;
}

// is_empty() where one domain object is empty.
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A %= 1) / 2);

  bool ok = dp.is_empty();

  return ok;
}

// is_empty() where both domain objects are empty.
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A == 1);
  dp.add_constraint(A == 3);

  bool ok = dp.is_empty();

  return ok;
}

// reduce()
bool
test18() {
  Variable A(0);

  Direct_Product<NNC_Polyhedron, Grid> dp(1);
  dp.add_constraint(A > 7);
  dp.add_constraint(A < 7);

  bool ok = dp.domain2().is_universe();

  dp.reduce();

  ok &= dp.domain2().is_empty();

  return ok;
}

// is_universe() where both domain objects are empty.
bool
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3, EMPTY);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where one domain object is universe.
bool
test20() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A %= 1) / 2);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where both domain objects are universe.
bool
test21() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);

  bool ok = dp.is_universe();

  return ok;
}

// upper_bound_assign(dp2)
bool
test22() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs(A == 9);

  Direct_Product<NNC_Polyhedron, Grid> dp1(cs);

  Direct_Product<NNC_Polyhedron, Grid> dp2(1);
  dp2.add_constraint(A == 19);

  dp1.upper_bound_assign(dp2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(1);
  known_dp.add_constraint(A >= 9);
  known_dp.add_constraint(A <= 19);
  known_dp.add_congruence((A %= 9) / 10);

  bool ok = (dp1 == known_dp);

  return ok;
}

// upper_bound_assign_if_exact()
bool
test23() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp1(3);
  dp1.add_constraint(B == 0);

  Direct_Product<NNC_Polyhedron, Grid> dp2(3);
  dp2.add_constraint(B == 0);
  dp2.add_constraint(A == 12);
  dp2.add_constraint(A == 16);

  dp1.upper_bound_assign_if_exact(dp2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_constraint(B == 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// intersection_assign()
bool
test24() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp1(3);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  Direct_Product<NNC_Polyhedron, Grid> dp2(3);
  dp2.add_constraint(A <= 0);
  dp2.add_congruence((A %= 0) / 7);

  dp1.intersection_assign(dp2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence((A %= 0) / 14);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(A <= 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

#if 0
// reduce()
bool
testr0() {
  Variable A(0);

  Direct_Product<NNC_Polyhedron, Grid> dp(1);

  dp.add_congruence((A %= 0) / 2);
  dp.add_constraint(A >= 7);

  bool ok = dp.reduce();

  Direct_Product<NNC_Polyhedron, Grid> known_dp(1);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(A >= 8);

  ok &= (dp == known_dp);

  return ok;
}

// reduce() where there is a ph divisor > 1
bool
testr2() {
  Variable A(0);

  Direct_Product<NNC_Polyhedron, Grid> dp(1);

  dp.add_congruence((A %= 0) / 3);
  dp.add_constraint(3*A >= 2);

  Direct_Product<NNC_Polyhedron, Grid> original_dp = dp;

  bool ok = dp.reduce();

  if (dp.domain1().strictly_contains(original_dp.domain1())) {
    ok = false;
    nout << "Polyhedron was reduced." << endl;
  }
  else
    nout << "Polyhedron stayed the same." << endl;

  if (dp.domain2().strictly_contains(original_dp.domain2())) {
    ok = false;
    nout << "Grid was reduced." << endl;
  }
  else
    nout << "Grid stayed the same." << endl;

  return ok;
}

// reduce() where there is a ph divisor > 1
bool
testr1() {
  Variable A(0);

  Direct_Product<NNC_Polyhedron, Grid> dp(1);

  dp.add_congruence((A %= 0) / 2);
  dp.add_constraint(3*A >= 2);

  bool ok = dp.reduce();

  Direct_Product<NNC_Polyhedron, Grid> known_dp(1);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(A >= 2);

  ok &= (dp == known_dp);

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
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  //DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST(test18);

  DO_TEST(test19);
  DO_TEST(test20);
  DO_TEST(test21);
  DO_TEST(test22);
  DO_TEST(test23);
  DO_TEST(test24);
END_MAIN
