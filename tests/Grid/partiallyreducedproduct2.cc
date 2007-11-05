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

#define PH_IS_NNC
// #define PH_IS_FIRST

#ifdef PH_IS_NNC
typedef NNC_Polyhedron Poly;
#else
typedef C_Polyhedron Poly;
#endif

#ifdef PH_IS_FIRST
typedef Domain_Product<Poly, Grid>::Smash_Product SProduct;
typedef Domain_Product<Poly, Grid>::Constraints_Product CProduct;
#else
typedef Domain_Product<Grid, Poly>::Smash_Product SProduct;
typedef Domain_Product<Grid, Poly>::Constraints_Product CProduct;
#endif

namespace {

// is_empty() where both domain objects have points.
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  SProduct sp(3);
  sp.add_congruence(A %= 9);
  sp.add_congruence(B + C %= 3);

  bool smash_ok = !sp.is_empty();

  CProduct cp(3);
  cp.add_congruence(A %= 9);
  cp.add_congruence(B + C %= 3);

  bool cons_ok = !cp.is_empty();

  print_congruences(sp, "*** sp congruences ***");
  print_constraints(sp, "*** sp constraints ***");
  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return smash_ok && cons_ok;
}

// is_empty() where one domain object is empty.
bool
test02() {
  Variable A(0);

  SProduct sp(3);
  sp.add_congruence((A %= 0) / 2);
  sp.add_congruence((A %= 1) / 2);

  bool smash_ok = sp.is_empty();

  CProduct cp(3);
  cp.add_congruence((A %= 0) / 2);
  cp.add_congruence((A %= 1) / 2);

  bool cons_ok = cp.is_empty();

  print_congruences(sp, "*** sp congruences ***");
  print_constraints(sp, "*** sp constraints ***");
  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return smash_ok && cons_ok;
}

// is_empty() where both domain objects are empty.
bool
test03() {
  Variable A(0);

  SProduct sp(3);
  sp.add_constraint(A == 1);
  sp.add_constraint(A == 3);

  bool smash_ok = sp.is_empty();

  CProduct cp(3);
  cp.add_constraint(A == 1);
  cp.add_constraint(A == 3);

  bool cons_ok = cp.is_empty();

  print_congruences(sp, "*** sp congruences ***");
  print_constraints(sp, "*** sp constraints ***");
  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return smash_ok && cons_ok;
}

// is_universe() where both domain objects are empty.
bool
test04() {
  CProduct cp(3, EMPTY);

  bool ok = !cp.is_universe();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_universe() where one domain object is universe.
bool
test05() {
  Variable A(0);

  SProduct sp(3);
  sp.add_congruence((A %= 0) / 2);
  sp.add_congruence((A %= 1) / 2);

  bool ok = !sp.is_universe();

  print_congruences(sp, "*** sp congruences ***");
  print_constraints(sp, "*** sp constraints ***");

  return ok;
}

// is_universe() where both domain objects are universe.
bool
test06() {
  CProduct cp(3);

  bool ok = cp.is_universe();

  return ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// open.
bool
test07() {
  Variable A(0);

  SProduct sp(3);
  bool smash_ok;
  CProduct cp(3);
  bool cons_ok;
#ifdef PH_IS_NNC
  sp.add_constraint(A < 3);
  sp.add_congruence((A %= 0) / 3);
  cp.add_constraint(A < 3);
  cp.add_congruence((A %= 0) / 3);

  smash_ok = !sp.is_topologically_closed();
  cons_ok = !cp.is_topologically_closed();
#else
  sp.add_constraint(A <= 3);
  sp.add_congruence((A %= 0) / 3);
  cp.add_constraint(A <= 3);
  cp.add_congruence((A %= 0) / 3);

  smash_ok = sp.is_topologically_closed();
  cons_ok = cp.is_topologically_closed();
#endif

  print_congruences(sp, "*** sp congruences ***");
  print_constraints(sp, "*** sp constraints ***");
  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return smash_ok && cons_ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// closed.
bool
test08() {
  Variable A(0);

  SProduct sp(3);
  sp.add_constraint(A <= 3);
  sp.add_congruence((A %= 0) / 3);

  bool smash_ok = sp.is_topologically_closed();

  CProduct cp(3);
  cp.add_constraint(A <= 3);
  cp.add_congruence((A %= 0) / 3);

  bool cons_ok = cp.is_topologically_closed();

  print_congruences(sp, "*** sp congruences ***");
  print_constraints(sp, "*** sp constraints ***");
  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return smash_ok && cons_ok;
}

// is_topologically_closed() where the Polyhedron is topologically
// open and the intersection is closed.
bool
test09() {
  Variable A(0);

  bool ok;

  CProduct cp(3);
#ifdef PH_IS_NNC
  cp.add_constraint(A < 3);
  cp.add_congruence((A %= 0) / 4);

  ok = !cp.is_topologically_closed();
#else
  cp.add_constraint(A <= 3);
  cp.add_congruence((A %= 0) / 4);

  ok = cp.is_topologically_closed();
#endif

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_disjoint_from(), due to the Polyhedra.
bool
test10() {
  Variable B(1);

  bool ok;

  SProduct sp1(12);
#ifdef PH_IS_NNC
  sp1.add_constraint(B < 3);

  SProduct sp2(12);
  sp2.add_constraint(B > 3);

  ok = sp1.is_disjoint_from(sp2);
#else
  sp1.add_constraint(B <= 3);

  SProduct sp2(12);
  sp2.add_constraint(B >= 4);

  ok = sp1.is_disjoint_from(sp2);
#endif

  print_congruences(sp1, "*** sp1 congruences ***");
  print_constraints(sp1, "*** sp1 constraints ***");
  print_congruences(sp2, "*** sp2 congruences ***");
  print_constraints(sp2, "*** sp2 constraints ***");

  return ok;
}

// is_disjoint_from(), due to the Grids.
bool
test11() {
  Variable A(0);

  CProduct cp1(3);
  cp1.add_congruence((A %= 0) / 7);

  CProduct cp2(3);
  cp2.add_congruence((A %= 1) / 7);

  bool ok = cp1.is_disjoint_from(cp2);

  print_congruences(cp1, "*** cp1 congruences ***");
  print_constraints(cp1, "*** cp1 constraints ***");
  print_congruences(cp2, "*** cp2 congruences ***");
  print_constraints(cp2, "*** cp2 constraints ***");

  return ok;
}

// is_disjoint_from(), due to either.
bool
test12() {
  Variable A(0);

  bool ok;

  CProduct cp1(3);
#ifdef PH_IS_NNC
  cp1.add_constraint(A < 3);
  cp1.add_congruence((A %= 0) / 7);

  CProduct cp2(3);
  cp2.add_constraint(A > 3);
  cp2.add_congruence((A %= 1) / 7);

  ok = cp1.is_disjoint_from(cp2);
#else
  cp1.add_constraint(A <= 2);
  cp1.add_congruence((A %= 0) / 7);

  CProduct cp2(3);
  cp2.add_constraint(A >= 4);
  cp2.add_congruence((A %= 1) / 7);

  ok = cp1.is_disjoint_from(cp2);
#endif

  print_congruences(cp1, "*** cp1 congruences ***");
  print_constraints(cp1, "*** cp1 constraints ***");
  print_congruences(cp2, "*** cp2 congruences ***");
  print_constraints(cp2, "*** cp2 constraints ***");

  return ok;
}

// is_disjoint_from(cp), due to both.
bool
test13() {
  Variable A(0);

  bool ok;

  CProduct cp1(3);
#ifdef PH_IS_NNC
  cp1.add_constraint(A < 6);
  cp1.add_congruence((A %= 1) / 7);

  CProduct cp2(3);
  cp2.add_constraint(A > 3);
  cp2.add_congruence((A %= 1) / 14);

  ok = !cp1.is_disjoint_from(cp2);
#else
  cp1.add_constraint(A <= 6);
  cp1.add_congruence((A %= 1) / 7);

  CProduct cp2(3);
  cp2.add_constraint(A >= 3);
  cp2.add_congruence((A %= 1) / 14);

  ok = !cp1.is_disjoint_from(cp2);
#endif

  print_congruences(cp1, "*** cp1 congruences ***");
  print_constraints(cp1, "*** cp1 constraints ***");
  print_congruences(cp2, "*** cp2 congruences ***");
  print_constraints(cp2, "*** cp2 constraints ***");

  return ok;
}

// is_disjoint_from(cp), due to the intersection of the entire direct
// products (i.e. the cp1 and cp2 polyhedron components intersect, as
// do the grid components).
bool
test14() {
  Variable A(0);
  Variable B(1);

  CProduct cp1(2);
  cp1.add_constraint(A <= 4);
  cp1.add_constraint(A >= 0);
  cp1.add_constraint(A - B <= 0);
  cp1.add_constraint(A - B >= 2);
  cp1.add_congruence((A %= 0) / 2);
  cp1.add_congruence((A %= 0) / 4);

  CProduct cp2(2);
  cp2.add_constraint(A <= 4);
  cp2.add_constraint(A <= 0);
  cp2.add_constraint(A + B >= 4);
  cp2.add_constraint(A + B <= 6);
  // Same grid as cp1.
  cp2.add_congruence((A %= 0) / 2);
  cp2.add_congruence((A %= 0) / 4);

  bool ok = cp1.is_disjoint_from(cp2);

  print_congruences(cp1, "*** cp1 congruences ***");
  print_constraints(cp1, "*** cp1 constraints ***");
  print_congruences(cp2, "*** cp2 congruences ***");
  print_constraints(cp2, "*** cp2 constraints ***");

  return ok;
}

// is_discrete(), due to grid.
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  CProduct cp(3);
  cp.add_constraint(A == 1);
  cp.add_constraint(B == 2);
  cp.add_constraint(C <= 3);
  cp.add_congruence((C %= 0) / 3);

  bool ok = cp.is_discrete();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_discrete(), due to polyhedron.
bool
test16() {
  Variable A(0);
  Variable B(1);

  CProduct cp(2);
  cp.add_constraint(A <= 3);
  cp.add_constraint(A >= 3);
  cp.add_constraint(B == 0);

  bool ok = cp.is_discrete();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_discrete(), due to intersection.
bool
test17() {
  Variable A(0);
  Variable B(1);

  CProduct cp(3, EMPTY);
  cp.add_grid_generator(grid_point());
  cp.add_grid_generator(grid_line(A + B));
  cp.add_generator(point());
  cp.add_generator(line(A));

  bool ok = !cp.is_discrete();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_bounded(), due to polyhedron.
bool
test18() {
  Variable A(0);
  Variable B(1);

  bool ok;

  CProduct cp(2);
#ifdef PH_IS_NNC
  cp.add_constraint(A > 1);
  cp.add_constraint(A < 4);
  cp.add_constraint(B > 1);
  cp.add_constraint(B < 4);
  cp.add_congruence((A %= 1) / 3);

  ok = cp.is_bounded();
#else
  cp.add_constraint(A >= 1);
  cp.add_constraint(A <= 4);
  cp.add_constraint(B >= 1);
  cp.add_constraint(B <= 4);
  cp.add_congruence((A %= 1) / 3);
#endif

  ok = cp.is_bounded();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_bounded(), due to grid.
bool
test19() {
  Variable A(0);

  CProduct cp(2, EMPTY);
  cp.add_generator(point());
  cp.add_generator(ray(-A));
  cp.add_grid_generator(grid_point());

  bool ok = cp.is_bounded();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

// is_bounded(), due to intersection.
bool
test20() {
  Variable A(0);
  Variable B(1);

  CProduct cp(2, EMPTY);
  cp.add_grid_generator(grid_point());
  cp.add_grid_generator(grid_line(A + B));
  cp.add_generator(point());
  cp.add_generator(line(A));

  bool ok = !cp.is_bounded();

  print_congruences(cp, "*** cp congruences ***");
  print_constraints(cp, "*** cp constraints ***");

  return ok;
}

} // namecpace

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
  DO_TEST(test18);
  DO_TEST(test19);
  DO_TEST(test20);
END_MAIN
