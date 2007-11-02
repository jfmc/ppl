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

// #define PH_IS_NNC

#ifdef PH_IS_NNC
// FIXME when Box.defs.hh no longer includes the old Direct_Product.
typedef Domain_Product<Grid, NNC_Polyhedron>::Direct_Product
No_Reduction_Product;
typedef Domain_Product<Grid, NNC_Polyhedron>::Smash_Product
Smash_Product;
typedef Domain_Product<Grid, NNC_Polyhedron>::Constraints_Product
Constraints_Product;
#else
// FIXME when Box.defs.hh no longer includes the old Direct_Product.
typedef Domain_Product<Grid, C_Polyhedron>::Direct_Product
No_Reduction_Product;
typedef Domain_Product<Grid, C_Polyhedron>::Smash_Product
Smash_Product;
typedef Domain_Product<Grid, C_Polyhedron>::Constraints_Product
Constraints_Product;
#endif

namespace {

// Product()
bool
test01() {
  Constraints_Product dp1;
  Constraints_Product dp2(0, UNIVERSE);
  Smash_Product dp3;
  Smash_Product dp4(0, UNIVERSE);
  No_Reduction_Product dp5;
  No_Reduction_Product dp6(0, UNIVERSE);

  bool ok = (dp1 == dp2 && dp3 == dp4 && dp5 == dp6);

  return ok;
}

// Product(dims, type)
bool
test02() {
  Constraints_Product dp1(3);
  Constraints_Product dp2(3, EMPTY);
  Smash_Product dp3(3);
  Smash_Product dp4(3, EMPTY);
  No_Reduction_Product dp5(3);
  No_Reduction_Product dp6(3, EMPTY);

  bool ok = (dp1 != dp2 && dp3 != dp4 && dp5 != dp6);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// Product(ccgs), add_congruence(cg)
bool
test03() {
  Variable A(0);

  const Congruence_System cgs((A %= 0) / 4);

  Constraints_Product dp1(cgs);
  Constraints_Product dp2(1);
  dp2.add_congruence((A %= 0) / 4);
  Smash_Product dp3(cgs);
  Smash_Product dp4(1);
  dp4.add_congruence((A %= 0) / 4);
  No_Reduction_Product dp5(cgs);
  No_Reduction_Product dp6(1);
  dp6.add_congruence((A %= 0) / 4);

  bool ok =  (dp1 == dp2 && dp3 == dp4 && dp5 == dp6);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// Product(cgs), domain1(), domain2()
bool
test04() {
  Variable A(0);

  const Congruence_System cgs((A %= 0) / 4);

  Constraints_Product dp1(cgs);
  Constraints_Product dp2(1);
  dp2.add_congruence((A %= 0) / 4);
  Smash_Product dp3(cgs);
  Smash_Product dp4(1);
  dp4.add_congruence((A %= 0) / 4);
  No_Reduction_Product dp5(cgs);
  No_Reduction_Product dp6(1);
  dp6.add_congruence((A %= 0) / 4);

  bool ok =  (dp1 == dp2 && dp3 == dp4 && dp5 == dp6);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// Product(ccs), add_constraint(cc)
bool
test05() {
  Variable A(0);

  const Constraint_System cs(A >= 0);

  Constraints_Product dp1(cs);
  Constraints_Product dp2(1);
  dp2.add_constraint(static_cast<const Constraint>(A >= 0));
  Smash_Product dp3(cs);
  Smash_Product dp4(1);
  dp4.add_constraint(static_cast<const Constraint>(A >= 0));
  No_Reduction_Product dp5(cs);
  No_Reduction_Product dp6(1);
  dp6.add_constraint(static_cast<const Constraint>(A >= 0));

  bool ok =  (dp1 == dp2 && dp3 == dp4 && dp5 == dp6);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// Product(cs)
bool
test06() {
  Variable A(0);

  Constraint_System cs(A == 9);

  Constraints_Product dp1(cs);
  Constraints_Product dp2(1);
  dp2.add_constraint(A == 9);
  Smash_Product dp3(cs);
  Smash_Product dp4(1);
  dp4.add_constraint(A == 9);
  No_Reduction_Product dp5(cs);
  No_Reduction_Product dp6(1);
  dp6.add_constraint(A == 9);

  Grid known_gr(1);
  known_gr.add_congruence(A == 9);

  bool ok =  (dp1 == dp2 && dp1.domain1() == known_gr)
               && (dp3 == dp4 && dp3.domain1() == known_gr)
               && (dp5 == dp6 && dp5.domain1() == known_gr);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// Product(cggs), add_grid_generator(g)
bool
test07() {
  Variable A(0);
  Variable B(1);

  const Grid_Generator_System gs(grid_point(A + B));

  Constraints_Product dp(gs);

  Grid known_gr(2, EMPTY);
  known_gr.add_grid_generator(grid_point(A + B));

  bool ok = (dp.domain1() == known_gr);

  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");

  return ok;
}

// Product(ggs)
bool
test08() {
  Variable A(0);
  Variable C(2);

  Grid_Generator_System gs(grid_point(A + 7*C));

  Smash_Product dp(gs);

  Grid known_gr(3, EMPTY);
  known_gr.add_grid_generator(grid_point(A + 7*C));

  bool ok = (dp.domain1() == known_gr);

  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");

  return ok;
}

// Product(bounding_box)
bool
test09() {
  Variable B(1);

  Rational_Box box(2);
  box.add_constraint(3*B == 2);

  Constraints_Product dp(box);

  Grid known_gr(2);
  known_gr.add_congruence(3*B == 2);

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(2);
#else
  C_Polyhedron known_ph(2);
#endif
  known_ph.add_constraint(3*B == 2);

  bool ok = (dp.domain1() == known_gr && dp.domain2() == known_ph);

  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");

  return ok;
}

// FIXME: Waiting for covering box methods, details in
//        Partially_Reduced_Product.defs.hh.
#if 0
// Product(covering_box)
bool
test10() {
  Variable B(1);

  Rational_Box box(2);
  box.add_constraint(A == 0);
  box.add_constraint(3*B >= 2);
  box.add_constraint(3*B <= 3);

  Constraints_Product dp(box, From_Covering_Box());

  Grid known_gr(2);
  known_gr.add_congruence(3*B %= 0);

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(2);
#else
  C_Polyhedron known_ph(2);
#endif

  bool ok = (dp.domain1() == known_gr && dp.domain2() == known_ph);

  print_congruences(dp.domain1(),
    "*** dp.domain1() ***");
  print_constraints(dp.domain2(),
     "*** dp.domain2() ***");

  return ok;
}
#endif

// operator=
bool
test11() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs(A + B <= 9);

  Constraints_Product dp1(cs);
  dp1.add_congruence((A %= 9) / 19);
  Constraints_Product dp2 = dp1;
  Smash_Product dp3(cs);
  dp3.add_congruence((A %= 9) / 19);
  Smash_Product dp4 = dp3;
  No_Reduction_Product dp5(cs);
  dp5.add_congruence((A %= 9) / 19);
  No_Reduction_Product dp6 = dp5;

  bool ok =  (dp1 == dp2 && dp3 == dp4 && dp5 == dp6);


  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// space_dimension()
bool
test12() {
  Variable A(0);
  Variable E(4);

#ifdef PH_IS_NNC
  Constraint_System cs(A + E < 9);
#else
  Constraint_System cs(A + E <= 9);
#endif

  Constraints_Product dp1(cs);
  Smash_Product dp2(cs);
  No_Reduction_Product dp3(cs);

  bool ok = (dp1.space_dimension() == 5
             && dp2.space_dimension() == 5
             && dp3.space_dimension() == 5);


  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  return ok;
}

// Copy constructor.
bool
test13() {
  Variable A(0);
  Variable B(2);

  Constraint_System cs(A - B == 0);

  Constraints_Product dp1(cs);
  Constraints_Product dp2(dp1);
  Smash_Product dp3(cs);
  Smash_Product dp4(dp3);
  No_Reduction_Product dp5(cs);
  No_Reduction_Product dp6(dp5);

  bool ok =  (dp1 == dp2 && dp3 == dp4 && dp5 == dp6);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  return ok;
}

// affine_dimension()
bool
test14() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraint_System cs;
  cs.insert(A - C <= 9);
  cs.insert(A - C >= 9);
  cs.insert(B == 2);

  Constraints_Product dp1(3);
  dp1.add_constraints(cs);
  Smash_Product dp2(3);
  dp2.add_constraints(cs);
  No_Reduction_Product dp3(3);
  dp3.add_constraints(cs);

  bool ok = (dp1.affine_dimension() == 1
             && dp2.affine_dimension() == 1
             && dp3.affine_dimension() == 1
	     && dp1.domain1().affine_dimension() == 1
	     && dp1.domain2().affine_dimension() == 1
	     && dp2.domain1().affine_dimension() == 2
	     && dp2.domain2().affine_dimension() == 1
	     && dp3.domain1().affine_dimension() == 2
	     && dp3.domain2().affine_dimension() == 1);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");
  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");
  print_congruences(dp3.domain1(),
    "*** dp3.domain1() ***");
  print_constraints(dp3.domain2(),
     "*** dp3.domain2() ***");

  return ok;
}

// congruences()
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraints_Product dp1(3);
  dp1.add_congruence(A %= 9);
  dp1.add_congruence(B + C %= 3);
  Smash_Product dp2(3);
  dp2.add_congruence(A %= 9);
  dp2.add_congruence(B + C %= 3);
  No_Reduction_Product dp3(3);
  dp3.add_congruence(A %= 9);
  dp3.add_congruence(B + C %= 3);

  Congruence_System cgs;
  cgs.insert(A %= 9);
  cgs.insert(B + C %= 3);

  Grid known_gr(cgs);

  Grid gr1(dp1.congruences());
  Grid gr2(dp2.congruences());
  Grid gr3(dp3.congruences());

  bool ok = (gr1 == known_gr && gr2 == known_gr && gr3 == known_gr);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  return ok;
}

// minimized_congruences()
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraints_Product dp1(3);
  dp1.add_congruence(B + C %= 3);
  dp1.add_constraint(A >= 9);
  dp1.add_constraint(A <= 9);
  Smash_Product dp2(3);
  dp2.add_congruence(B + C %= 3);
  dp2.add_constraint(A >= 9);
  dp2.add_constraint(A <= 9);
  No_Reduction_Product dp3(3);
  dp3.add_congruence(B + C %= 3);
  dp3.add_constraint(A >= 9);
  dp3.add_constraint(A <= 9);

  Congruence_System cgs;
  cgs.insert(B + C %= 3);

  Grid known_gr(cgs);

  Grid gr1(dp1.minimized_congruences());
  Grid gr2(dp2.minimized_congruences());
  Grid gr3(dp3.minimized_congruences());

  bool ok = (gr1 != known_gr && gr2 != known_gr && gr3 == known_gr);

  print_congruences(dp2.domain1(),
    "*** dp2.domain1() ***");
  print_constraints(dp2.domain2(),
     "*** dp2.domain2() ***");

  return ok;
}

// constraints()
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

#ifdef PH_IS_NNC
  Constraint c(A > 9);
#else
  Constraint c(A >= 9);
#endif

  Constraints_Product dp1(3);
  dp1.add_congruence((B + C %= 3) / 0);
  dp1.add_constraint(c);
  dp1.add_constraint(A <= 11);
  Smash_Product dp2(3);
  dp2.add_congruence((B + C %= 3) / 0);
  dp2.add_constraint(c);
  dp2.add_constraint(A <= 11);
  No_Reduction_Product dp3(3);
  dp3.add_congruence((B + C %= 3) / 0);
  dp3.add_constraint(c);
  dp3.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph1(dp1.space_dimension());
  NNC_Polyhedron ph2(dp2.space_dimension());
  NNC_Polyhedron ph3(dp3.space_dimension());
#else
  C_Polyhedron ph1(dp1.space_dimension());
  C_Polyhedron ph2(dp2.space_dimension());
  C_Polyhedron ph3(dp3.space_dimension());
#endif

  ph1.add_constraints(dp1.constraints());
  ph2.add_constraints(dp2.constraints());
  ph3.add_constraints(dp3.constraints());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(dp1.space_dimension());
#else
  C_Polyhedron known_ph(dp1.space_dimension());
#endif
  known_ph.add_constraint(B + C == 3);
  known_ph.add_constraint(A <= 11);
  known_ph.add_constraint(c);

  bool ok = (ph1 == known_ph && ph2 == known_ph && ph3 == known_ph);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  return ok;
}

// minimized_constraints()
bool
test18() {
  Variable A(0);
  Variable B(1);
  Variable C(2);


#ifdef PH_IS_NNC
  Constraint c(A > 9);
#else
  Constraint c(A >= 9);
#endif

  Constraints_Product dp1(3);
  dp1.add_congruence((B + C %= 3) / 0);
  dp1.add_constraint(c);
  dp1.add_constraint(A <= 11);
  Smash_Product dp2(3);
  dp2.add_congruence((B + C %= 3) / 0);
  dp2.add_constraint(c);
  dp2.add_constraint(A <= 11);
  No_Reduction_Product dp3(3);
  dp3.add_congruence((B + C %= 3) / 0);
  dp3.add_constraint(c);
  dp3.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph1(dp1.space_dimension());
  NNC_Polyhedron ph2(dp2.space_dimension());
  NNC_Polyhedron ph3(dp3.space_dimension());
#else
  C_Polyhedron ph1(dp1.space_dimension());
  C_Polyhedron ph2(dp2.space_dimension());
  C_Polyhedron ph3(dp3.space_dimension());
#endif

  ph1.add_constraints(dp1.constraints());
  ph2.add_constraints(dp2.constraints());
  ph3.add_constraints(dp3.constraints());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(dp1.space_dimension());
#else
  C_Polyhedron known_ph(dp1.space_dimension());
#endif
  known_ph.add_constraint(B + C == 3);
  known_ph.add_constraint(A <= 11);
  known_ph.add_constraint(c);

  bool ok = (ph1 == known_ph && ph2 == known_ph && ph3 == known_ph);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

  return ok;
}

// generators()
bool
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Smash_Product dp(3);
  dp.add_congruence((B + C %= 3) / 0);
#ifdef PH_IS_NNC
  dp.add_constraint(A > 9);
#else
  dp.add_constraint(A >= 9);
#endif
  dp.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph(dp.space_dimension(), EMPTY);
#else
  C_Polyhedron ph(dp.space_dimension(), EMPTY);
#endif
  ph.add_generators(dp.generators());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(dp.space_dimension());
#else
  C_Polyhedron known_ph(dp.space_dimension());
#endif

  bool ok = (ph == known_ph);

  print_generators(dp.domain1(),
    "*** dp.domain1() ***");
  print_generators(dp.domain2(),
     "*** dp.domain2() ***");

  return ok;
}

// minimized_generators()
bool
test20() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraints_Product dp(3);
  dp.add_congruence((B + C %= 3) / 0);
#ifdef PH_IS_NNC
  dp.add_constraint(A > 9);
#else
  dp.add_constraint(A >= 9);
#endif
  dp.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph(dp.space_dimension(), EMPTY);
#else
  C_Polyhedron ph(dp.space_dimension(), EMPTY);
#endif
  ph.add_generators(dp.minimized_generators());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(dp.space_dimension());
#else
  C_Polyhedron known_ph(dp.space_dimension());
#endif

  bool ok = (ph == known_ph);

  print_generators(dp.domain1(),
    "*** dp.domain1() ***");
  print_generators(dp.domain2(),
     "*** dp.domain2() ***");

  return ok;
}

// inconsistent constraints()
bool
test21() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Constraints_Product dp1(3);
  dp1.add_congruence((B + C %= 3) / 0);
  Smash_Product dp2(3);
  dp2.add_congruence((B + C %= 3) / 0);
  No_Reduction_Product dp3(3);
  dp3.add_congruence((B + C %= 3) / 0);
#ifdef PH_IS_NNC
  dp1.add_constraint(A > 12);
  dp2.add_constraint(A > 12);
  dp3.add_constraint(A > 12);
#else
  dp1.add_constraint(A >= 12);
  dp2.add_constraint(A >= 12);
  dp3.add_constraint(A >= 12);
#endif
  dp1.add_constraint(A <= 11);
  dp2.add_constraint(A <= 11);
  dp3.add_constraint(A <= 11);

  Constraints_Product known_dp1(dp1.space_dimension(), EMPTY);
  Smash_Product known_dp2(dp2.space_dimension(), EMPTY);
  No_Reduction_Product known_dp3(dp3.space_dimension(), EMPTY);

  bool ok = (dp1 == known_dp1 && dp2 == known_dp2 && dp3 != known_dp3);

  print_congruences(dp1.domain1(),
    "*** dp1.domain1() ***");
  print_constraints(dp1.domain2(),
     "*** dp1.domain2() ***");

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
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
  DO_TEST(test16);
  DO_TEST(test17);
  DO_TEST(test18);
  DO_TEST(test19);
  DO_TEST(test20);
  DO_TEST(test21);
END_MAIN
