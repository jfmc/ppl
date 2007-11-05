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

// Product()
bool
test01() {
  CProduct pd1;
  CProduct pd2(0, UNIVERSE);
  SProduct pd3;
  SProduct pd4(0, UNIVERSE);

  bool ok = (pd1 == pd2 && pd3 == pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd3, "*** pd3 congruences ***");
  print_constraints(pd3, "*** pd3 constraints ***");

  return ok;
}

// Product(dims, type)
bool
test02() {
  CProduct pd1(3);
  CProduct pd2(3, EMPTY);
  SProduct pd3(3);
  SProduct pd4(3, EMPTY);

  bool ok = (pd1 != pd2 && pd3 != pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// Product(ccgs), add_congruence(cg)
bool
test03() {
  Variable A(0);

  const Congruence_System cgs((A %= 0) / 4);

  CProduct pd1(cgs);
  CProduct pd2(1);
  pd2.add_congruence((A %= 0) / 4);
  SProduct pd3(cgs);
  SProduct pd4(1);
  pd4.add_congruence((A %= 0) / 4);

  bool ok =  (pd1 == pd2 && pd3 == pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// Product(cgs), domain1(), domain2()
bool
test04() {
  Variable A(0);

  const Congruence_System cgs((A %= 0) / 4);

  CProduct pd1(cgs);
  CProduct pd2(1);
  pd2.add_congruence((A %= 0) / 4);
  SProduct pd3(cgs);
  SProduct pd4(1);
  pd4.add_congruence((A %= 0) / 4);

  bool ok =  (pd1 == pd2 && pd3 == pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// Product(ccs), add_constraint(cc)
bool
test05() {
  Variable A(0);

  const Constraint_System cs(A >= 0);

  CProduct pd1(cs);
  CProduct pd2(1);
  pd2.add_constraint(static_cast<const Constraint>(A >= 0));
  SProduct pd3(cs);
  SProduct pd4(1);
  pd4.add_constraint(static_cast<const Constraint>(A >= 0));

  bool ok =  (pd1 == pd2 && pd3 == pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// Product(cs)
bool
test06() {
  Variable A(0);

  Constraint_System cs(A == 9);

  CProduct pd1(cs);
  CProduct pd2(1);
  pd2.add_constraint(A == 9);
  SProduct pd3(cs);
  SProduct pd4(1);
  pd4.add_constraint(A == 9);

  Grid known_gr(1);
  known_gr.add_congruence(A == 9);

  bool ok =  (pd1 == pd2 && pd1.domain1() == known_gr
               && pd3 == pd4 && pd3.domain1() == known_gr);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd3, "*** pd3 congruences ***");
  print_constraints(pd3, "*** pd3 constraints ***");

  return ok;
}

#if 0
// Product(cggs), add_grid_generator(g)
bool
test07() {
  Variable A(0);
  Variable B(1);

  const Grid_Generator_System gs(grid_point(A + B));

  CProduct pd(gs);

  Grid known_gr(2, EMPTY);
  known_gr.add_grid_generator(grid_point(A + B));

  bool ok = (pd.domain1() == known_gr);

  print_congruences(pd.domain1(), "*** pd.domain1() ***");
  print_constraints(pd.domain2(), "*** pd.domain2() ***");

  return ok;
}
#endif

#if 0
// Product(ggs)
bool
test08() {
  Variable A(0);
  Variable C(2);

  Grid_Generator_System gs(grid_point(A + 7*C));

  SProduct pd(gs);

  Grid known_gr(3, EMPTY);
  known_gr.add_grid_generator(grid_point(A + 7*C));

  bool ok = (pd.domain1() == known_gr);

  print_congruences(pd, "*** pd congruences ***");
  print_constraints(pd, "*** pd constraints ***");

  return ok;
}
#endif

// Product(bounding_box)
bool
test09() {
  Variable B(1);

  Rational_Box box(2);
  box.add_constraint(3*B == 2);

  CProduct pd(box);

  Grid known_gr(2);
  known_gr.add_congruence(3*B == 2);

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(2);
#else
  C_Polyhedron known_ph(2);
#endif
  known_ph.add_constraint(3*B == 2);

  bool ok = (pd.domain1() == known_gr && pd.domain2() == known_ph);

  print_congruences(pd, "*** pd congruences ***");
  print_constraints(pd, "*** pd constraints ***");

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

  CProduct pd(box, From_Covering_Box());

  Grid known_gr(2);
  known_gr.add_congruence(3*B %= 0);

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(2);
#else
  C_Polyhedron known_ph(2);
#endif

  bool ok = (pd.domain1() == known_gr && pd.domain2() == known_ph);

  print_congruences(pd, "*** pd congruences ***");
  print_constraints(pd, "*** pd constraints ***");

  return ok;
}
#endif

// operator=
bool
test11() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs(A + B <= 9);

  CProduct pd1(cs);
  pd1.add_congruence((A %= 9) / 19);
  CProduct pd2 = pd1;
  SProduct pd3(cs);
  pd3.add_congruence((A %= 9) / 19);
  SProduct pd4 = pd3;

  bool ok =  (pd1 == pd2 && pd3 == pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd3, "*** pd3 congruences ***");
  print_constraints(pd3, "*** pd3 constraints ***");

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

  CProduct pd1(cs);
  SProduct pd2(cs);

  bool ok = (pd1.space_dimension() == 5
             && pd2.space_dimension() == 5);


  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// Copy constructor.
bool
test13() {
  Variable A(0);
  Variable B(2);

  Constraint_System cs(A - B == 0);

  CProduct pd1(cs);
  CProduct pd2(pd1);
  SProduct pd3(cs);
  SProduct pd4(pd3);

  bool ok =  (pd1 == pd2 && pd3 == pd4);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd3, "*** pd3 congruences ***");
  print_constraints(pd3, "*** pd3 constraints ***");

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

  CProduct pd1(3);
  pd1.add_constraints(cs);
  SProduct pd2(3);
  pd2.add_constraints(cs);

  bool ok = (pd1.affine_dimension() == 1
             && pd2.affine_dimension() == 1
	     && pd1.domain1().affine_dimension() == 1
	     && pd1.domain2().affine_dimension() == 1
	     && pd2.domain1().affine_dimension() == 2
	     && pd2.domain2().affine_dimension() == 1);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// congruences()
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  CProduct pd1(3);
  pd1.add_congruence(A %= 9);
  pd1.add_congruence(B + C %= 3);
  SProduct pd2(3);
  pd2.add_congruence(A %= 9);
  pd2.add_congruence(B + C %= 3);

  Congruence_System cgs;
  cgs.insert(A %= 9);
  cgs.insert(B + C %= 3);

  Grid known_gr(cgs);

  Grid gr1(pd1.congruences());
  Grid gr2(pd2.congruences());

  bool ok = (gr1 == known_gr && gr2 == known_gr);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

// minimized_congruences()
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  CProduct pd1(3);
  pd1.add_congruence(B + C %= 3);
  pd1.add_constraint(A >= 9);
  pd1.add_constraint(A <= 9);
  SProduct pd2(3);
  pd2.add_congruence(B + C %= 3);
  pd2.add_constraint(A >= 9);
  pd2.add_constraint(A <= 9);

  Congruence_System cgs;
  cgs.insert(B + C %= 3);
  cgs.insert(A == 9);

  Grid known_gr(cgs);

  Grid gr1(pd1.minimized_congruences());
  Grid gr2(pd2.minimized_congruences());

  bool ok = (gr1 == known_gr && gr2 == known_gr);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

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

  CProduct pd1(3);
  pd1.add_congruence((B + C %= 3) / 0);
  pd1.add_constraint(c);
  pd1.add_constraint(A <= 11);
  SProduct pd2(3);
  pd2.add_congruence((B + C %= 3) / 0);
  pd2.add_constraint(c);
  pd2.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph1(pd1.space_dimension());
  NNC_Polyhedron ph2(pd2.space_dimension());
#else
  C_Polyhedron ph1(pd1.space_dimension());
  C_Polyhedron ph2(pd2.space_dimension());
#endif

  ph1.add_constraints(pd1.constraints());
  ph2.add_constraints(pd2.constraints());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(pd1.space_dimension());
#else
  C_Polyhedron known_ph(pd1.space_dimension());
#endif
  known_ph.add_constraint(B + C == 3);
  known_ph.add_constraint(A <= 11);
  known_ph.add_constraint(c);

  bool ok = (ph1 == known_ph && ph2 == known_ph);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

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

  CProduct pd1(3);
  pd1.add_congruence((B + C %= 3) / 0);
  pd1.add_constraint(c);
  pd1.add_constraint(A <= 11);
  SProduct pd2(3);
  pd2.add_congruence((B + C %= 3) / 0);
  pd2.add_constraint(c);
  pd2.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph1(pd1.space_dimension());
  NNC_Polyhedron ph2(pd2.space_dimension());
#else
  C_Polyhedron ph1(pd1.space_dimension());
  C_Polyhedron ph2(pd2.space_dimension());
#endif

  ph1.add_constraints(pd1.constraints());
  ph2.add_constraints(pd2.constraints());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(pd1.space_dimension());
#else
  C_Polyhedron known_ph(pd1.space_dimension());
#endif
  known_ph.add_constraint(B + C == 3);
  known_ph.add_constraint(A <= 11);
  known_ph.add_constraint(c);

  bool ok = (ph1 == known_ph && ph2 == known_ph);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

  return ok;
}

#if 0
// generators()
bool
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  SProduct pd(3);
  pd.add_congruence((B + C %= 3) / 0);
#ifdef PH_IS_NNC
  pd.add_constraint(A > 9);
#else
  pd.add_constraint(A >= 9);
#endif
  pd.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph(pd.space_dimension(), EMPTY);
#else
  C_Polyhedron ph(pd.space_dimension(), EMPTY);
#endif
  ph.add_generators(pd.generators());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(pd.space_dimension());
#else
  C_Polyhedron known_ph(pd.space_dimension());
#endif

  bool ok = (ph == known_ph);

  print_congruences(pd, "*** pd congruences ***");
  print_constraints(pd, "*** pd constraints ***");

  return ok;
}

// minimized_generators()
bool
test20() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  CProduct pd(3);
  pd.add_congruence((B + C %= 3) / 0);
#ifdef PH_IS_NNC
  pd.add_constraint(A > 9);
#else
  pd.add_constraint(A >= 9);
#endif
  pd.add_constraint(A <= 11);

#ifdef PH_IS_NNC
  NNC_Polyhedron ph(pd.space_dimension(), EMPTY);
#else
  C_Polyhedron ph(pd.space_dimension(), EMPTY);
#endif
  ph.add_generators(pd.minimized_generators());

#ifdef PH_IS_NNC
  NNC_Polyhedron known_ph(pd.space_dimension());
#else
  C_Polyhedron known_ph(pd.space_dimension());
#endif

  bool ok = (ph == known_ph);

  print_congruences(pd, "*** pd congruences ***");
  print_constraints(pd, "*** pd constraints ***");

  return ok;
}
#endif

// inconsistent constraints()
bool
test21() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  CProduct pd1(3);
  pd1.add_congruence((B + C %= 3) / 0);
  SProduct pd2(3);
  pd2.add_congruence((B + C %= 3) / 0);
#ifdef PH_IS_NNC
  pd1.add_constraint(A > 11);
  pd2.add_constraint(A > 11);
#else
  pd1.add_constraint(A >= 12);
  pd2.add_constraint(A >= 12);
#endif
  pd1.add_constraint(A <= 11);
  pd2.add_constraint(A <= 11);

  CProduct known_pd1(pd1.space_dimension(), EMPTY);
  SProduct known_pd2(pd2.space_dimension(), EMPTY);

  bool ok = (pd1 == known_pd1 && pd2 == known_pd2);

  print_congruences(pd1, "*** pd1 congruences ***");
  print_constraints(pd1, "*** pd1 constraints ***");
  print_congruences(pd2, "*** pd2 congruences ***");
  print_constraints(pd2, "*** pd2 constraints ***");

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
  //DO_TEST(test07);
  //DO_TEST(test08);
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
//  DO_TEST(test19);
//  DO_TEST(test20);
  DO_TEST(test21);
END_MAIN
