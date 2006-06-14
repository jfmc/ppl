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

// Direct_Product(cgs), domain1(), domain2()
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

// space_dimension()
bool
test11() {
  Variable A(0);
  Variable E(4);

  Constraint_System cs(A + E < 9);

  Direct_Product<NNC_Polyhedron, Grid> dp(cs);

  bool ok = (dp.space_dimension() == 5);

  return ok;
}

// Copy constructor.
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

// affine_dimension()
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

#define define_identical(type)					\
  bool								\
  identical(const type& cs1, const type& cs2) {			\
    type::const_iterator i1 = cs1.begin();			\
    type::const_iterator i2 = cs2.begin();			\
    type::const_iterator cs1_end = cs1.end();			\
    type::const_iterator cs2_end = cs2.end();			\
    for (; i1 != cs1_end && i2 != cs2_end; ++i1, ++i2) {	\
      if (i1->space_dimension() != i2->space_dimension())	\
	return false;						\
      for (dimension_type d = i1->space_dimension(); d-- > 0; )	\
	if (i1->coefficient(Variable(d))			\
	    != i2->coefficient(Variable(d)))			\
	  return false;						\
    }								\
    if (i2 == cs2.end() && i1 == cs1.end())			\
      return true;						\
    return false;						\
  }

define_identical(Congruence_System);

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
  cgs.insert(Linear_Expression(0) %= -1);
  cgs.insert(A %= 9);
  cgs.insert(B + C %= 3);

  bool ok = identical(dp.congruences(), cgs);

  return ok;
}

// minimized_congruences()
bool
test15() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(B + C %= 3);
  dp.add_constraint(A >= 9);
  dp.add_constraint(A <= 9);

  Congruence_System cgs;
  cgs.insert(B + C %= 0);
  cgs.insert((A %= 9) / 0);
  cgs.insert(Linear_Expression(0) %= -1);

  bool ok = identical(dp.minimized_congruences(), cgs);

  return ok;
}

// constraints()
bool
test16() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence((B + C %= 3) / 0);
  dp.add_constraint(A > 9);
  dp.add_constraint(A <= 11);

  NNC_Polyhedron ph(dp.space_dimension());
  ph.add_constraints(dp.constraints());

  NNC_Polyhedron known_ph(dp.space_dimension());
  known_ph.add_constraint(B + C == 3);
  known_ph.add_constraint(A <= 11);
  known_ph.add_constraint(A > 9);

  bool ok = (ph == known_ph);

  return ok;
}

// minimized_constraints()
bool
test17() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence((B + C %= 3) / 0);
  dp.add_constraint(A > 9);
  dp.add_constraint(A <= 11);

  NNC_Polyhedron ph(dp.space_dimension());
  ph.add_constraints(dp.constraints());

  NNC_Polyhedron known_ph(dp.space_dimension());
  known_ph.add_constraint(A > 9);
  known_ph.add_constraint(B + C == 3);
  known_ph.add_constraint(A <= 11);

  bool ok = (ph == known_ph);

  return ok;
}

// generators()
bool
test18() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
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
test19() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
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
test20() {
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
test21() {
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
test22() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A == 1);
  dp.add_constraint(A == 3);

  bool ok = dp.is_empty();

  return ok;
}

// is_universe() where both domain objects are empty.
bool
test23() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3, EMPTY);

  bool ok = !dp.is_universe();

  return ok;
}

// is_universe() where one domain object is universe.
bool
test24() {
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
test25() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);

  bool ok = dp.is_universe();

  return ok;
}

// intersection_assign()
bool
test26() {
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

// upper_bound_assign(dp2)
bool
test27() {
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
test28() {
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

// difference_assign()
bool
test29() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp1(3);
  dp1.add_constraint(A > 0);
  dp1.add_congruence((A - B %= 0) / 2);

  Direct_Product<NNC_Polyhedron, Grid> dp2(3);
  dp2.add_constraint(A > 3);
  dp2.add_congruence((A - B %= 0) / 4);

  dp1.difference_assign(dp2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_constraint(A > 0);
  known_dp.add_constraint(A <= 3);
  known_dp.add_congruence((A - B %= 2) / 4);

  bool ok = (dp1 == known_dp);

  return ok;
}

// add_space_dimensions_and_embed()
bool
test30() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp1(2);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  dp1.add_space_dimensions_and_embed(3);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(5);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(A >= 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// add_space_dimensions_and_project()
bool
test31() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp1(2);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  dp1.add_space_dimensions_and_project(1);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(C == 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// concatenate_assign()
bool
test32() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Direct_Product<NNC_Polyhedron, Grid> dp1(2);
  dp1.add_constraint(A >= 0);
  dp1.add_congruence((A %= 0) / 2);

  Direct_Product<NNC_Polyhedron, Grid> dp2(2);
  dp2.add_constraint(A < 1);
  dp2.add_constraint(B > 0);

  dp1.concatenate_assign(dp2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(4);
  known_dp.add_constraint(A >= 0);
  known_dp.add_congruence((A %= 0) / 2);
  known_dp.add_constraint(C < 1);
  known_dp.add_constraint(D > 0);

  bool ok = (dp1 == known_dp);

  return ok;
}

// remove_space_dimensions()
bool
test33() {
  Variable A(0);
  Variable C(2);
  Variable D(3);

  Direct_Product<NNC_Polyhedron, Grid> dp(4);
  dp.add_constraint(A >= 0);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A - C %= 0) / 2);

  Variables_Set vars;
  vars.insert(C);
  vars.insert(D);

  dp.remove_space_dimensions(vars);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_congruence((A %= 0) / 2);

  bool ok = (dp == known_dp);

  return ok;
}

// remove_higher_space_dimensions()
bool
test34() {
  Variable A(0);
  Variable C(2);
  Variable D(3);

  Direct_Product<NNC_Polyhedron, Grid> dp(4);
  dp.add_constraint(A >= 0);
  dp.add_congruence((A %= 0) / 2);
  dp.add_congruence((A - C %= 0) / 2);

  dp.remove_higher_space_dimensions(2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(2);
  known_dp.add_constraint(A >= 0);
  known_dp.add_congruence((A %= 0) / 2);

  bool ok = (dp == known_dp);

  return ok;
}

// map_space_dimensions()
bool
test35() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(2);
  dp.add_constraint(A >= 0);
  dp.add_congruence((A - B %= 0) / 2);

  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);

  dp.map_space_dimensions(function);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(2);
  known_dp.add_constraint(B >= 0);
  known_dp.add_congruence((B - A %= 0) / 2);

  bool ok = (dp == known_dp);

  return ok;
}

// expand_space_dimension()
bool
test36() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence((A + B %= 2) / 7);
  dp.add_constraint(A >= 0);

  dp.expand_space_dimension(A, 1);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(4);
  known_dp.add_congruence((A + B %= 2) / 7);
  known_dp.add_congruence((D + B %= 2) / 7);
  known_dp.add_constraint(A >= 0);
  known_dp.add_constraint(D >= 0);

  bool ok = (dp == known_dp);

  return ok;
}

// fold_space_dimensions()
bool
test37() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
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

  Direct_Product<NNC_Polyhedron, Grid> known_dp(1);
  known_dp.add_congruence((A %= 2) / 7);
  known_dp.add_constraint(A <= 10);

  bool ok = (dp == known_dp);

  return ok;
}

// affine_image()
bool
test38() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence((B %= 2) / 14);
  dp.add_constraint(A <= 5);
  dp.add_constraint(B <= 10);

  dp.affine_image(A, B + C);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence((B %= 2) / 14);
  known_dp.add_constraint(A - B - C == 0);
  known_dp.add_constraint(B <= 10);

  bool ok = (dp == known_dp);

  return ok;
}

// affine_preimage()
bool
test39() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A - B == 0);
  dp.add_congruence((A %= 0) / 3);

  dp.affine_preimage(A, B);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence((B %= 0) / 3);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(v, e, relsym, d)
bool
test40() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((A + B %= 0) / 2);
  dp.add_constraint(B >= 0);
  dp.add_constraint(A - B >= 0);

  dp.generalized_affine_image(A, EQUAL, A + 2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence(A %= 0);
  known_dp.add_congruence((A + B %= 0) / 2);
  known_dp.add_constraint(B >= 0);
  known_dp.add_constraint(A - B >= 2);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(v, e, d, modulus)
bool
test41() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((A + B %= 0) / 2);
  dp.add_constraint(A > 3);

  dp.generalized_affine_image(B, A + 1, 2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence((A - 2*B %= -1) / 2);
  known_dp.add_congruence(A %= 0);
  known_dp.add_constraint(A > 3);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(v, e, relsym, d)
bool
test42() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A >= 0);
  dp.add_constraint(A <= 4);
  dp.add_constraint(B <= 5);
  dp.add_constraint(A <= B);
  dp.add_congruence(A %= B);

  dp.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL, A+2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_constraint(0 <= A);
  known_dp.add_constraint(A <= 3);
  known_dp.add_congruence(A %= B);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(v, e, d, modulus)
bool
test43() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(A %= 0);
  dp.add_congruence((B %= 0) / 2);

  dp.generalized_affine_preimage(B, A + B, 1, 0);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3, EMPTY);
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(parameter(2*B));
  known_dp.add_grid_generator(parameter(A + B));
  known_dp.add_grid_generator(grid_line(C));
  known_dp.add_generator(point());
  known_dp.add_generator(line(A));
  known_dp.add_generator(line(B));
  known_dp.add_generator(line(C));

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(lhs, relsym, rhs)
bool
test44() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_congruence(A %= 0);
  dp.add_constraint(B >= 0);
  dp.add_constraint(A - B >= 1);

  dp.generalized_affine_image(Linear_Expression(2),
			      LESS_THAN_OR_EQUAL,
			      A + B);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence(A %= 0);
  known_dp.add_constraint(B >= 0);
  known_dp.add_constraint(A - B >= 1);
  known_dp.add_constraint(2 <= A + B);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_image(lhs, rhs, modulus)
bool
test45() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(2);
  dp.add_congruence((A %= 0) / 1);
  dp.add_congruence((B %= 0) / 2);
  dp.add_constraint(A <= 3);

  dp.generalized_affine_image(A + 2*B, A - B, 3);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(2, EMPTY);
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(grid_point(B, 2));
  known_dp.add_grid_generator(grid_line(2*A - B));
  known_dp.add_generator(point(3*A));
  known_dp.add_generator(ray(-A));
  known_dp.add_generator(line(B));

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(lhs, relsym, rhs)
bool
test46() {
  Variable A(0);
  Variable B(1);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A >= 0);
  dp.add_constraint(A <= 4);
  dp.add_constraint(B <= 5);
  dp.add_constraint(A <= B);
  dp.add_congruence(A %= B);

  dp.generalized_affine_preimage(1*B, GREATER_THAN_OR_EQUAL, A+2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_constraint(0 <= A);
  known_dp.add_constraint(A <= 3);
  known_dp.add_congruence(A %= B);

  bool ok = (dp == known_dp);

  return ok;
}

// generalized_affine_preimage(lhs, rhs, modulus)
bool
test47() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3);
  dp.add_constraint(A - B == 0);

  dp.generalized_affine_preimage(A - B, 2*A - 2*B, 5);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3);
  known_dp.add_congruence((2*A - 2*B %= 0) / 5);
  known_dp.add_constraint(A - B >= 0);
  known_dp.add_constraint(A - B <= 0);

  bool ok = (dp == known_dp);

  return ok;
}

// time_elapse_assign(y)
bool
test48() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp1(3, EMPTY);
  dp1.add_grid_generator(grid_point());
  dp1.add_grid_generator(grid_point(A + 2*B - 3*C, 3));
  dp1.add_generator(point(3*A));
  dp1.add_generator(ray(A));
  dp1.add_generator(point(3*B));
  dp1.add_generator(ray(B));
  dp1.add_generator(line(C));

  Direct_Product<NNC_Polyhedron, Grid> dp2(3, EMPTY);
  dp2.add_grid_generator(grid_point(3*A - B + 4*C, 7));
  dp2.add_generator(point(A + B));

  dp1.time_elapse_assign(dp2);

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3, EMPTY);
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
test49() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Direct_Product<NNC_Polyhedron, Grid> dp(3, EMPTY);
  dp.add_grid_generator(grid_point());
  dp.add_grid_generator(grid_point(A + 2*B - 3*C, 3));
  dp.add_generator(point(A));
  dp.domain1().constraints();
  dp.add_generator(closure_point());
  dp.add_generator(ray(A));
  dp.add_generator(ray(B));

  dp.topological_closure_assign();

  Direct_Product<NNC_Polyhedron, Grid> known_dp(3, EMPTY);
  known_dp.add_generator(point());
  known_dp.add_generator(ray(A));
  known_dp.add_generator(ray(B));
  // Grid_Generators as in dp.
  known_dp.add_grid_generator(grid_point());
  known_dp.add_grid_generator(grid_point(A + 2*B - 3*C, 3));

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
  DO_TEST(test21);
  DO_TEST(test22);
  DO_TEST(test23);
  DO_TEST(test24);
  DO_TEST(test25);
  DO_TEST(test26);
  DO_TEST(test27);
  DO_TEST(test28);
  DO_TEST(test29);
  DO_TEST(test30);
  DO_TEST(test31);
  DO_TEST(test32);
  DO_TEST(test33);
  DO_TEST(test34);
  DO_TEST(test35);
  DO_TEST(test36);
  DO_TEST(test37);
  DO_TEST(test38);
  DO_TEST(test39);
  DO_TEST(test40);
  DO_TEST(test41);
  DO_TEST(test42);
  DO_TEST(test43);
  DO_TEST(test44);
  DO_TEST(test45);
  DO_TEST(test46);
  DO_TEST(test47);
  DO_TEST(test48);
  DO_TEST(test49);
END_MAIN
