/* Test BD_Shape::add_space_dimensions_and_embed():
   we add two variables to a BD_Shape.
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

namespace {

bool
test01() {
  Variable x(0);
  //Variable y(1);
  Variable z(2);

  TBD_Shape bd(2);
  bd.add_constraint(x <= 2);

  print_constraints(bd, "*** bd ***");

  bd.add_space_dimensions_and_embed(2);
  bd.add_constraint(z <= 2);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(z <= 2);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result) ;

  print_constraints(bd,
		    "*** bd.add_space_dimensions_and_embed(2) "
		    "and bd.add_constraint(z <= 2) ***");

  return ok;
}

bool
test02() {
  TBD_Shape bd1(0, EMPTY);
  TBD_Shape bd2(1, EMPTY);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.add_space_dimensions_and_embed(2);
  bd2.add_space_dimensions_and_embed(1);

  bool ok = (bd1 == bd2) ;

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_embed(2) ***");
  print_constraints(bd2, "*** bd2.add_space_dimensions_and_embed(1) ***");

  return ok;
}

bool
test03() {
  TBD_Shape bd1(0, UNIVERSE);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_embed(3);

  BD_Shape<mpq_class> known_result(3, UNIVERSE);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimension_and_embed(3) ***");

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(0);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(4);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(4) ***");

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(A == 1);
  bd1.add_constraint(C - B >= 9);

  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(0);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(0) ***");

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  return ok;
}

bool
test06() {
  //Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(1);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(3);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(3) ***");

  return ok;
}

bool
test07() {
  TBD_Shape bd(10, UNIVERSE);

  bd.remove_higher_space_dimensions(5);
  bd.add_space_dimensions_and_embed(6);

  return bd == TBD_Shape(11, UNIVERSE);
}

bool
test08() {
  Variable x(0);
  //Variable y(1);
  Variable z(2);
  Variable w(3);

  TBD_Shape bd(2);
  bd.add_constraint(x <= 2);

  print_constraints(bd, "*** bd ***");

  bd.add_space_dimensions_and_project(2);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(w == 0);
  known_result.add_constraint(z == 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result) ;

  print_constraints(bd, "*** bd.add_space_dimensions_and_project(2) ***");

  return ok;
}

bool
test09() {
  TBD_Shape bd1(0, EMPTY);
  TBD_Shape bd2(1, EMPTY);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.add_space_dimensions_and_project(2);
  bd2.add_space_dimensions_and_project(1);

  bool ok = (bd1 == bd2) ;

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(2) ***");
  print_constraints(bd2, "*** bd2.add_space_dimensions_and_project(1) ***");

  return ok;
}

bool
test10() {
  TBD_Shape bd1(0, EMPTY);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(3);

  BD_Shape<mpq_class> known_result(3, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimension_and_project(3) ***");

  return ok;
}

bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(0);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(4);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(4) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
  NEW_TEST(test09);
  NEW_TEST(test10);
  NEW_TEST(test11);
END_MAIN
