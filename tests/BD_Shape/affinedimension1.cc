/* Test BD_Shape::affine_dimension().
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

namespace {

bool
test01() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(3);

  bd.add_constraint(x <= 2);
  bd.add_constraint(x - y == 3);
  bd.add_constraint(y <= 2);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 2);

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBD_Shape bd(5);

  bd.add_constraint(A <= 5);
  bd.add_constraint(A - B == 3);
  bd.add_constraint(C <= 2);
  bd.add_constraint(E - D == 2);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 3);

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBD_Shape bd(5);

  bd.add_constraint(A == 5);
  bd.add_constraint(A - B == 3);
  bd.add_constraint(C <= 2);
  bd.add_constraint(E - D == 2);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 2);

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A <= 3);
  bd.add_constraint(B - A <= -5);
  bd.add_constraint(-B <= 2);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 0);

  return ok;
}

bool
test05() {
  TBD_Shape bd(2, EMPTY);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 0);

  return ok;
}

bool
test06() {
  TBD_Shape bd(2);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 2);

  return ok;
}

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable D(3);

  TBD_Shape bd(4);
  bd.add_constraint(A <= 1);
  bd.add_constraint(A - D == 8);
  bd.add_constraint(B <= 7);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 3);

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable D(3);
  Variable E(4);

  TBD_Shape bd(5);
  bd.add_constraint(A == 1);
  bd.add_constraint(E == 1);
  bd.add_constraint(A - D == 8);
  bd.add_constraint(B <= 7);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 2);

  return ok;
}

bool
test09() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A == 0);
  bd.add_constraint(B == 2);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

  bool ok = (affine_dim == 0);

  return ok;
}

bool
test10() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(7);
  bd.add_constraint(A <= 1);
  bd.add_constraint(B == 2);
  bd.add_constraint(B - A <= -6);

  print_constraints(bd, "*** bd ***");

  dimension_type affine_dim = bd.affine_dimension();

  nout << endl
       << "The affine dimension of a system of `bd' "
       << endl
       << affine_dim
       << endl;

   bool ok = (affine_dim == 0);

  return ok;
}

bool
test11() {
  TBD_Shape bd(0, UNIVERSE);

  const dimension_type affine_dim = bd.affine_dimension();

  print_constraints(bd, "*** bd ***");

  return (affine_dim == 0);
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
END_MAIN


