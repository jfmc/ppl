/* Test Octagonal_Shape::affine_image on interval linear forms.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

// tests affine_image(A, [-2, 1])
bool
test01() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<float> oc1(3);
  oc1.add_constraint(A <= 2);
  oc1.add_constraint(A - B <= 3);
  oc1.add_constraint(B <= 2);
  Octagonal_Shape<float> oc2(3);

  fl_r_oc free_term(-2);
  free_term.join_assign(1);
  Linear_Form<fl_r_oc> l(free_term);
  oc1.affine_image(A, l);
  print_constraints(oc1, "*** oc1.affine_image(A, [-2, 1]) ***");

  Octagonal_Shape<float> known_result(3);
  known_result.add_constraint(A >= -2);
  known_result.add_constraint(B <= 2);
  known_result.add_constraint(A <= 1);
  print_constraints(known_result, "*** known_result ***");

  bool ok = (oc1 == known_result);

  return ok;
}

/*
// tests affine_image(A, [-1, -1]*A)
bool test02() {
  return true;
}

// tests affine_image(A, [-1, 1]*B)
bool test03() {
  return true;
}

// tests affine_image(A, i + i1*A + i2*B)
bool test04() {
  return true;
}
*/

} //namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
