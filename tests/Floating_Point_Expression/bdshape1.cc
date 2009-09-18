/* Test BD_Shape::affine_image on interval linear forms.
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

// tests space_dimensions and trivial cases
bool
test01() {
  Variable A(0);
  Variable B(1);
  BD_Shape<double> bd1(0);
  bool ok1 = false;
  Linear_Form<db_r_oc> l(A);

  try {
      bd1.affine_image(A, l);
  }
  catch(std::invalid_argument e) {
    nout << "bd1_space_dim < lf_space_dim" << endl;
    ok1 = true;
  }

  bool ok2 = false;
  BD_Shape<double> bd2(1);

  try {
    bd2.affine_image(B, l);
  }
  catch(std::invalid_argument e) {
    nout << "space_dim < var_id + 1" << endl;
    bd2.affine_image(A, l);
    Constraint_System cs(A < A);
    bd2.add_constraints(cs);
    bd2.affine_image(A, l);
    ok2 = true;
  }

  return ok1 && ok2;
}


// tests affine_image(A, [-2, 1])
// FIXME: It's a preliminary version, not sound at the moment.
bool
test02() {
  Variable A(0);
  Variable B(1);

  BD_Shape<float> bd1(3);
  bd1.add_constraint(A <= 2);
  bd1.add_constraint(A - B <= 3);
  bd1.add_constraint(B <= 2);
  fl_r_oc free_term(-2);
  free_term.join_assign(1);
  Linear_Form<fl_r_oc> l(free_term);
  bd1.affine_image(A, l);
  print_constraints(bd1, "*** bd1.affine_image(A, [-2, 1]) ***");

  // At the moment, affine_image is simply an identity function.

  BD_Shape<float> known_result(3);
  known_result.add_constraint(A <= 2);
  known_result.add_constraint(B <= 2);
  known_result.add_constraint(A - B <= 3);
  print_constraints(known_result, "*** known_result ***");

  bool ok = (bd1 == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
