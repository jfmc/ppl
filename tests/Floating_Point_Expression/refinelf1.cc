/* Test Octagonal_Shape::refine_fp_interval_abstract_store and
   Octagonal_Shape::refine_with_linear_form_inequality
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

// tests trivial cases
bool
test01() {
  Variable A(0);
  Variable B(1);
  Octagonal_Shape<double> oc1(0);
  bool ok1 = false;
  Linear_Form<db_r_oc> l1(A);
  Linear_Form<db_r_oc> l2;
  try {
      oc1.refine_with_linear_form_inequality(l1, l2);
  }
  catch(std::invalid_argument e) {
    nout << "space_dim < left_space_dim" << endl;
    ok1 = true;
  }

  bool ok2 = false;
  try {
    oc1.refine_with_linear_form_inequality(l2, l1);
  }
  catch(std::invalid_argument e) {
    nout << "space_dim < right_space_dim" << endl;
    Octagonal_Shape<double> oc2(1);
    oc2.refine_with_linear_form_inequality(l1, l1);
    oc2.refine_with_linear_form_inequality(-l1, l1);
    oc2.refine_with_linear_form_inequality(l1, -l1);
    db_r_oc tmp;
    l1 -= A;
    l1 += tmp;
    l2 += tmp;
    oc2.refine_with_linear_form_inequality(l1, l2);
    Constraint_System cs(A < A);
    oc2.add_constraints(cs);
    oc2.refine_with_linear_form_inequality(l2, l1);
    ok2 = true;
  }

  return ok1 && ok2;
}

// tests [-1, 2] <= [-4, -1] + A and [-4, -1] + A <= [-1, 2]
bool
test02() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<float> oc1(3);
  oc1.add_constraint(A <= 2);
  oc1.add_constraint(A - B <= 3);
  oc1.add_constraint(B <= 2);
  fl_r_oc tmp(-1);
  tmp.join_assign(2);
  Linear_Form<fl_r_oc> l1(tmp);
  Linear_Form<fl_r_oc> l2(A);
  tmp.lower() = -4;
  tmp.upper() = -1;
  l2 += tmp;
  oc1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(oc1, "*** [-1, 2] <= [-4, -1] + A ***");

  Octagonal_Shape<float> known_result(oc1);
  known_result.add_constraint(-A <= 0);
  print_constraints(known_result, "*** known_result1 ***");

  bool ok1 = (oc1 == known_result);

  oc1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(oc1, "*** [-4, -1] + A <= [-1, 2] ***");

  known_result.add_constraint(A <= 6);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (oc1 == known_result);

  return ok1 && ok2;

}

// tests [3.5, 6] <= [-2.5, 0] - A and [-2.5, 0] - A <= [3.5, 6]
bool
test03() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<double> oc1(3);
  oc1.add_constraint(A <= 2);
  oc1.add_constraint(A - B <= 3);
  oc1.add_constraint(B <= 2);
  db_r_oc tmp(3.5);
  tmp.join_assign(6);
  Linear_Form<db_r_oc> l1(tmp);
  Linear_Form<db_r_oc> l2(-A);
  tmp.lower() = -2.5;
  tmp.upper() = 0;
  l2 += tmp;
  oc1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(oc1, "*** [3.5, 6] <= [-2.5, 0] - A ***");

  Octagonal_Shape<double> known_result(oc1);
  known_result.add_constraint(2*A <= -7);
  print_constraints(known_result, "*** known_result ***");

  bool ok1 = (oc1 == known_result);

  oc1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(oc1, "*** [-2.5, 0] - A <= [3.5, 6] ***");

  known_result.add_constraint(2*A >= -17);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (oc1 == known_result);

  return ok1 && ok2;

}

// tests [-0.5, 1] + A <= [2.5, 5] + B and [2.5, 5] + B <= [-0.5, 1] + A
bool
test04() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<double> oc1(3);
  oc1.add_constraint(A <= 2);
  oc1.add_constraint(A - B <= 3);
  oc1.add_constraint(B <= 2);
  db_r_oc tmp(-0.5);
  tmp.join_assign(1);
  Linear_Form<db_r_oc> l1(A);
  l1 += tmp;
  Linear_Form<db_r_oc> l2(B);
  tmp.lower() = 2.5;
  tmp.upper() = 5;
  l2 += tmp;
  oc1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(oc1, "*** [-0.5, 1] + A <= [2.5, 5] + B ***");

  Octagonal_Shape<double> known_result(oc1);
  known_result.add_constraint(2*A - 2*B <= 11);
  print_constraints(known_result, "*** known_result ***");

  bool ok1 = (oc1 == known_result);

  oc1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(oc1, "*** [2.5, 5] + B <= [-0.5, 1] + A ***");

  known_result.add_constraint(2*B - 2*A <= -3);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (oc1 == known_result);

  return ok1 && ok2;

}

// tests [1, 3] + A <= [4, 4] - B and [4, 4] - B <= [1, 3] + A
bool
test05() {
  Variable A(0);
  Variable B(1);

  Octagonal_Shape<float> oc1(3);
  oc1.add_constraint(A <= 2);
  oc1.add_constraint(A - B <= 3);
  oc1.add_constraint(B <= 2);
  fl_r_oc tmp(4);
  Linear_Form<fl_r_oc> l2(-B);
  l2 += tmp;
  Linear_Form<fl_r_oc> l1(A);
  tmp.lower() = 1;
  tmp.upper() = 3;
  l1 += tmp;
  oc1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(oc1, "*** [1, 3] + A <= [4, 4] - B ***");

  Octagonal_Shape<float> known_result(oc1);
  known_result.add_constraint(A + B <= 3);
  print_constraints(known_result, "*** known_result ***");

  bool ok1 = (oc1 == known_result);

  oc1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(oc1, "*** [4, 4] - B <= [1, 3] + A ***");

  known_result.add_constraint(-B - A <= -1);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (oc1 == known_result);

  return ok1 && ok2;

}

} //namespace

BEGIN_MAIN
/*DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04); */
  DO_TEST(test05);
END_MAIN
