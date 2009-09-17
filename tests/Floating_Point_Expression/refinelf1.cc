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

// tests [-1, 2] <= [-4, -1] + A
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
  print_constraints(known_result, "*** known_result ***");

  bool ok = (oc1 == known_result);

  return ok;

}

} //namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
