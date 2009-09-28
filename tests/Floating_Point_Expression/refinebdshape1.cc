/* Test BD_Shape::refine_wiht_linear_form_inequaity on interval linear forms.
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
  FP_BD_Shape bd1(0);
  bool ok1 = false;
  FP_Linear_Form l1(A);
  FP_Linear_Form l2(B);

  try {
    bd1.refine_with_linear_form_inequality(l1,l2);
  }
  catch(std::invalid_argument e) {
    nout << "bd1_space_dim < lf_space_dim" << endl;
    ok1 = true;
  }

  bool ok2 = false;
  FP_BD_Shape bd2(1);

  try {
    bd2.refine_with_linear_form_inequality(l1,l2);
  }
  catch(std::invalid_argument e) {
    nout << "space_dim < var_id + 1" << endl;
    oc2.refine_with_linear_form_inequality(l1, l1);
    oc2.refine_with_linear_form_inequality(-l1, l1);
    oc2.refine_with_linear_form_inequality(l1, -l1);
    FP_Interval tmp(0);
    l1 -= A;
    l1 += tmp;
    l2 += tmp;
    oc2.refine_with_linear_form_inequality(l1, l2);
    ok2 = true;
  }

  return ok1 && ok2;
}

// tests [-1, 2] <= [-4, -1] + A and [-4, -1] + A <= [-1, 2]
bool
test02() {
  Variable A(0);
  Variable B(1);

  FP_BD_Shape oc1(2);
  oc1.add_constraint(A <= 2);
  oc1.add_constraint(A - B <= 3);
  oc1.add_constraint(B <= 2);
  FP_BD_Shape known_result(oc1);
  FP_Interval tmp(-1);
  tmp.join_assign(2);
  FP_Linear_Form l1(tmp);
  FP_Linear_Form l2(A);
  tmp.lower() = -4;
  tmp.upper() = -1;
  l2 += tmp;
  oc1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(oc1, "*** [-1, 2] <= [-4, -1] + A ***");

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

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
