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
  FP_BD_Shape bd1(0);
  bool ok1 = false;
  FP_Linear_Form l1(A);
  FP_Linear_Form l2;
  
  try{
    bd1.refine_with_linear_form_inequality(l1,l2);
    std::cout <<"no eccezione" <<std::endl;
  }
  catch(std::invalid_argument e) {
    ok1 = true;
  }
  
  
  bool ok2 = false;
  try{
    bd1.refine_with_linear_form_inequality(l2,l1);
    std::cout <<"no eccezione" <<std::endl;
  }
  catch(std::invalid_argument e){
    FP_BD_Shape bd2(1);
    bd2.refine_with_linear_form_inequality(l1, l1);
    bd2.refine_with_linear_form_inequality(-l1, l1);
    bd2.refine_with_linear_form_inequality(l1, -l1);
    FP_Interval tmp(0);
    l1 -= A;
    l1 += tmp;
    l2 += tmp;
    bd2.refine_with_linear_form_inequality(l1, l2);
    ok2 = true;
  }
  
  return ok1 && ok2;
}

// tests [-1, 2] <= [-4, -1] + A and [-4, -1] + A <= [-1, 2]
bool
test02() {
  Variable A(0);
  Variable B(1);

  FP_BD_Shape bd1(2);
  bd1.add_constraint(A <= 2);
  bd1.add_constraint(A - B <= 3);
  bd1.add_constraint(B <= 2);
  FP_BD_Shape known_result(bd1);
  FP_Interval tmp(-1);
  tmp.join_assign(2);
  FP_Linear_Form l1(tmp);
  FP_Linear_Form l2(A);
  tmp.lower() = -4;
  tmp.upper() = -1;
  l2 += tmp;
  bd1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(bd1, "*** [-1, 2] <= [-4, -1] + A ***");

  known_result.add_constraint(-A <= 0);
  print_constraints(known_result, "*** known_result1 ***");

  bool ok1 = (bd1 == known_result);

  bd1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(bd1, "*** [-4, -1] + A <= [-1, 2] ***");

  known_result.add_constraint(A <= 6);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (bd1 == known_result);

  return ok1 && ok2;

}

// tests [3.5, 6] <= [-2.5, 0] - A and [-2.5, 0] - A <= [3.5, 6]
bool
test03() {
  Variable A(0);
  Variable B(1);

  FP_Octagonal_Shape bd1(2);
  bd1.add_constraint(A <= 2);
  bd1.add_constraint(A - B <= 3);
  bd1.add_constraint(B <= 2);
  FP_Octagonal_Shape known_result(bd1);
  FP_Interval tmp(3.5);
  tmp.join_assign(6);
  FP_Linear_Form l1(tmp);
  FP_Linear_Form l2(-A);
  tmp.lower() = -2.5;
  tmp.upper() = 0;
  l2 += tmp;
  bd1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(bd1, "*** [3.5, 6] <= [-2.5, 0] - A ***");

  known_result.add_constraint(2*A <= -7);
  print_constraints(known_result, "*** known_result ***");

  bool ok1 = (bd1 == known_result);

  bd1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(bd1, "*** [-2.5, 0] - A <= [3.5, 6] ***");

  known_result.add_constraint(2*(-A) <= 17);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (bd1 == known_result);

  return ok1 && ok2;

}

// tests [-0.5, 1] + A <= [2.5, 5] + B and [2.5, 5] + B <= [-0.5, 1] + A
bool
test04() {
  Variable A(0);
  Variable B(1);

  FP_BD_Shape bd1(2);
  bd1.add_constraint(A <= 2);
  bd1.add_constraint(A - B <= 3);
  bd1.add_constraint(B <= 2);
  FP_BD_Shape known_result(bd1);
  FP_Interval tmp(-0.5);
  tmp.join_assign(1);
  FP_Linear_Form l1(A);
  l1 += tmp;
  FP_Linear_Form l2(B);
  tmp.lower() = 2.5;
  tmp.upper() = 5;
  l2 += tmp;
  bd1.refine_with_linear_form_inequality(l1, l2);
  print_constraints(bd1, "*** [-0.5, 1] + A <= [2.5, 5] + B ***");

  known_result.add_constraint(2*A - 2*B <= 11);
  print_constraints(known_result, "*** known_result ***");

  bool ok1 = (bd1 == known_result);

  bd1.refine_with_linear_form_inequality(l2, l1);
  print_constraints(bd1, "*** [2.5, 5] + B <= [-0.5, 1] + A ***");

  known_result.add_constraint((2*B) - (2*A) <= -3);
  print_constraints(known_result, "*** known_result2 ***");

  bool ok2 = (bd1 == known_result);

  return ok1 && ok2;

}

} // namespace

BEGIN_MAIN
//DO_TEST(test01);
//DO_TEST(test02);
//DO_TEST(test03);
  DO_TEST(test04);
END_MAIN
