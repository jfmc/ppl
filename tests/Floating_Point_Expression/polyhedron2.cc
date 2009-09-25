/* Test Polyhedron::refine_fp_interval_abstract_store and
   Polyhedron::refine_with_linear_form_inequality.
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

bool
test01() {
  C_Polyhedron pol(1);
  FP_Interval_Abstract_Store store(1);
  store.set_interval(Variable(0), FP_Interval(0));
  try {
    FP_Linear_Form l1(Variable(3));
    FP_Linear_Form l2;
    pol.refine_with_linear_form_inequality(l1, l2, store);
  }
  catch (std::invalid_argument) {
    try {
      FP_Linear_Form l1;
      FP_Linear_Form l2(Variable(3));
      pol.refine_with_linear_form_inequality(l1, l2, store);
    }
    catch (std::invalid_argument) {
      return true;
    }
  }

  return false;
}

bool
test02() {
  C_Polyhedron result(1);
  FP_Interval_Abstract_Store store(1);
  store.set_interval(Variable(0), FP_Interval(1.5));
  FP_Interval interval(57);
  FP_Linear_Form lf1(Variable(0));
  FP_Linear_Form lf2(interval);
  result.refine_with_linear_form_inequality(lf1, lf2, store);
  print_constraints(result, "RESULT");
  C_Polyhedron known_result(1);
  known_result.refine_with_constraint(Variable(0) <= 57);
  print_constraints(known_result, "KNOWN RESULT");
  return result == known_result;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(A, FP_Interval(1));
  store.set_interval(B, FP_Interval(2));

  C_Polyhedron result(2);
  result.refine_with_linear_form_inequality(-FP_Linear_Form(A),
				 FP_Linear_Form(FP_Interval(0)), store);
  result.refine_with_linear_form_inequality(FP_Linear_Form(A),
				 FP_Linear_Form(FP_Interval(2)), store);
  result.refine_with_linear_form_inequality(-FP_Linear_Form(A),
				 FP_Linear_Form(FP_Interval(1)), store);
  result.refine_with_linear_form_inequality(-FP_Linear_Form(B),
				 FP_Linear_Form(FP_Interval(-1)), store);
  print_constraints(result, "RESULT");

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 2);
  known_result.add_constraint(B >= 1);
  print_constraints(known_result, "KNOWN RESULT");

  return result == known_result;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
END_MAIN
