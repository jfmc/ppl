/* Test the termination analysis facilities of the PPL.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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
  C_Polyhedron ph1(2);
  C_Polyhedron ph2(4);
  bool ok1 = !termination_test_MS(ph1) && !termination_test_PR(ph1);
  bool ok2 = !termination_test_MS_2(ph1, ph2)
    && !termination_test_PR_2(ph1, ph2);
  return ok1 && ok2;
}

bool
test02() {
  C_Polyhedron ph1(2, EMPTY);
  C_Polyhedron ph2(4, EMPTY);
  bool ok1 = termination_test_MS(ph1) && termination_test_PR(ph1);
  bool ok2 = termination_test_MS_2(ph1, ph2)
    && termination_test_PR_2(ph1, ph2);
  return ok1 && ok2;
}

bool
test03() {
  C_Polyhedron ph1(2);
  C_Polyhedron ph2(4, EMPTY);
  bool ok = termination_test_MS_2(ph1, ph2)
              && termination_test_PR_2(ph1, ph2);
  return ok;
}

bool
test04() {
  C_Polyhedron ph1(2, EMPTY);
  C_Polyhedron ph2(4);
  bool ok = termination_test_PR_2(ph1, ph2);
  ok = ok && termination_test_MS_2(ph1, ph2);
  return ok;
}

bool
test05() {
  C_Polyhedron ph1(2);
  C_Polyhedron ph2(4);
  Generator witness(point());
  bool ok = !one_affine_ranking_function_MS(ph1, witness);
  ok = ok && !one_affine_ranking_function_PR(ph1, witness);
  ok = ok && !one_affine_ranking_function_MS_2(ph1, ph2, witness);
  ok = ok && !one_affine_ranking_function_PR_2(ph1, ph2, witness);
  return ok;
}

bool
test06() {
  C_Polyhedron ph1(2, EMPTY);
  C_Polyhedron ph2(4, EMPTY);
  Generator witness(point());
  bool ok = one_affine_ranking_function_MS(ph1, witness);
  ok = ok && one_affine_ranking_function_PR(ph1, witness);
  ok = ok && one_affine_ranking_function_MS_2(ph1, ph2, witness);
  ok = ok && one_affine_ranking_function_PR_2(ph1, ph2, witness);
  return ok;
}

bool
test07() {
  C_Polyhedron ph1(2, EMPTY);
  C_Polyhedron ph2(4);
  Generator witness(point());
  bool ok = one_affine_ranking_function_PR_2(ph1, ph2, witness);
  ok = ok && one_affine_ranking_function_MS_2(ph1, ph2, witness);
  return ok;
}

bool
test08() {
  C_Polyhedron ph1(2);
  C_Polyhedron ph2(4, EMPTY);
  Generator witness(point());
  bool ok = one_affine_ranking_function_PR_2(ph1, ph2, witness);
  ok = ok && one_affine_ranking_function_MS_2(ph1, ph2, witness);
  return ok;
}

bool
test09() {
  C_Polyhedron ph1(2);
  C_Polyhedron ph2(4);
  C_Polyhedron c_mu_space;
  NNC_Polyhedron nnc_mu_space;
  all_affine_ranking_functions_MS(ph1, c_mu_space);
  all_affine_ranking_functions_MS_2(ph1, ph2, c_mu_space);
  all_affine_ranking_functions_PR(ph1, nnc_mu_space);
  all_affine_ranking_functions_PR_2(ph1, ph2, nnc_mu_space);

  C_Polyhedron c_known_result(3, EMPTY);
  NNC_Polyhedron nnc_known_result(3, EMPTY);

  print_constraints(ph1, "*** ph ***");
  print_constraints(c_mu_space, "*** c_mu_space ***");
  print_constraints(nnc_mu_space, "*** nnc_mu_space ***");
  return ph1.OK() && (nnc_mu_space == nnc_known_result);
}

bool
test10() {
  C_Polyhedron ph1(2, EMPTY);
  C_Polyhedron ph2(4, EMPTY);
  C_Polyhedron c_mu_space;
  NNC_Polyhedron nnc_mu_space;
  all_affine_ranking_functions_MS(ph1, c_mu_space);
  all_affine_ranking_functions_MS_2(ph1, ph2, c_mu_space);
  all_affine_ranking_functions_PR(ph1, nnc_mu_space);
  all_affine_ranking_functions_PR_2(ph1, ph2, nnc_mu_space);

  C_Polyhedron c_known_result(2, EMPTY);
  c_known_result.add_generator(point());
  NNC_Polyhedron nnc_known_result(3, EMPTY);
  nnc_known_result.add_generator(point());

  print_constraints(ph1, "*** ph ***");
  print_constraints(c_mu_space, "*** c_mu_space ***");
  print_constraints(nnc_known_result, "*** nnc_known_result ***");
  print_constraints(nnc_mu_space, "*** nnc_mu_space ***");

  return ph1.OK() && (nnc_mu_space == nnc_known_result);
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
  DO_TEST_F(test10);
END_MAIN
