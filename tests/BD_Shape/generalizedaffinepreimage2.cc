/* Test BD_Shape::generalized_affine_preimage().
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

void
test1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B >= 5);
  bd.add_constraint(3*C <= 7);
  bd.add_constraint(5*C >= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, EQUAL, 3*A+2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(2*A == 1);
  known_result.add_constraint(3*C <= 7);
  known_result.add_constraint(5*C >= 7);

  bool ok = check_result(bd, known_result, "3.66e-7", "2.28e-7", "1.59e-7");

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "EQUAL, -1) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B <= 5);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, GREATER_THAN_OR_EQUAL,
				 Linear_Expression(-1));

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(2*A == 1);

  bool ok = check_result(bd, known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "GREATER_THAN_OR_EQUAL, -1) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(2*A == 1);
  bd.add_constraint(B <= 5);
  bd.add_constraint(3*C <= 8);
  bd.add_constraint(2*C >= 7);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_preimage(B, EQUAL, 3*A+2);

  BD_Shape<mpq_class> known_result(3, EMPTY);

  bool ok = check_result(bd,  known_result);

  print_constraints(bd,
		    "*** bd.generalized_affine_preimage(B, "
		    "EQUAL, 3*A+2) ***");

  if (!ok)
    exit(1);
}

} // namespace



int
main() TRY {

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);

  return 0;
}
CATCH
