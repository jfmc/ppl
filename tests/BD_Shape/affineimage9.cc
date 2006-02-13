/* Test BD_Shape::affine_image().
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
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);

  print_constraints(bd, "*** bd ***");

  bd.affine_image(x, -2*x - 3*y + 1, -5);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(5*x >= -4);
  known_result.add_constraint(5*x <= 7);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);
  known_result.add_constraint(y - x <= 1);
  known_result.add_constraint(5*x - 5*y <= 3);

  bool ok = check_result(bd, known_result, "3.70e-7", "2.10e-7", "1.44e-7");

  print_constraints(bd, "*** bd.affine_image(x, -2*x - 3*y + 1, -5) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);
  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(z >= 3);

  print_constraints(bd, "*** bd ***");

  bd.affine_image(z, x + 2*y -3*z + 2, 4);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(2*z <= -1);

  bool ok = check_result(bd, known_result);

  print_constraints(bd, "*** bd.affine_image(z, x + 2*y -3*z + 2, 4) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd(4);
  bd.add_constraint(A <= 1);
  bd.add_constraint(B <= 2);
  bd.add_constraint(B >= 1);
  bd.add_constraint(C <= 0);
  bd.add_constraint(D == 3);

  print_constraints(bd, "*** bd ***");

  bd.affine_image(A, -B + 2*C + 1, -3);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B <= 2);
  known_result.add_constraint(B >= 1);
  known_result.add_constraint(C <= 0);
  known_result.add_constraint(D == 3);
  known_result.add_constraint(3*B - 3*A <= 5);

  bool ok = check_result(bd, known_result, "7.95e-8", "7.95e-8", "7.95e-8");

  print_constraints(bd, "*** bd.affine_image(A, -B + 2*C + 1, -3) ***");

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
