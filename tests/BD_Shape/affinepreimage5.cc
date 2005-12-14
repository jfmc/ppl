/* Test BD_Shape::affine_preimage().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 4*B + 6*C + 2, -2);

  TBD_Shape known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 4*B + 6*C + 2, -2) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 2*A + 3*C + 2, 2);

  TBD_Shape known_result(3);
  known_result.add_constraint(A <= -2);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 2*A + 3*C + 2, 2) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 2*A + 3*C + 2, 2);

  TBD_Shape known_result(3);
  known_result.add_constraint(A <= -2);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, -3*A + C - 1, 2) ***");

  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 3*A + C - 1, -2);

  TBD_Shape known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 3*A + C - 1, -2) ***");

  if (!ok)
    exit(1);
}

void
test5() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, -3*A + C - 1, -2);

  TBD_Shape known_result(3);
  known_result.add_constraint(B <= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, -3*A + C - 1, -2) ***");

  if (!ok)
    exit(1);
}

void
test6() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd(4);
  bd.add_constraint(A == 2);
  bd.add_constraint(B == 0);
  bd.add_constraint(C >= -3);
  bd.add_constraint(D <= 5);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(D, 4*A - B + 2*C + 5*D - 1, 3);

  TBD_Shape known_result(4);
  known_result.add_constraint(A == 2);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C >= -3);
  known_result.add_constraint(5*D <= 14);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage"
		        "(D, 4*A - B + 2*C + 5*D - 1, 3) ***");

  if (!ok)
    exit(1);
}

void
test7() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(B, -B);

  TBD_Shape known_result(3);
  known_result.add_constraint(A <= -1);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(C >= 0);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(B, -B) ***");

  if (!ok)
    exit(1);
}

void
test8() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= -1);
  bd.add_constraint(B <= 0);
  bd.add_constraint(C >= 0);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(bd);

  bd.affine_preimage(B, -B, -1);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_preimage(B, -B, -1) ***");

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();

  return 0;
}
CATCH

