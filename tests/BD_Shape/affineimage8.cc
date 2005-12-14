/* Test BD_Shape::affine_image().
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
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(z >= 3);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(3);
  known_result.add_constraint(x >= -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(z >= 3);

  bd.affine_image(x, -x);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -x) ***");

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

  TBD_Shape known_result(3);
  known_result.add_constraint(x <= -3);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(z >= 3);

  bd.affine_image(x, -z);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -z) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= 1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 0);
  known_result.add_constraint(x >= -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= 1);

  bd.affine_image(x, -y + 1);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -y + 1) ***");

  if (!ok)
    exit(1);
}

void
test4() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= 1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(2*x - 2*y == -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= 1);

  bd.affine_image(x, -2*y + 1, -2);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -2*y + 1, -2) ***");

  if (!ok)
    exit(1);
}

void
test5() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= 1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(2*x <= -1);
  known_result.add_constraint(2*x >= -3);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= 1);

  bd.affine_image(x, -2*y + 1, 2);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -2*y + 1, 2) ***");

  if (!ok)
    exit(1);
}

void
test6() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 5);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);
  known_result.add_constraint(x - y <= 3);
  known_result.add_constraint(x - y >= 1);

  bd.affine_image(x, 2*x + y + 1);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, 2*x + y + 1) ***");

  if (!ok)
    exit(1);
}

void
test7() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 3);
  known_result.add_constraint(x >= -2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);
  known_result.add_constraint(x - y <= 1);
  known_result.add_constraint(x - y >= -1);

  bd.affine_image(x, -2*x + y + 1);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -2*x + y + 1) ***");

  if (!ok)
    exit(1);
}

void
test8() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(5*x <= 6);
  known_result.add_constraint(x >= -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, 2*x - 3*y + 1, 5);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, 2*x - 3*y + 1, 5) ***");

  if (!ok)
    exit(1);
}

void
test9() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);

  print_constraints(bd, "*** bd ***");

  TBD_Shape known_result(2);
  known_result.add_constraint(5*x <= 4);
  known_result.add_constraint(5*x >= -7);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, -2*x - 3*y + 1, 5);

  bool ok = (bd == known_result);

  print_constraints(bd, "*** bd.affine_image(x, -2*x - 3*y + 1, 5) ***");

  if (!ok)
    exit(1);
}

void
test10() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);

  print_constraints(bd, "*** bd ***");

  bd.affine_image(x, 2*x - 3*y + 1, -5);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(5*x >= -6);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);
  known_result.add_constraint(5*x - 5*y <= 1);
  known_result.add_constraint(5*x - 5*y >= -7);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd, "*** bd.affine_image(x, 2*x - 3*y + 1, -5) ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 1);
  }

  if (!ok)
    exit(1);
}

void
test11() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);

  bd.add_constraint(y >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(z <= 3);

  print_constraints(bd, "*** bd ***");

  bd.affine_image(x, y + 5*z, 3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(3*x <= 17);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(z <= 3);
  known_result.add_constraint(x - y <= 5);
  known_result.add_constraint(3*x - 3*z <= 8);

  TBD_Shape T_known_result(known_result);
  bool ok = bd.contains(T_known_result);

  print_constraints(bd, "*** bd.affine_image(x, y + 5*z, 3) ***");

  if (ok) {
    Checked_Number<mpq_class, Extended_Number_Policy> distance;
    rectilinear_distance_assign(distance, T_known_result, bd, ROUND_UP);

    nout << "Rectilinear distance = " << distance << endl;

    ok = (distance <= 1);
  }

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
  test9();
  test10();
  test11();

  return 0;

}
CATCH
