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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(z >= 3);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(3);
  known_result.add_constraint(x >= -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(z >= 3);

  bd.affine_image(x, -x);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -x) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(z >= 3);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(3);
  known_result.add_constraint(x <= -3);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(z >= 3);

  bd.affine_image(x, -z);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -z) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= 1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 0);
  known_result.add_constraint(x >= -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= 1);
  
  bd.affine_image(x, -y + 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -y + 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= 1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(2*x - 2*y == -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= 1);
  
  bd.affine_image(x, -2*y + 1, -2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -2*y + 1, -2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= 1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 0);
  known_result.add_constraint(x >= -2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= 1);

  bd.affine_image(x, -2*y + 1, 2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -2*y + 1, 2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test6() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 5);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, 2*x + y + 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, 2*x + y + 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test7() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 3);
  known_result.add_constraint(x >= -2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, -2*x + y + 1);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -2*x + y + 1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test8() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(x >= -1);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, 2*x - 3*y + 1, 5);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, 2*x - 3*y + 1, 5) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test9() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(x >= -2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, -2*x - 3*y + 1, 5);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, -2*x - 3*y + 1, 5) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test10() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 1);
  bd.add_constraint(x >= 0);
  bd.add_constraint(y <= 2);
  bd.add_constraint(y >= -1);


#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(x >= -2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y >= -1);

  bd.affine_image(x, 2*x - 3*y + 1, -5);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd.affine_image(x, 2*x - 3*y + 1, -5) ***");
#endif

  if (!ok)
    exit(1);
}

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

  return 0;

}
CATCH
