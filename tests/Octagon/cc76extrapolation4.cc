/* Test Octagon::CC76_extrapolation_assign().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {

  TOctagon oct1(0);

  TOctagon oct2(0, EMPTY);

  Octagon<mpq_class> known_result(oct1);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct1.CC76_extrapolation_assign(oct2);

  bool ok = (oct1 == known_result);

#if NOISY
  print_constraints(oct1,
		    "*** oct1.CC76_extrapolation_assign(oct2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {

  TOctagon oct1(2, EMPTY);

  TOctagon oct2(2, EMPTY);

  Octagon<mpq_class> known_result(oct2);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct2.CC76_extrapolation_assign(oct1);

  bool ok = (oct2 == known_result);

#if NOISY
  print_constraints(oct2,
		    "*** oct2.CC76_extrapolation_assign(oct1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);

  TOctagon oct1(2);
  oct1.add_constraint(A - B >= -2);
  oct1.add_constraint(B - A >= 1);

  TOctagon oct2(2);
  oct2.add_constraint(A - B >= 2);
  oct2.add_constraint(B - A >= 1);

  Octagon<mpq_class> known_result(oct1);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct1.CC76_extrapolation_assign(oct2);

  bool ok = (oct1 == known_result);

#if NOISY
  print_constraints(oct1,
		    "*** oct1.CC76_extrapolation_assign(oct2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);

  TOctagon oct1(2);
  oct1.add_constraint(A <= 2);

  TOctagon oct2(2);
  oct2.add_constraint(A <= 1);
  oct2.add_constraint(B == -1);


  Octagon<mpq_class> known_result(2);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct1.CC76_extrapolation_assign(oct2);

  bool ok = (oct1 == known_result);

#if NOISY
  print_constraints(oct1,
		    "*** oct1.CC76_extrapolation_assign(oct2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oct1(3);
  oct1.add_constraint(C <= 4);
  oct1.add_constraint(B >= 2);

  TOctagon oct2(3);
  oct2.add_constraint(C <= 3);
  oct2.add_constraint(B >= 3);

  Octagon<mpq_class> known_result(3);
  known_result.add_constraint(B >= 1);
  known_result.add_constraint(C - B <= 2);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct1.CC76_extrapolation_assign(oct2);

  bool ok = (oct1 == known_result);

#if NOISY
  print_constraints(oct1,
		    "*** oct1.CC76_extrapolation_assign(oct2) ***");
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

  return 0;
}
CATCH
