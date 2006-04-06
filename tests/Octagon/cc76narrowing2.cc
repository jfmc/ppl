/* Test Octagon::CC76_narrowing_assign().
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

  TOctagon oc1(3, EMPTY);
  TOctagon oc2(3, EMPTY);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  TOctagon known_result(3, EMPTY);

  oc1.CC76_narrowing_assign(oc2);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.CC76_narrowing_assign(oc2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {

  TOctagon oc1(0);
  TOctagon oc2(0, EMPTY);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  TOctagon known_result(0);

  oc1.CC76_narrowing_assign(oc2);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.cc76_narrowing_assign(oc2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {

  TOctagon oc1(2);
  TOctagon oc2(2);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  TOctagon known_result(2);

  oc1.CC76_narrowing_assign(oc2);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.cc76_narrowing_assign(oc2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {

  TOctagon oc1(0, EMPTY);
  TOctagon oc2(0, EMPTY);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  TOctagon known_result(0, EMPTY);

  oc1.CC76_narrowing_assign(oc2);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.cc76_narrowing_assign(oc2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {

  TOctagon oc1(0);
  TOctagon oc2(0);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  TOctagon known_result(0);

  oc1.CC76_narrowing_assign(oc2);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.cc76_narrowing_assign(oc2) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test6() {
  Variable A(0);
  Variable B(1);

  TOctagon oc1(2);
  TOctagon oc2(2);

  oc1.add_constraint(A <= 3);
  oc1.add_constraint(A + B <= 5);

  oc2.add_constraint(A <= 2);
  oc2.add_constraint(B == 2);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  TOctagon known_result(2);
  known_result.add_constraint(A <= 3);
  known_result.add_constraint(A + B <= 5);
  known_result.add_constraint(A - B <= 0);
  known_result.add_constraint(B == 2);

  oc1.CC76_narrowing_assign(oc2);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.cc76_narrowing_assign(oc2) ***");
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

  return 0;
}
CATCH

