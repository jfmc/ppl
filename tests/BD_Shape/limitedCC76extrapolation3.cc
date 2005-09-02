/* Test BD_Shape::limited_CC76_extrapolation_assign().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(3, EMPTY);
  TBD_Shape bd2(3, EMPTY);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  TBD_Shape known_result(bd1);

  Constraint_System cs;
  cs.insert(x <= 1);
  cs.insert(y >= 4);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

#if NOISY
  print_constraints(cs, "*** cs ***");
  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2) ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x <= 1);
  cs1.insert(y >= 4);
  cs1.insert(x - y >= 2);

  TBD_Shape bd1(cs1);

  TBD_Shape bd2(2, EMPTY);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

 Constraint_System cs2;
 cs2.insert(x <= 0);
 cs2.insert(y >= 3);

 bd1.limited_CC76_extrapolation_assign(bd2, cs2);

 TBD_Shape known_result(bd1);

#if NOISY
  print_constraints(cs2, "*** cs2 ***");
  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs2) ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

int
main() TRY {
  test1();
  test2();

  return 0;
}
CATCH
