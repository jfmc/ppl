/* Test BD_Shape::limited_CC76_extrapolation_assign().
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

  TBD_Shape bd1(3, EMPTY);
  TBD_Shape bd2(3, EMPTY);

  Constraint_System cs;
  cs.insert(x <= 1);
  cs.insert(y >= 4);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs1;
  cs1.insert(x <= 1);
  cs1.insert(y >= 4);
  cs1.insert(x - y >= 2);

  TBD_Shape bd1(cs1);
  TBD_Shape bd2(2, EMPTY);

  Constraint_System cs2;
  cs2.insert(x <= 0);
  cs2.insert(y >= 3);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs2, "*** cs2 ***");

  BD_Shape<mpq_class> known_result(bd1);

  bd1.limited_CC76_extrapolation_assign(bd2, cs2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.limited_CC76_extrapolation_assign(bd2, cs2) ***");

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {
  DO_TEST(test1);
  DO_TEST(test2);

  return 0;
}
CATCH
