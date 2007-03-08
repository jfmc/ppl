/* Test Box::refine(const Constraint&)
   and Box::refine(const Constraint_System&).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

bool
test01() {
  Variable A(0);
  Variable B(1);

  TBox box(2);
  box.add_constraint(A >= 0);

  print_constraints(box, "*** box.refine(A >= 0) ***");

  box.refine(B >= A);

  print_constraints(box, "*** box.refine(B >= A) ***");

  box.refine(11*A < 127);

  print_constraints(box, "*** box.refine(11*A < 127) ***");

  box.refine(7*A - 15*B > 8);

  print_constraints(box, "*** box.refine(7*A - 15*B > 8) ***");

  box.refine(3*B > 2*A);

  print_constraints(box, "*** box.refine(3*B > 2*A) ***");

  box.refine(A == B);

  print_constraints(box, "*** box.refine(A == B) ***");

#if 0
  Rational_Box known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B == 5);
  known_result.add_constraint(B - A <= 5);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(known_result, "*** known_result ***");
#endif
  bool ok = true;

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B >= A);
  cs.insert(11*A < 127);
  cs.insert(7*A - 15*B > 8);
  cs.insert(3*B > 2*A);
  cs.insert(A == B);

  print_constraints(cs, "*** cs ***");

  TBox box(2);
  box.refine(cs);

  print_constraints(box, "*** box.refine(cs) ***");

#if 0
  Rational_Box known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B == 5);
  known_result.add_constraint(B - A <= 5);

  bool ok = (Rational_Box(box1) == known_result);

  print_constraints(known_result, "*** known_result ***");
#endif
  bool ok = true;

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
