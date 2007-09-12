/* Test Box::refine(const Constraint_System&) with instances that may
   require a watchdog timer.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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
#include <sstream>

namespace {

bool
test01() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= -5);
  cs.insert(A <= 5);
  cs.insert(A == B);
  cs.insert(A == 2*B);
  print_constraints(cs, "*** cs ***");

  Rational_Box known_result(2);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B == 0);
  print_constraints(known_result, "*** known_result ***");

  Box<fl_r_oc> box(2);

  bool ok = false;

  box.refine(cs);

  ok = check_result(box, known_result, "5.61e-45", "2.81e-45", "1.41e-45");

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
