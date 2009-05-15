/* Test Box::wrap_assign().
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

bool
test01() {
  Variable x(0);
  Variable y(1);
  TBox box(2);
  box.add_constraint(100 <= x);
  box.add_constraint(x <= 300);
  box.add_constraint(-50 <= y);
  box.add_constraint(y <= 50);

  print_constraints(box, "*** box ***");

  Variables_Set vars(x, y);

  box.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS);

  // FIXME.
  TBox known_result(2);
  known_result.refine_with_constraint(0 <= x);
  known_result.refine_with_constraint(x <= 255);
  // known_result.refine_with_constraint(x < 256);
  known_result.refine_with_constraint(0 <= y);
  known_result.refine_with_constraint(y <= 255);
  // known_result.refine_with_constraint(y < 256);

  bool ok = (box == known_result);

  print_constraints(box, "*** box.wrap_assign(...) ***");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);
  TBox box(2);
  box.add_constraint(100 <= x);
  box.add_constraint(x <= 300);
  box.add_constraint(-50 <= y);
  box.add_constraint(y <= 50);

  print_constraints(box, "*** box ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x >= y);
  cs.insert(y <= 75);

  box.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs);

  // FIXME.
  TBox known_result(2);
  known_result.refine_with_constraint(0 <= x);
  known_result.refine_with_constraint(x <= 255);
  // known_result.refine_with_constraint(x < 256);
  known_result.refine_with_constraint(0 <= y);
  known_result.refine_with_constraint(y <= 50);

  bool ok = (box == known_result);

  print_constraints(box, "*** box.wrap_assign(...) ***");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);
  TBox box(2);
  box.add_constraint(100 <= x);
  box.add_constraint(x <= 300);
  box.add_constraint(-50 <= y);
  box.add_constraint(y <= 50);

  print_constraints(box, "*** box ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x + y >= 50);
  cs.insert(x >= y);
  cs.insert(y <= 75);

  box.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs);

  // FIXME.
  TBox known_result(2);
  known_result.refine_with_constraint(6 <= x);
  // known_result.refine_with_constraint(0 <= x);
  known_result.refine_with_constraint(x <= 255);
  // known_result.refine_with_constraint(x < 256);
  known_result.refine_with_constraint(0 <= y);
  known_result.refine_with_constraint(y <= 50);

  bool ok = (box == known_result);

  print_constraints(box, "*** box.wrap_assign(...) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST_F8(test01);
  DO_TEST_F8(test02);
  DO_TEST_F8(test03);
END_MAIN
