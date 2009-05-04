/* Test Polyhedron::wrap_assign().
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
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(x + 24*y >= 3072);
  known_result.add_constraint(193*x + 504*y <= 129792);
  known_result.add_constraint(x - 8*y >= -1280);
  known_result.add_constraint(x - 8*y <= -768);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test02() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= y);
  known_result.add_constraint(x - 8*y >= -1280);
  known_result.add_constraint(x - 8*y <= -1024);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test03() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_UNDEFINED);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(y >= 120);
  known_result.add_constraint(y <= 184);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test04() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_UNDEFINED, &cs);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y >= 120);
  known_result.add_constraint(y <= 184);
  known_result.add_constraint(x <= y);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_IMPOSSIBLE);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(x + 1024 == 8*y);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test06() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_IMPOSSIBLE, &cs);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(7*x <= 1024);
  known_result.add_constraint(x + 1024 == 8*y);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test07() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(y <= 255);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test08() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y <= 255);
  known_result.add_constraint(x <= y);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test09() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, 0, 32, false);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(x + 24*y >= 3072);
  known_result.add_constraint(193*x + 504*y <= 129792);
  known_result.add_constraint(x - 8*y >= -1280);
  known_result.add_constraint(x - 8*y <= -768);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test10() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs, 32, false);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= y);
  known_result.add_constraint(x - 8*y >= -1280);
  known_result.add_constraint(x - 8*y <= -1024);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test11() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, 0, 32, false);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(y >= 0);
  known_result.add_constraint(y <= 255);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test12() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x + 1024 == 8*y);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs, 32, false);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y <= 255);
  known_result.add_constraint(x <= y);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test13() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x >= 224);
  ph.add_constraint(x-y <= 160);
  ph.add_constraint(x+y <= 416);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(x <= 255);
  known_result.add_constraint(+x-y <= 160);
  known_result.add_constraint(+x+y <= 416);
  known_result.add_constraint(x + 7*y >= 672);
  known_result.add_constraint(x - 7*y >= -1120);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

bool
test14() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x >= 224);
  ph.add_constraint(x-y <= 160);
  ph.add_constraint(x+y <= 416);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  Constraint_System cs;
  cs.insert(x <= y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, &cs);

  C_Polyhedron known_result(2);
  known_result.add_constraint(x >= 0);
  known_result.add_constraint(y >= 96);
  known_result.add_constraint(-x+y >= 96);
  known_result.add_constraint(+x+y <= 160);

  bool ok = (ph == known_result);

  print_constraints(ph, "*** ph.wrap_assign(...) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST_F16(test01);
  DO_TEST_F8(test02);
  DO_TEST_F8(test03);
  DO_TEST_F8(test04);
  DO_TEST_F16(test05);
  DO_TEST_F16(test06);
  DO_TEST_F16(test07);
  DO_TEST_F8(test08);
  DO_TEST_F8(test09);
  DO_TEST_F16(test10);
  DO_TEST_F8(test11);
  DO_TEST_F8(test12);
  DO_TEST_F16(test13);
  DO_TEST_F8(test14);
END_MAIN
