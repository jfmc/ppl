/* Test BD_Shape::operator==().
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

bool
test01() {
  Variable x(0);
  Variable y(1);
  // Variable z(2);

  TBD_Shape bd1(3);
  TBD_Shape bd2(2);

  bd1.add_constraint(x <= 3);
  bd1.add_constraint(x - y <= 4);

  bd2.add_constraint(x - y <= 5);
  bd2.add_constraint(-y <= -2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bool known_result = false;
  bool ok = ((bd1 == bd2) == known_result);

  return ok;
}

bool
test02() {
  TBD_Shape bd1(0, EMPTY);
  TBD_Shape bd2(0);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bool known_result = false;
  bool ok = ((bd1 == bd2) == known_result);

  return ok;
}

bool
test03() {
  TBD_Shape bd1(0);
  TBD_Shape bd2(0, EMPTY);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bool known_result = false;
  bool ok = ((bd1 == bd2) == known_result);

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A - B <= 1);
  bd1.add_constraint(B - A <= -2);

  TBD_Shape bd2(2);
  bd2.add_constraint(A - B <= 1);
  bd2.add_constraint(B - A <= 2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bool known_result = false;
  bool ok = ((bd1 == bd2) == known_result);

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A - B <= 1);
  bd1.add_constraint(B - A <= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(A - B <= 1);
  bd2.add_constraint(B - A <= -2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bool known_result = false;
  bool ok = ((bd1 == bd2) == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
END_MAIN
