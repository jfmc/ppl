/* Test BD_Shape::BD_Shape(dimension_type, Degenerate_Element).
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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
  TBD_Shape bd(0, EMPTY);

  bool ok = (bd.OK() && bd.is_empty() && bd.space_dimension() == 0);

  print_constraints(bd, "*** bd ***");

  return ok;
}

bool
test02() {
  TBD_Shape bd(0, UNIVERSE);

  bool ok = (bd.OK() && bd.is_universe() && bd.space_dimension() == 0);

  print_constraints(bd, "*** bd ***");

  return ok;
}

bool
test03() {
  TBD_Shape bd(4, EMPTY);

  bool ok = (bd.OK() && bd.is_empty() && bd.space_dimension() == 4);

  print_constraints(bd, "*** bd ***");

  return ok;
}

bool
test04() {
  TBD_Shape bd(4, UNIVERSE);

  bool ok = (bd.OK() && bd.is_universe() && bd.space_dimension() == 4);

  print_constraints(bd, "*** bd ***");

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(4);
  bd.add_constraint(A <= -1);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(B >= 0);
  bd.add_constraint(C <= 2);
  bd.add_constraint(C >= 1);

  bool ok = (bd.OK() && bd.is_empty() && bd.space_dimension() == 4);

  print_constraints(bd, "*** bd ***");

  return ok;
}

bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A <= 1);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B <= 1);
  bd.add_constraint(B >= 0);
  bd.add_constraint(C <= 2);
  bd.add_constraint(C >= 1);

  bool ok = (bd.OK() && bd.space_dimension() == 3);

  print_constraints(bd, "*** bd ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
END_MAIN
