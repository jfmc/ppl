/* Test Box::Box(const Direct_Product<D1, D2>&, Complexity_Class).
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

// shrink_bounding_box(box)
bool
test01() {
  Variable A(0);

  Rational_Box box(1);
  box.raise_lower_bound(0, true, 2, 3);
  box.lower_upper_bound(0, true, 6, 1);

  Direct_Product<Grid, NNC_Polyhedron> dp(1);
  dp.add_constraint(A <= 4);
  dp.add_constraint(A >= 2);
  dp.add_congruence(A %= 0);

  dp.shrink_bounding_box(box);

  Rational_Box known_box(1);
  known_box.raise_lower_bound(0, true, 2, 1);
  known_box.lower_upper_bound(0, true, 4, 1);

  bool ok = (box == known_box);

  return ok;
}

// shrink_bounding_box(box), shrink to intersection.
bool
test02() {
  Variable A(0);

  Rational_Box box(1);
  box.raise_lower_bound(0, true, 2, 3);
  box.lower_upper_bound(0, true, 6, 1);

  Direct_Product<Grid, NNC_Polyhedron> dp(1);
  dp.add_constraint(A <= 4);
  dp.add_constraint(A >= 2);
  dp.add_congruence((A %= 0) / 3);

  dp.shrink_bounding_box(box);

  Rational_Box known_box(1);
  known_box.raise_lower_bound(0, true, 3, 1);
  known_box.lower_upper_bound(0, true, 3, 1);

  bool ok = !/*FIX*/ (box == known_box);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
