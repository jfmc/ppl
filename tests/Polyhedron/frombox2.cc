/* Test NNC_Polyhedron(const Box&, From_Bounding_Box).
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

// An unbounded box in 4D but bounded in 2D with strict inequalities.
bool
test01() {
  Rational_Box box(4);
  box.raise_lower_bound(1, false, -2, 3);
  box.lower_upper_bound(1, true, 4, 1);
  box.raise_lower_bound(2, false, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);
  box.raise_lower_bound(3, true, 15, 3);

  NNC_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x > -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y > -10);
  known_ph.add_constraint(z >= 5);

  bool ok = (ph == known_ph);

  print_generators(ph, "*** ph ***");
  print_generators(known_ph, "*** known_ph ***");

  return ok;
}

// A bounded NNC polyhedron with strict inequalities
// causing upper and lower bounds of the box to be open.
bool
test02() {
  Rational_Box box(4);
  box.raise_lower_bound(1, true, -2, 3);
  box.lower_upper_bound(1, false, 4, 1);
  box.raise_lower_bound(2, false, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);

  NNC_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);

  NNC_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x < 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y > -10);

  bool ok = (ph == known_ph);

  print_generators(ph, "*** ph ***");
  print_generators(known_ph, "*** known_ph ***");

  return ok;
}

// An empty box in 2D.
bool
test03() {
  Rational_Box box(2);
  box.set_empty();

  NNC_Polyhedron ph(box, From_Bounding_Box());

  NNC_Polyhedron known_ph(2, EMPTY);

  bool ok = (ph == known_ph);

  print_constraints(ph, "*** ph ***");
  print_constraints(known_ph, "*** known_ph ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST_F8A(test02);
  DO_TEST(test03);
END_MAIN
